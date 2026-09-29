# Network and input validation are isolated from modules and calculations.
as_number <- function(x) {
  value <- suppressWarnings(as.numeric(gsub(",", "", as.character(x), fixed = TRUE)))
  value[!is.finite(value) | value < 0] <- NA_real_
  value
}

download_cdph <- function(config) {
  offset <- 0L
  pages <- list()
  repeat {
    request <- httr2::request(config$cdph_url)
    request <- httr2::req_url_query(request, resource_id = config$cdph_resource, limit = 5000L,
      offset = offset, filters = jsonlite::toJSON(list(wwtp_name = "Modesto_Cty",
        data_source = "CDC NWSS Commercial Contract (Verily)"), auto_unbox = TRUE))
    request <- httr2::req_timeout(request, 90)
    request <- httr2::req_retry(request, max_tries = 2L)
    body <- httr2::resp_body_json(httr2::req_perform(request), simplifyVector = TRUE)
    if (!isTRUE(body$success)) stop("CDPH returned an unsuccessful response.")
    records <- body$result$records
    if (!is.data.frame(records) || !nrow(records)) break
    pages[[length(pages) + 1L]] <- records
    offset <- offset + nrow(records)
    if (nrow(records) < 5000L || (!is.null(body$result$total) && offset >= body$result$total)) break
  }
  if (!length(pages)) stop("CDPH returned no Modesto observations.")
  records <- do.call(rbind, pages)
  required <- c("sample_collect_date", "pcr_target", "pcr_target_avg_conc", "hum_frac_mic_conc")
  if (!all(required %in% names(records))) stop("CDPH schema changed.")
  records$date <- as.Date(records$sample_collect_date)
  numerator <- as_number(records$pcr_target_avg_conc)
  denominator <- as_number(records$hum_frac_mic_conc)
  records$value <- numerator / denominator
  records$value[!is.finite(records$value)] <- NA_real_
  # Alternative assay labels map to one canonical column, never duplicate names.
  mapping <- c("sars-cov-2" = "SC2_N_norm_PMMoV", rsv = "RSV_norm_PMMoV",
    "fluav h1" = "InfA_H1_norm_PMMoV", "fluav h3" = "InfA_H3_norm_PMMoV",
    "fluav h5" = "InfA_H5_norm_PMMoV", fluav = "Influenza_A_norm_PMMoV",
    flubv = "Influenza_B_norm_PMMoV", "hmpxv clade i" = "MPXV_dD14.16_norm_PMMoV",
    nvo = "Noro_G2_norm_PMMoV", mev = "MeV_norm_PMMoV", mev_wt = "MeV_norm_PMMoV")
  records$pathogen <- unname(mapping[tolower(records$pcr_target)])
  records <- records[!is.na(records$date) & !is.na(records$pathogen), , drop = FALSE]
  if (!nrow(records)) stop("No recognized CDPH pathogen labels.")
  dates <- sort(unique(records$date))
  result <- data.frame(Collection_Date = dates, City = "Modesto",
    Site_Name = "Modesto's Sutter Primary Treatment Facility", Population_Served = 230000)
  for (pathogen in names(config$pathogens)) {
    x <- records[records$pathogen == pathogen, , drop = FALSE]
    daily <- tapply(x$value, as.character(x$date), finite_mean)
    result[[pathogen]] <- if (length(daily)) as.numeric(daily[as.character(dates)]) else rep(NA_real_, length(dates))
  }
  result
}

download_data <- function(config) {
  request <- httr2::req_timeout(httr2::request(config$scan_url), 120)
  request <- httr2::req_retry(request, max_tries = 2L)
  csv <- httr2::resp_body_string(httr2::req_perform(request))
  scan <- as.data.frame(readr::read_csv(I(csv), show_col_types = FALSE, progress = FALSE))
  aliases <- c("MPXV_dD14-16_norm_PMMoV" = "MPXV_dD14.16_norm_PMMoV",
    "EV-D68_norm_PMMoV" = "EV.D68_norm_PMMoV")
  for (old in names(aliases)) {
    if (old %in% names(scan) && !aliases[[old]] %in% names(scan)) names(scan)[names(scan) == old] <- aliases[[old]]
  }
  required <- c("Collection_Date", "City", "Site_Name", "Population_Served")
  if (!all(required %in% names(scan))) stop("WastewaterSCAN schema changed.")
  if (!all(names(config$pathogens) %in% names(scan))) {
    stop("WastewaterSCAN pathogen columns changed; refusing an incomplete live dataset.")
  }
  # Modesto uses CDPH only, avoiding duplicate samples from two providers.
  scan <- scan[scan$City %in% setdiff(config$cities, "Modesto"), c(required, names(config$pathogens)), drop = FALSE]
  modesto <- download_cdph(config)
  rbind(scan, modesto[, names(scan), drop = FALSE])
}

data_cache_path <- function(config) {
  cache_dir <- Sys.getenv("HCVT_CACHE_DIR", "")
  if (!nzchar(cache_dir)) cache_dir <- file.path(getwd(), "cache")
  file.path(cache_dir, "dashboard-data.rds")
}

read_data_cache <- function(config) {
  path <- data_cache_path(config)
  if (!file.exists(path)) return(NULL)
  cached <- tryCatch(readRDS(path), error = function(e) {
    message("Ignoring unreadable data cache: ", conditionMessage(e))
    NULL
  })
  if (!is.list(cached) || !is.data.frame(cached$data) || is.null(cached$updated_at)) return(NULL)
  updated_at <- tryCatch(as.POSIXct(cached$updated_at), error = function(e) as.POSIXct(NA))
  if (length(updated_at) != 1L || is.na(updated_at)) return(NULL)
  required <- c("Collection_Date", "City", "Site_Name", "Population_Served",
    names(config$pathogens), paste0(names(config$pathogens), "_raw"),
    paste0(names(config$pathogens), "_10"))
  if (!all(required %in% names(cached$data)) || !nrow(cached$data)) return(NULL)
  cached$updated_at <- updated_at
  cached
}

write_data_cache <- function(data, updated_at, config) {
  path <- data_cache_path(config)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temp_path <- tempfile(pattern = "dashboard-data-", tmpdir = dirname(path), fileext = ".rds")
  backup_path <- tempfile(pattern = "dashboard-data-backup-", tmpdir = dirname(path), fileext = ".rds")
  backup_needed <- FALSE
  on.exit({
    unlink(temp_path)
    if (!backup_needed) unlink(backup_path)
  }, add = TRUE)

  tryCatch({
    saveRDS(list(data = data, updated_at = as.POSIXct(updated_at), version = 1L),
      temp_path, compress = FALSE)
    if (file.exists(path)) {
      if (!file.rename(path, backup_path)) stop("Could not preserve the previous cache file.")
      backup_needed <- TRUE
    }
    if (!file.rename(temp_path, path)) {
      if (backup_needed) {
        restored <- file.rename(backup_path, path)
        backup_needed <- !restored
      }
      stop("Could not install the new cache file.")
    }
    if (backup_needed) unlink(backup_path)
    backup_needed <- FALSE
    invisible(TRUE)
  }, error = function(e) {
    message("Could not update local data cache: ", conditionMessage(e))
    invisible(FALSE)
  })
}

rolling_trimmed <- function(values, width) {
  result <- rep(NA_real_, length(values))
  if (length(values) >= width) {
    for (i in seq.int(width, length(values))) result[i] <- trim_fun(values[seq.int(i - width + 1L, i)])
  }
  result
}

prepare_data <- function(data, config) {
  required <- c("Collection_Date", "City", "Site_Name", "Population_Served")
  if (!all(required %in% names(data))) stop("Missing data columns: ", paste(setdiff(required, names(data)), collapse = ", "))
  data$Collection_Date <- as.Date(data$Collection_Date)
  data$Population_Served <- as_number(data$Population_Served)
  data <- data[!is.na(data$Collection_Date) & !is.na(data$Site_Name) &
    data$City %in% config$cities, , drop = FALSE]
  if (!nrow(data)) stop("No observations for configured cities.")
  # Snapshots include raw observations. Always rebuild derived columns from raw.
  for (pathogen in names(config$pathogens)) {
    raw <- paste0(pathogen, "_raw")
    if (raw %in% names(data)) {
      data[[raw]] <- as_number(data[[raw]])
    } else if (pathogen %in% names(data)) {
      data[[raw]] <- as_number(data[[pathogen]])
    } else {
      stop("Missing pathogen column: ", pathogen)
    }
  }
  output <- list()
  for (site in unique(data$Site_Name)) {
    x <- data[data$Site_Name == site, , drop = FALSE]
    cities <- unique(x$City)
    if (length(cities) != 1L) stop("Site belongs to multiple cities: ", site)
    if (cities == "Modesto") x <- x[x$Collection_Date >= as.Date("2024-10-01"), , drop = FALSE]
    if (!nrow(x)) next
    populations <- unique(x$Population_Served[is.finite(x$Population_Served)])
    if (length(populations) != 1L || populations <= 0) stop("Missing or inconsistent population: ", site)
    dates <- seq(min(x$Collection_Date), max(x$Collection_Date), by = "day")
    result <- data.frame(Collection_Date = dates, City = cities, Site_Name = site, Population_Served = populations)
    for (pathogen in names(config$pathogens)) {
      raw <- paste0(pathogen, "_raw")
      daily <- tapply(x[[raw]], as.character(x$Collection_Date), finite_mean)
      values <- as.numeric(daily[as.character(dates)])
      result[[raw]] <- values
      result[[pathogen]] <- replace_min(values)
      result[[paste0(pathogen, "_10")]] <- rolling_trimmed(result[[pathogen]], config$moving_days)
    }
    output[[site]] <- result
  }
  if (!length(output)) stop("No usable sites.")
  result <- do.call(rbind, output)
  rownames(result) <- NULL
  # Multiple sites in a city require an explicit aggregation policy.
  site_city <- unique(result[c("City", "Site_Name")])
  if (anyDuplicated(site_city$City)) stop("Multiple sites per city require configuration.")
  result[order(result$City, result$Collection_Date), , drop = FALSE]
}

load_app_data <- function(config, force_refresh = FALSE) {
  if (!config$data_mode %in% c("auto", "live", "snapshot")) stop("Invalid HCVT_DATA_MODE.")
  cached <- read_data_cache(config)
  if (!force_refresh && config$data_mode != "snapshot" && !is.null(cached) &&
      identical(as.Date(cached$updated_at, tz = Sys.timezone()), Sys.Date())) {
    return(list(data = cached$data, source = "cache", updated_at = cached$updated_at,
      refresh_error = NULL))
  }

  should_download <- force_refresh || config$data_mode != "snapshot"
  download_error <- NULL
  if (should_download) {
    raw_live <- tryCatch(download_data(config), error = function(e) {
      download_error <<- conditionMessage(e)
      NULL
    })
    live <- if (is.null(raw_live)) NULL else tryCatch(prepare_data(raw_live, config), error = function(e) {
      download_error <<- conditionMessage(e)
      NULL
    })
    if (!is.null(live)) {
      updated_at <- Sys.time()
      saved <- write_data_cache(live, updated_at, config)
      if (!saved) download_error <- "Download succeeded, but the cache could not be saved."
      return(list(data = live, source = "live", updated_at = updated_at,
        refresh_error = if (saved) NULL else download_error))
    }
    if (!is.null(cached)) {
      message("Live data unavailable; retaining the last successful cache: ", download_error)
      return(list(data = cached$data, source = "cache", updated_at = cached$updated_at,
        refresh_error = download_error))
    }
    if (config$data_mode == "live") {
      if (is.null(download_error) || !nzchar(download_error)) download_error <- "Live data could not be loaded."
      stop(download_error)
    }
  }
  if (!file.exists(config$snapshot)) stop("Snapshot does not exist: ", config$snapshot)
  snapshot <- utils::read.csv(config$snapshot, check.names = FALSE, stringsAsFactors = FALSE)
  list(data = prepare_data(snapshot, config), source = "snapshot", updated_at = NULL,
    refresh_error = download_error)
}

read_all_data <- function(config = app_config(), force_refresh = FALSE) {
  tryCatch({
    loaded <- load_app_data(config, force_refresh = force_refresh)
    model <- build_app_model(loaded$data, config)
    sample_dates <- loaded$data$Collection_Date
    scan_dates <- sample_dates[loaded$data$City != "Modesto"]
    modesto_dates <- sample_dates[loaded$data$City == "Modesto"]
    latest_date <- function(x) {
      x <- x[!is.na(x)]
      if (length(x)) max(x) else as.Date(NA)
    }
    list(data = loaded$data, model = model, source = loaded$source, error = NULL,
      updated_at = loaded$updated_at, refresh_error = loaded$refresh_error,
      i18n = translations,
      params = list(report_end = model$report_end,
        compiled_on = Sys.Date(),
        last_sample = latest_date(sample_dates),
        last_scan_sample = latest_date(scan_dates),
        last_modesto_sample = latest_date(modesto_dates)))
  }, error = function(e) {
    message("HCVT data initialization failed: ", conditionMessage(e))
    list(data = NULL, model = NULL, source = NULL, error = conditionMessage(e),
      updated_at = NULL, refresh_error = NULL, i18n = translations, params = NULL)
  })
}
