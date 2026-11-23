# Persistent local cache for wastewater trend and growth-probability series.
# Each site/pathogen pair is stored independently so one bad series cannot
# invalidate the rest of the cache.

ww_series_cache_dir <- function() {
  configured <- Sys.getenv("HCVT_WW_CACHE_DIR", "")
  if (nzchar(configured)) return(path.expand(configured))
  file.path(tools::R_user_dir("HCVTdashboard", which = "cache"), "ww_growth")
}

ww_series_cache_key <- function(city, pathogen) paste(city, pathogen, sep = "\r")

ww_series_cache_path <- function(site, pathogen, cache_dir = ww_series_cache_dir()) {
  slug <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)
  file.path(cache_dir, paste0(slug(site), "__", slug(pathogen), ".rds"))
}

ww_series_signature <- function(date, conc, delta, lod, prior, rule) {
  payload <- serialize(list(date = as.Date(date), conc = as.numeric(conc), delta = delta,
    lod = lod, prior = prior, rule = rule),
    connection = NULL, version = 2)
  tmp <- tempfile("ww-signature-")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(payload, tmp)
  unname(tools::md5sum(tmp))
}

ww_read_series_cache <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) {
    message("Ignoring unreadable wastewater series cache ", basename(path), ": ", conditionMessage(e))
    NULL
  })
}

ww_write_series_cache <- function(entry, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("ww-cache-", tmpdir = dirname(path), fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(entry, tmp, version = 3)
  if (!file.copy(tmp, path, overwrite = TRUE)) stop("Could not save wastewater series cache: ", path)
  invisible(path)
}

read_ww_series_cache <- function(data, config, cache_dir = ww_series_cache_dir()) {
  output <- list()
  sites <- unique(data[c("City", "Site_Name")])
  for (i in seq_len(nrow(sites))) {
    city <- as.character(sites$City[i])
    site <- as.character(sites$Site_Name[i])
    for (pathogen in names(config$pathogens)) {
      entry <- ww_read_series_cache(ww_series_cache_path(site, pathogen, cache_dir))
      if (is.list(entry) && identical(entry$schema, 1L)) {
        output[[ww_series_cache_key(city, pathogen)]] <- entry
      }
    }
  }
  output
}

ww_series_lod <- function(data, city, pathogen, date = Sys.Date(), conc = NULL) {
  samples <- attr(data, "wval_samples")
  if (!is.data.frame(samples) || !all(c("date", "city", "pathogen", "lod") %in% names(samples))) return(NULL)
  x <- samples[!is.na(samples$date) & samples$city == city & samples$pathogen == pathogen &
    is.finite(samples$lod) & samples$lod > 0, , drop = FALSE]
  if (nrow(x)) return(x$lod[which.max(x$date)])

  # WastewaterSCAN does not publish a measured LOD. Use the lowest positive
  # concentration observed in the latest two-year window for this series.
  if (is.null(conc)) return(NULL)
  date <- as.Date(date)
  conc <- as.numeric(conc)
  valid_dates <- date[!is.na(date)]
  if (!length(valid_dates)) return(NULL)
  reference_date <- max(valid_dates)
  window <- !is.na(date) & date >= reference_date - 730L & is.finite(conc) & conc > 0
  if (!any(window)) return(NULL)
  min(conc[window])
}

update_ww_series_cache <- function(data, config, city = NULL, pathogen = NULL,
    cache_dir = ww_series_cache_dir()) {
  if (!exists("ww_realtime_update", mode = "function") || !exists("ww_trend", mode = "function")) {
    stop("Source R/dashboard_export/ww_trend.R before updating wastewater series.")
  }
  deltas <- config$city_pathogen$growth_delta
  rule <- config$city_pathogen$growth_rule
  output <- list()
  sites <- unique(data[c("City", "Site_Name")])
  if (!is.null(city)) sites <- sites[sites$City == city, , drop = FALSE]
  for (i in seq_len(nrow(sites))) {
    city <- as.character(sites$City[i])
    site <- as.character(sites$Site_Name[i])
    site_data <- data[data$Site_Name == site, , drop = FALSE]
    site_data <- site_data[order(site_data$Collection_Date), , drop = FALSE]
    selected_pathogens <- if (is.null(pathogen)) names(config$pathogens) else intersect(pathogen, names(config$pathogens))
    for (pathogen in selected_pathogens) {
      raw_column <- paste0(pathogen, "_raw")
      if (!raw_column %in% names(site_data)) next
      date <- as.Date(site_data$Collection_Date)
      conc <- suppressWarnings(as.numeric(site_data[[raw_column]]))
      conc[!is.finite(conc) | conc < 0] <- NA_real_
      if (!any(is.finite(conc))) {
        old <- ww_read_series_cache(ww_series_cache_path(site, pathogen, cache_dir))
        if (is.list(old)) output[[ww_series_cache_key(city, pathogen)]] <- old
        next
      }
      delta <- unname(deltas[pathogen])
      if (!length(delta) || is.na(delta)) delta <- NULL
      path <- ww_series_cache_path(site, pathogen, cache_dir)
      old <- ww_read_series_cache(path)
      key <- ww_series_cache_key(city, pathogen)
      lod <- ww_series_lod(data, city, pathogen, date = date, conc = conc)
      first_date <- min(date[is.finite(conc)])
      prior_selected <- is.list(old) && isTRUE(old$prior_selected)
      prior <- if (prior_selected) old$prior_saved else
        if (as.integer(max(date[is.finite(conc)]) - first_date) < 730L) WW_PRIOR else NULL
      signature <- ww_series_signature(date, conc, delta, lod, prior, rule)
      if (is.list(old) && identical(old$schema, 1L) && identical(old$signature, signature)) {
        if (!is.null(delta)) old$status <- "no_new_data"
        output[[key]] <- old
        next
      }
      entry <- list(schema = 1L, city = city, site = site, pathogen = pathogen,
        delta = delta, rule = rule, prior_selected = TRUE, prior_saved = prior,
        signature = signature, lod = lod,
        updated_at = Sys.time(), status = "unavailable", smooth = NULL, published = NULL)
      if (is.list(old)) {
        entry$published <- old$published
        entry$smooth <- old$smooth
        entry$params <- old$params
        entry$status <- old$status
      }

      result <- tryCatch({
        if (!is.null(delta)) {
          ww_realtime_update(published = entry$published, date = date, conc = conc,
            lod = lod, delta = delta, rule = rule, prior = prior)
        } else {
          curve <- ww_trend(date = date, conc = conc, lod = lod, rule = rule, prior = prior)
          curve$provisional <- curve$date > max(curve$date) - 21L
          list(published = NULL, smooth = curve, status = attr(curve, "status"))
        }
      }, error = function(e) {
        message("Wastewater trend unavailable for ", city, " / ", pathogen, ": ", conditionMessage(e))
        NULL
      })
      if (!is.null(result)) {
        entry$smooth <- result$smooth
        entry$published <- result$published
        entry$status <- result$status
        params <- result$params
        if (is.null(params)) params <- attr(result$smooth, "params")
        entry$params <- params
        entry$lod <- if (!is.null(params$lod)) params$lod else lod
      }
      if (!is.null(result)) {
        tryCatch(ww_write_series_cache(entry, path), error = function(e) {
          message("Could not persist wastewater trend for ", city, " / ", pathogen, ": ", conditionMessage(e))
        })
      }
      output[[key]] <- entry
    }
  }
  output
}
