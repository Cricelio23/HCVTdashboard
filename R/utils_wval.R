# WVAL is calculated from unnormalized sample concentrations, never from _10.
# References: https://www.cdc.gov/wastewater/about/wval.html (August 2026)
# https://github.com/CDCgov/NWSS/tree/master/jurisdiction-scripts/WVAL%20Collaborative%20Script
# Local implementation: exp((log concentration - log P10 baseline) / log SD),
# followed by the arithmetic weekly mean. Use 2026 winsorization and cutoffs.
# The dashboard assumes one continuous method per site. Zero concentrations use
# half the smallest positive value in the reference window; NA remains missing.
empty_wval_samples <- function() {
  data.frame(date = as.Date(character()), city = character(), site = character(),
    pathogen = character(), concentration = numeric(), lod = numeric(), lab = character(),
    method = character(), gene = character(), units = character(), matrix = character(),
    source = character(), qc_ok = logical(), stringsAsFactors = FALSE)
}

cdph_wval_samples <- function(records, config) {
  x <- records[records$pathogen %in% names(config$wval$pathogens), , drop = FALSE]
  if (!nrow(x)) return(empty_wval_samples())
  field <- function(name) if (name %in% names(x)) as.character(x[[name]]) else rep(NA_character_, nrow(x))
  qc_ok <- rep(TRUE, nrow(x))
  for (name in c("analysis_ignore", "dashboard_ignore", "qc_ignore")) {
    qc_ok <- qc_ok & !(tolower(trimws(field(name))) %in% c("yes", "true", "1"))
  }
  data.frame(date = as.Date(x$date), city = "Modesto", site = "Modesto's Sutter Primary Treatment Facility",
    pathogen = x$pathogen, concentration = as_number(field("pcr_target_avg_conc")),
    lod = as_number(field("lod_sewage")), lab = field("lab_id"), method = field("major_lab_method"),
    gene = field("pcr_gene_target"), units = field("pcr_target_units"), matrix = field("sample_matrix"),
    source = field("data_source"), qc_ok = qc_ok, stringsAsFactors = FALSE)
}

scan_wval_samples <- function(scan, config) {
  output <- list()
  for (pathogen in names(config$wval$pathogens)) {
    column <- sub("_norm_PMMoV$", "_gc_g_dry_weight", pathogen)
    if (!column %in% names(scan)) next
    concentration <- as_number(scan[[column]])
    keep <- is.finite(concentration)
    if (!any(keep)) next
    output[[pathogen]] <- data.frame(date = as.Date(scan$Collection_Date[keep]),
      city = scan$City[keep], site = scan$Site_Name[keep], pathogen = pathogen,
      concentration = concentration[keep], lod = NA_real_, lab = "assumed_same_lab",
      method = "assumed_same_method", gene = sub("_norm_PMMoV$", "", pathogen),
      units = "gc/g dry weight", matrix = "solids", source = "WastewaterSCAN", qc_ok = TRUE,
      stringsAsFactors = FALSE)
  }
  if (length(output)) do.call(rbind, output) else empty_wval_samples()
}

wval_week_end <- function(date) {
  date <- as.Date(date)
  date + 6L - as.integer(format(date, "%w"))
}

wval_result <- function(status, value = NA_real_, category = NA_character_, week_end = as.Date(NA),
    unit = NA_character_) {
  list(wval = category, wval_value = value, wval_status = status, wval_week_end = week_end,
    wval_unit = unit)
}

calculate_site_wval <- function(samples, city, pathogen, end_date, config) {
  group <- unname(config$wval$pathogens[pathogen])
  if (is.na(group)) return(wval_result("wval_not_applicable"))
  completed_week <- as.Date(end_date) - ((as.integer(format(as.Date(end_date), "%w")) + 1L) %% 7L)
  week_end <- completed_week
  fail <- function(status) wval_result(status, week_end = week_end)
  if (!is.data.frame(samples) || !nrow(samples)) return(fail("wval_missing_inputs"))
  required <- names(empty_wval_samples())
  if (!all(required %in% names(samples))) return(fail("wval_missing_inputs"))
  x <- samples[!is.na(samples$date) & samples$date <= completed_week &
    !is.na(samples$city) & samples$city == city & samples$pathogen %in% pathogen, , drop = FALSE]
  if (!nrow(x)) return(fail("wval_missing_inputs"))
  x <- unique(x)
  available_weeks <- wval_week_end(x$date)
  available_weeks <- available_weeks[available_weeks <= completed_week]
  if (isTRUE(config$wval$latest_available_week) && length(available_weeks)) week_end <- max(available_weeks)
  current <- x$date >= week_end - 6L
  if (!any(current)) return(fail("wval_no_week"))
  metadata <- c("site", "gene", "units", "matrix", "source")
  if (!isTRUE(config$wval$assume_same_method)) metadata <- c(metadata, "lab", "method")
  complete <- Reduce(`&`, lapply(x[metadata], function(v) !is.na(v) & nzchar(trimws(v))))
  possible_reference_start <- wlevel_months_before(wlevel_update_date(week_end, group), config$wval$months)
  if (any(!complete[current | x$date >= possible_reference_start])) return(fail("wval_missing_metadata"))
  x <- x[complete, , drop = FALSE]
  # Do not pool incompatible assays or methods within the reporting week.
  keys <- do.call(paste, c(x[metadata], sep = "\r"))
  current_keys <- unique(keys[x$date >= week_end - 6L])
  if (length(current_keys) != 1L) return(fail("wval_mixed_methods"))
  x <- x[keys == current_keys, , drop = FALSE]
  x <- x[order(x$date), , drop = FALSE]
  # A long interruption restarts eligibility for this site/method combination.
  gaps <- which(diff(as.numeric(x$date)) > 365L)
  if (length(gaps)) x <- x[seq.int(max(gaps) + 1L, nrow(x)), , drop = FALSE]
  if (any(is.na(x$qc_ok[x$date >= week_end - 6L]) | !x$qc_ok[x$date >= week_end - 6L])) {
    return(fail("wval_quality"))
  }
  x <- x[!is.na(x$qc_ok) & x$qc_ok, , drop = FALSE]
  if (!nrow(x)) return(fail("wval_quality"))
  age <- as.integer(week_end - min(x$date))
  if (age < config$wval$min_days || length(unique(wval_week_end(x$date))) < config$wval$min_weeks) {
    return(fail("wval_short_history"))
  }
  if (group != "covid") {
    year <- as.integer(format(week_end, "%Y"))
    season_start <- as.Date(sprintf("%d-10-01", year - as.integer(format(week_end, "%m") < "08")))
    if (min(x$date) > season_start) return(fail("wval_season_ineligible"))
  }
  # Under the configured same-method assumption, use the site's own data from
  # the first eligible eight-week period instead of a laboratory-wide fallback.
  reference_end <- if (age < config$wval$established_days[[group]]) week_end else
    max(wlevel_update_date(week_end, group), min(x$date) + config$wval$established_days[[group]])
  reference_start <- wlevel_months_before(reference_end, config$wval$months)
  relevant <- (x$date >= reference_start & x$date < reference_end) | x$date >= week_end - 6L
  x <- x[relevant, , drop = FALSE]
  if (any(!is.finite(x$concentration) | x$concentration < 0)) return(fail("wval_missing_inputs"))
  historical <- x$date >= reference_start & x$date < reference_end
  positive_reference <- x$concentration[historical & x$concentration > 0]
  if (!length(positive_reference)) return(fail("wval_no_positive_reference"))
  zero_replacement <- min(positive_reference) / 2
  concentration <- x$concentration
  concentration[concentration == 0] <- zero_replacement
  logs <- log(concentration)
  if (sum(historical) < 2L || length(unique(wval_week_end(x$date[historical]))) < config$wval$min_weeks) {
    return(fail("wval_short_history"))
  }
  cap <- as.numeric(stats::quantile(logs[historical], config$wval$winsor_prob,
    type = config$wval$quantile_type, names = FALSE))
  winsor <- pmin(logs, cap)
  baseline <- as.numeric(stats::quantile(winsor[historical], config$wval$baseline_prob,
    type = config$wval$quantile_type, names = FALSE))
  deviation <- stats::sd(winsor[historical])
  if (!is.finite(deviation) || deviation <= 0) return(fail("wval_zero_sd"))
  values <- exp((winsor[x$date >= week_end - 6L] - baseline) / deviation)
  if (!length(values) || any(!is.finite(values))) return(fail("wval_missing_inputs"))
  value <- mean(values)
  unit <- unique(x$units[x$date >= week_end - 6L])
  unit <- if (length(unit) == 1L) unit else NA_character_
  result <- wval_result("wval_local", value, wlevel_classify(value, config$wval$cuts[[group]]),
    week_end, unit)
  result$baseline <- baseline
  result$standard_deviation <- deviation
  result$reference_end <- reference_end
  result
}

add_wval_metrics <- function(metrics, samples, end_date, config, city_metrics = NULL) {
  metrics$wval <- rep(NA_character_, nrow(metrics))
  metrics$wval_value <- rep(NA_real_, nrow(metrics))
  metrics$wval_status <- rep(NA_character_, nrow(metrics))
  metrics$wval_week_end <- rep(as.Date(NA), nrow(metrics))
  metrics$wval_unit <- rep(NA_character_, nrow(metrics))
  for (i in seq_len(nrow(metrics))) {
    pathogen <- metrics$pathogen[i]
    if (is.null(city_metrics)) {
      result <- calculate_site_wval(samples, metrics$location[i], pathogen, end_date, config)
    } else {
      cities <- config$counties[[metrics$location[i]]]
      x <- city_metrics[city_metrics$pathogen == pathogen, , drop = FALSE]
      x <- x[match(cities, x$location), , drop = FALSE]
      group <- unname(config$wval$pathogens[pathogen])
      result <- wval_result(if (is.na(group)) "wval_not_applicable" else "wval_county_incomplete",
        week_end = as.Date(end_date) - ((as.integer(format(as.Date(end_date), "%w")) + 1L) %% 7L))
      # County median is a local extension; require complete, contemporaneous coverage.
      if (!is.na(group) && nrow(x) && all(is.finite(x$wval_value)) &&
          !anyNA(x$wval_week_end) && length(unique(x$wval_week_end)) == 1L &&
          !anyNA(x$wval_unit) && length(unique(x$wval_unit)) == 1L) {
        value <- stats::median(x$wval_value)
        result <- wval_result("wval_county_local", value,
          wlevel_classify(value, config$wval$cuts[[group]]), x$wval_week_end[1], x$wval_unit[1])
      }
    }
    for (key in c("wval", "wval_value", "wval_status", "wval_week_end", "wval_unit")) {
      metrics[i, key] <- result[[key]]
    }
  }
  metrics
}
