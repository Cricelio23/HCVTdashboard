# Local percentile-based Wastewater Level (WLevel), independent of Level.
# Uses the dashboard's existing smoothed series; this is not CDC WVAL.
# Causal configurable cap: each observation uses only its preceding calendar days,
# including itself. Nondetect substitution uses half the positive minimum,
# using only positive samples available at that date (not a measured LOD).
winsorize_observations <- function(values, dates, config) {
  result <- rep(NA_real_, length(values))
  observed <- which(!is.na(dates) & is.finite(values) & values >= 0)
  if (!length(observed)) return(result)
  observed_dates <- as.numeric(dates[observed])
  for (j in seq_along(observed)) {
    i <- observed[j]
    start <- observed_dates[j] - config$preprocessing$window_days + 1L
    history <- observed[observed_dates >= start & observed_dates <= observed_dates[j]]
    x <- values[history]
    positive <- x[x > 0]
    if (length(positive)) x[x == 0] <- min(positive) / 2
    cap <- as.numeric(stats::quantile(x, config$preprocessing$winsor_prob,
      names = FALSE, type = config$preprocessing$quantile_type))
    value <- values[i]
    if (value == 0 && length(positive)) value <- min(positive) / 2
    result[i] <- min(value, cap)
  }
  result
}
wlevel_group <- function(pathogen, config) {
  for (group in names(config$wlevel$pathogens)) {
    if (pathogen %in% config$wlevel$pathogens[[group]]) return(group)
  }
  if (pathogen %in% config$seasonal) "seasonal" else "nonseasonal"
}

wlevel_update_date <- function(as_of_date, group) {
  as_of_date <- as.Date(as_of_date)
  year <- as.integer(format(as_of_date, "%Y"))
  month <- as.integer(format(as_of_date, "%m"))
  if (group == "covid") {
    if (month < 4L) return(as.Date(sprintf("%d-10-01", year - 1L)))
    return(as.Date(sprintf("%d-%02d-01", year, if (month < 10L) 4L else 10L)))
  }
  if (group %in% c("influenza", "rsv")) {
    return(as.Date(sprintf("%d-08-01", year - as.integer(month < 8L))))
  }
  as_of_date
}

# Subtract calendar months, clamping dates such as February 29 to month end.
wlevel_months_before <- function(date, months) {
  index <- as.integer(format(date, "%Y")) * 12L + as.integer(format(date, "%m")) - 1L - months
  target <- as.Date(sprintf("%04d-%02d-01", index %/% 12L, index %% 12L + 1L))
  next_month <- as.Date(sprintf("%04d-%02d-01", (index + 1L) %/% 12L, (index + 1L) %% 12L + 1L))
  as.Date(pmin(as.numeric(target) + as.integer(format(date, "%d")) - 1L,
    as.numeric(next_month) - 1L), origin = "1970-01-01")
}

wlevel_reference <- function(series, pathogen, as_of_date, config) {
  missing_cuts <- rep(NA_real_, 4L)
  as_of_date <- as.Date(as_of_date)
  if (length(as_of_date) != 1L || is.na(as_of_date)) return(missing_cuts)
  observed <- !is.na(series$date) & series$date <= as_of_date &
    is.finite(series$raw) & series$raw >= 0
  if (!any(observed)) return(missing_cuts)
  group <- wlevel_group(pathogen, config)
  # Fixed schedules use the reporting date; other pathogens advance with samples.
  anchor <- if (group %in% c("covid", "influenza", "rsv")) as_of_date else max(series$date[observed])
  reference_end <- wlevel_update_date(anchor, group) - config$wlevel$exclude_days
  reference_start <- wlevel_months_before(reference_end, config$wlevel$months)
  historical <- observed & series$date >= reference_start & series$date <= reference_end &
    is.finite(series$smooth) & series$smooth >= 0
  values <- series$smooth[historical]
  if (length(values) < config$wlevel$min_history_samples) return(missing_cuts)
  stats::quantile(values, probs = config$wlevel$probs[[group]],
    names = FALSE, type = config$wlevel$quantile_type)
}

wlevel_classify <- function(value, cuts) {
  if (length(value) != 1L || !is.finite(value) || value < 0 ||
      length(cuts) != 4L || any(!is.finite(cuts)) || is.unsorted(cuts)) return(NA_character_)
  # Equality belongs to the lower category, including tied percentile cutoffs.
  c("very_low", "low", "moderate", "high", "very_high")[1L + sum(value > cuts)]
}

calculate_wlevel <- function(series, pathogen, end_date, config) {
  end_date <- as.Date(end_date)
  if (length(end_date) != 1L || is.na(end_date)) return(NA_character_)
  series <- series[!is.na(series$date) & series$date <= end_date, , drop = FALSE]
  series <- series[order(series$date), , drop = FALSE]
  recent <- series[series$date >= end_date - config$recent_days + 1L, , drop = FALSE]
  week <- recent[recent$date >= end_date - config$wlevel$recent_week_days + 1L, , drop = FALSE]
  if (!any(is.finite(week$raw) & week$raw >= 0)) return(NA_character_)
  enough <- sum(is.finite(recent$raw) & recent$raw > 0) >= config$min_positive &&
    any(is.finite(week$raw) & week$raw > 0)
  # Preserve the original detection gate; it is separate from percentile ranking.
  if (!enough) return("very_low")
  latest <- week$smooth[is.finite(week$smooth) & week$smooth >= 0]
  if (!length(latest)) return(NA_character_)
  wlevel_classify(utils::tail(latest, 1L), wlevel_reference(series, pathogen, end_date, config))
}

# Compatibility entry points use explicit configuration, never global variables.
percentiles <- function(data, city, virus, n_rem = NULL, config = app_config(), as_of_date = NULL) {
  pathogen <- sub("_10$", "", virus)
  if (is.null(as_of_date)) {
    dates <- data$Collection_Date[!is.na(data$City) & data$City == city & !is.na(data$Collection_Date)]
    if (!length(dates)) return(rep(NA_real_, 4L))
    as_of_date <- max(as.Date(dates))
  }
  if (!is.null(n_rem)) config$wlevel$exclude_days <- n_rem
  series <- metric_series(data, city, pathogen, as_of_date)
  wlevel_reference(series, pathogen, as_of_date, config)
}

activity_level <- function(data, city, virus, county = FALSE, config = app_config(), as_of_date = NULL) {
  pathogen <- sub("_10$", "", virus)
  if (is.null(as_of_date)) {
    dates <- data$Collection_Date[!is.na(data$City) & data$City == city & !is.na(data$Collection_Date)]
    if (!length(dates)) return(NA_character_)
    as_of_date <- max(as.Date(dates))
  }
  calculate_wlevel(metric_series(data, city, pathogen, as_of_date, county), pathogen, as_of_date, config)
}

class_level_county <- function(data, city_name, virus, county = FALSE, config = app_config(), as_of_date = NULL) {
  levels <- matrix(NA_character_, length(virus), length(city_name), dimnames = list(unname(virus), city_name))
  for (j in seq_along(city_name)) {
    for (i in seq_along(virus)) {
      levels[i, j] <- activity_level(data, city_name[j], unname(virus[i]), county, config, as_of_date)
    }
  }
  levels
}
