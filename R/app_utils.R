# Text belongs to the presentation layer, not to numerical calculations.
method_text <- function(lang, config = app_config()) {
  if (is.null(config)) config <- app_config()
  es <- identical(lang, "es")
  nonseasonal <- tr("pbwl_level_note", lang)
  seasonal <- tr("pbwl_detection_note", lang)
  gate <- tr("pbwl_gate", lang)
  county <- tr("pbwl_county_note", lang)
  pc <- tr("method_pc", lang)
  trend <- sprintf(tr("method_trend", lang), as.integer(config$trend_days))
  data_text <- tr("method_data", lang)
  list(pc = pc, level = c(nonseasonal, seasonal, gate), trend = trend,
    criteria = c(gate, county, tr("comparison_note", lang)), no_sea_lab = nonseasonal,
    sea_lab = seasonal, pc_cri = gate,
    na_14 = tr("method_na_14", lang),
    city_des_1 = tr("preprocessing_reference", lang), county_des_1 = county, data = data_text,
    equation = "$$\\bar{x}_{t}=\\frac{\\sum_i x_{i,t}\\,pop_i}{\\sum_i pop_i}$$",
    rsi = tr("method_rsi", lang), slope_pc = tr("method_slope_pc", lang))
}
wlevel_method_text <- function(lang, config) {
  c(
    tr("pbwl_overview", lang), tr("pbwl_current_value", lang),
    tr("pbwl_reference", lang), tr("pbwl_threshold_rationale", lang),
    tr("pbwl_sufficiency", lang), tr("pbwl_detection", lang),
    tr("pbwl_county", lang), tr("pbwl_inspiration", lang)
  )
}

pbwl_method_ui <- function(lang, config) {
  paragraphs <- wlevel_method_text(lang, config)
  cuts <- vapply(config$wlevel$probs, function(probs) paste0("P", 100 * probs, collapse = "/"), character(1))
  threshold_rows <- list(
    c(tr("pbwl_pathogen_covid", lang), cuts[["covid"]]),
    c(tr("pbwl_pathogen_influenza", lang), cuts[["influenza"]]),
    c(tr("pbwl_pathogen_rsv", lang), cuts[["rsv"]]),
    c(tr("pbwl_pathogen_seasonal", lang), cuts[["seasonal"]]),
    c(tr("pbwl_pathogen_nonseasonal", lang), cuts[["nonseasonal"]]),
    c(tr("pbwl_pathogen_ndm", lang), cuts[["ndm"]])
  )
  threshold_table <- shiny::tags$table(class = "table table-sm table-bordered pbwl-thresholds",
    shiny::tags$thead(shiny::tags$tr(
      shiny::tags$th(scope = "col", tr("pbwl_pathogen_group", lang)),
      shiny::tags$th(scope = "col", tr("pbwl_percentile_thresholds", lang)))),
    shiny::tags$tbody(lapply(threshold_rows, function(row) shiny::tags$tr(
      shiny::tags$td(row[[1]]), shiny::tags$td(row[[2]])))))
  shiny::tagList(
    shiny::tags$p(shiny::tags$strong(tr("pbwl_overview_heading", lang)), " ", paragraphs[[1]]),
    shiny::tags$p(shiny::tags$strong(tr("pbwl_processing_heading", lang))),
    shiny::tags$p(tr("pbwl_processing_intro", lang)),
    shiny::tags$p(shiny::tags$strong(tr("pbwl_winsorization_label", lang)), " ",
      sprintf(tr("pbwl_winsorization", lang), 100 * config$preprocessing$winsor_prob,
        config$preprocessing$window_days, config$preprocessing$min_reference_samples)),
    shiny::tags$p(shiny::tags$strong(sprintf(tr("pbwl_geometric_mean_label", lang), config$moving_days)), " ",
      sprintf(tr("pbwl_geometric_mean", lang), config$moving_days,
        config$preprocessing$min_window_samples)),
    shiny::tags$p(shiny::tags$strong(tr("pbwl_current_heading", lang)), " ", paragraphs[[2]]),
    shiny::tags$p(shiny::tags$strong(tr("pbwl_reference_heading", lang)), " ", paragraphs[[3]]),
    shiny::tags$ul(
      shiny::tags$li(shiny::tags$strong(tr("pbwl_reference_covid_label", lang)), " ",
        tr("pbwl_reference_covid", lang)),
      shiny::tags$li(shiny::tags$strong(tr("pbwl_reference_influenza_rsv_label", lang)), " ",
        tr("pbwl_reference_influenza_rsv", lang)),
      shiny::tags$li(shiny::tags$strong(tr("pbwl_reference_other_label", lang)), " ",
        tr("pbwl_reference_other", lang))),
    shiny::tags$p(tr("pbwl_reference_update_note", lang)),
    shiny::tags$p(tr("pbwl_reference_basis", lang), " ",
      shiny::tags$a(href = "https://www.cdc.gov/wastewater/about/wval.html",
        target = "_blank", rel = "noopener noreferrer", tr("pbwl_reference_basis_link", lang))),
    shiny::tags$p(shiny::tags$strong(tr("pbwl_threshold_heading", lang)), " ", paragraphs[[4]]),
    threshold_table,
    shiny::tags$p(tr("pbwl_threshold_equality", lang)),
    shiny::tags$p(shiny::tags$strong(tr("pbwl_sufficiency_heading", lang)), " ", paragraphs[[5]]),
    shiny::tags$p(shiny::tags$strong(tr("pbwl_detection_heading", lang)), " ", paragraphs[[6]]),
    shiny::tags$p(shiny::tags$strong(tr("pbwl_county_heading", lang)), " ", paragraphs[[7]]),
    shiny::tags$p(shiny::tags$strong(tr("pbwl_inspiration_heading", lang)), " ", paragraphs[[8]], " ",
      shiny::tags$a(href = "https://www.cdc.gov/nhsn/psc/hospital-respiratory-reporting/data-methods.html",
        target = "_blank", rel = "noopener noreferrer", tr("pbwl_cdc_link", lang)))
  )
}

table_legend_ui <- function(lang, config = app_config()) {
  text <- method_text(lang, config)
  shiny::tags$details(class = "table-legend",
    shiny::tags$summary(
      shiny::tags$span(tr("legend_title", lang)),
      shiny::tags$span(class = "table-legend-chevron", `aria-hidden` = "true", "▾")),
    shiny::div(class = "table-legend-content",
      shiny::tags$h4(tr("legend_levels", lang)),
      shiny::tags$p(text$no_sea_lab),
      shiny::tags$p(text$sea_lab),
      shiny::tags$h4(tr("info_wlevel", lang)),
      pbwl_method_ui(lang, config),
      shiny::tags$h4(tr("info_criteria", lang)),
      shiny::tags$p(text$pc_cri),
      shiny::tags$p(text$na_14),
      shiny::tags$p(text$city_des_1),
      shiny::tags$p(text$county_des_1)))
}


# ---- www/functions.R ----
# Pure calculations: all dates and settings are explicit arguments.
finite_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

reference_week_number <- function(date = Sys.Date(), reference_date = as.Date("2023-05-07"),
    reference_week = 80L) {
  date <- as.Date(date)
  if (length(date) != 1L || is.na(date)) return(NA_integer_)
  as.integer(floor(reference_week + as.numeric(date - reference_date) / 7))
}

report_period_lines <- function(report_end, lang, today = Sys.Date()) {
  report_end <- as.Date(report_end)
  last_week <- reference_week_number(today)
  if (length(report_end) != 1L || is.na(report_end) || is.na(last_week)) return(character())
  week_ranges <- matrix(c(last_week - 3L, last_week - 2L,
    last_week - 1L, last_week), ncol = 2L, byrow = TRUE)
  date_ranges <- matrix(c(report_end - 27L, report_end - 14L,
    report_end - 13L, report_end), ncol = 2L, byrow = TRUE)
  sprintf("%s %d-%d: %s - %s", tr("weeks", lang), week_ranges[, 1L], week_ranges[, 2L],
    format(as.Date(date_ranges[, 1L], origin = "1970-01-01"), "%m/%d"),
    format(as.Date(date_ranges[, 2L], origin = "1970-01-01"), "%m/%d"))
}

report_period_ui <- function(report_end, lang, today = Sys.Date()) {
  lines <- report_period_lines(report_end, lang, today)
  shiny::tagList(lapply(lines, shiny::tags$div))
}

trim_fun <- function(x) {
  x <- sort(x[is.finite(x)])
  if (length(x) > 2L) x <- x[-c(1L, length(x))]
  finite_mean(x)
}

replace_min <- function(x) {
  if (sum(x > 0, na.rm = TRUE) > 10L) {
    x[which(x == 0)] <- min(x[x > 0 & is.finite(x)]) / 2
  }
  x
}

reporting_end <- function(data, today = Sys.Date()) {
  dates <- data$Collection_Date[!is.na(data$Collection_Date) & data$Collection_Date <= today]
  if (!length(dates)) stop("No valid sample dates.")
  last_saturday <- as.Date(today) - (as.integer(format(as.Date(today), "%w")) + 1L)
  min(max(dates), last_saturday)
}

metric_series <- function(data, location, pathogen, end_date, county = FALSE) {
  rows <- !is.na(data$City) & data$City == location & !is.na(data$Collection_Date) &
    data$Collection_Date <= end_date
  x <- data[rows, , drop = FALSE]
  x <- x[order(x$Collection_Date), , drop = FALSE]
  get_column <- function(name) {
    if (!name %in% names(x)) return(rep(NA_real_, nrow(x)))
    value <- suppressWarnings(as.numeric(x[[name]]))
    value[!is.finite(value) | value < 0] <- NA_real_
    value
  }
  data.frame(date = x$Collection_Date, raw = get_column(paste0(pathogen, "_raw")),
    value = get_column(pathogen),
    smooth = get_column(paste0(pathogen, "_10")))
}

classify_mk <- function(tau, p) {
  if (!is.finite(tau) || !is.finite(p)) return(NA_character_)
  if (tau == 0 || p > 0.1) return("no_trend")
  direction <- if (tau > 0) "upward" else "downward"
  if (p <= 0.005) paste0("very_likely_", direction) else if (p <= 0.05) paste0("likely_", direction) else direction
}

mk_test <- function(x) {
  fit <- Kendall::MannKendall(x)
  classify_mk(as.numeric(fit$tau), as.numeric(fit$sl))
}

level_cutoffs <- function(series, pathogen, end_date, config) {
  stats::setNames(wlevel_reference(series, pathogen, end_date, config)[2:3],
    c("low_medium", "medium_high"))
}

calculate_metric <- function(series, pathogen, end_date, config, trend_test = mk_test,
    pc_end_date = end_date) {
  result <- list(pc = NA_real_, pc_status = NA_character_, level = NA_character_, trend = NA_character_)
  result$wlevel <- calculate_wlevel(series, pathogen, end_date, config)
  level_groups <- c(very_low = "low", low = "low", moderate = "medium", high = "high",
    very_high = "high", detected = "detected", not_detected = "not_detected",
    insufficient_data = "insufficient_data")
  result$level <- unname(level_groups[result$wlevel])

  pc_recent <- series[series$date >= pc_end_date - 13L & series$date <= pc_end_date, , drop = FALSE]
  pc_week <- pc_recent[pc_recent$date >= pc_end_date - 6L, , drop = FALSE]
  pc_positive <- sum(pc_recent$raw > 0, na.rm = TRUE)
  pc_has_week <- any(is.finite(pc_week$raw))
  pc_enough <- pc_positive >= config$min_positive
  if (pc_enough) {
    pc_prior <- series[series$date >= pc_end_date - 27L & series$date < pc_end_date - 13L, , drop = FALSE]
    current_mean <- finite_mean(pc_recent$value)
    prior_mean <- finite_mean(pc_prior$value)
    if (is.finite(current_mean)) {
      if (!is.finite(prior_mean) || prior_mean == 0) {
        result$pc_status <- "low_baseline"
      } else {
        result$pc <- round(100 * (current_mean - prior_mean) / prior_mean, 1)
        result$pc_status <- "numeric"
      }
    }
  } else if (pc_has_week) {
    result$pc_status <- if (pc_positive) "sporadic" else "nd"
  }

  recent <- series[series$date >= end_date - 13L & series$date <= end_date, , drop = FALSE]
  if (!any(is.finite(recent$raw))) return(result)
  week <- recent[recent$date >= end_date - 6L, , drop = FALSE]
  n_positive <- sum(recent$raw > 0, na.rm = TRUE)
  has_week <- any(is.finite(week$raw))
  enough <- n_positive >= config$min_positive

  if (enough) {
    values <- series$smooth[series$date >= end_date - config$trend_days + 1L]
    values <- values[is.finite(values)]
    if (length(values) >= 3L) {
      result$trend <- if (length(unique(values)) == 1L) "no_trend" else trend_test(values)
    }
  } else if (has_week) result$trend <- if (n_positive) "sporadic" else "no_trend"
  result
}

calculate_metrics <- function(data, locations, config, end_date, county = FALSE, trend_test = mk_test,
    pc_end_date = end_date) {
  grid <- expand.grid(pathogen = names(config$pathogens), location = locations, stringsAsFactors = FALSE)
  grid$pc <- rep(NA_real_, nrow(grid))
  grid$pc_status <- grid$level <- grid$trend <- grid$wlevel <- rep(NA_character_, nrow(grid))
  grid$last_sample_date <- rep(as.Date(NA), nrow(grid))
  for (i in seq_len(nrow(grid))) {
    series <- metric_series(data, grid$location[i], grid$pathogen[i], end_date, county)
    observed_dates <- series$date[is.finite(series$raw)]
    if (length(observed_dates)) grid$last_sample_date[i] <- max(observed_dates)
    value <- calculate_metric(series, grid$pathogen[i], end_date, config, trend_test,
      pc_end_date = pc_end_date)
    for (key in names(value)) grid[i, key] <- value[[key]]
  }
  grid
}

aggregate_counties <- function(data, config) {
  output <- list()
  for (county in names(config$counties)) {
    x <- data[data$City %in% config$counties[[county]], , drop = FALSE]
    if (!nrow(x)) next
    dates <- seq(min(x$Collection_Date), max(x$Collection_Date), by = "day")
    sites <- unique(x$Site_Name)
    populations <- vapply(sites, function(site) {
      p <- unique(x$Population_Served[x$Site_Name == site & is.finite(x$Population_Served)])
      if (length(p) != 1L || p <= 0) stop("Missing or inconsistent site population: ", site)
      p
    }, numeric(1))
    result <- data.frame(City = county, Collection_Date = dates)
    for (pathogen in names(config$pathogens)) {
      for (suffix in c("", "_raw", "_10")) {
        column <- paste0(pathogen, suffix)
        values <- matrix(NA_real_, nrow = length(dates), ncol = length(sites))
        for (j in seq_along(sites)) {
          site <- x[x$Site_Name == sites[j], , drop = FALSE]
          values[, j] <- site[[column]][match(dates, site$Collection_Date)]
        }
        weighted <- sweep(values, 2L, populations, "*")
        aggregate <- rowSums(weighted, na.rm = TRUE) / sum(populations)
        aggregate[rowSums(is.finite(values)) == 0L] <- NA_real_
        result[[column]] <- aggregate
      }
    }
    output[[county]] <- result
  }
  if (!length(output)) return(data.frame(City = character(), Collection_Date = as.Date(character())))
  do.call(rbind, output)
}

apply_county_rules <- function(county_metrics, city_metrics, config) {
  for (i in seq_len(nrow(county_metrics))) {
    row <- county_metrics[i, ]
    cities <- config$counties[[row$location]]
    city_rows <- city_metrics[city_metrics$pathogen == row$pathogen, , drop = FALSE]
    city_rows <- city_rows[match(cities, city_rows$location), , drop = FALSE]
    if (all(is.na(city_rows$pc_status))) {
      county_metrics$pc[i] <- NA_real_
      county_metrics$pc_status[i] <- NA_character_
    }
  }
  county_metrics
}

# Shared table rendering preserves pathogen/location identities, even with one row.
metric_table_data <- function(metrics, selected, locations, kinds, config, lang) {
  selected <- names(config$pathogens)[names(config$pathogens) %in% selected]
  result <- data.frame(Pathogen = unname(pathogen_labels(config, lang)[selected]), stringsAsFactors = FALSE)
  for (j in seq_along(locations)) {
    x <- metrics[metrics$location == locations[j], , drop = FALSE]
    x <- x[match(selected, x$pathogen), , drop = FALSE]
    for (kind in kinds) {
      result[[paste0("loc", j, "_", kind)]] <- if (kind == "pc") {
        format_pc(x$pc, x$pc_status, lang)
      } else if (kind == "wval") {
        format_wval(x$wval_value, x$wval, x$wval_status, x$wval_week_end, x$wval_unit, lang)
      } else metric_label(x[[kind]], lang)
    }
  }
  result
}

build_metric_table <- function(metrics, selected, locations, kinds, config, lang, title,
    show_latest_sample = FALSE) {
  selected <- names(config$pathogens)[names(config$pathogens) %in% selected]
  data <- metric_table_data(metrics, selected, locations, kinds, config, lang)
  table <- gt::gt(data)
  table <- gt::cols_label(table, Pathogen = tr("pathogen", lang))
  for (j in seq_along(locations)) {
    columns <- paste0("loc", j, "_", kinds)
    labels <- as.list(stats::setNames(tr(kinds, lang), columns))
    table <- gt::cols_label(table, .list = labels)
    table <- gt::tab_spanner(table, label = locations[j], columns = columns)
    table <- gt::tab_style(table,
      style = gt::cell_borders(sides = "left", color = "#94a3b8", weight = gt::px(2)),
      locations = list(gt::cells_body(columns = columns[1L]),
        gt::cells_column_labels(columns = columns[1L]),
        gt::cells_column_spanners(spanners = locations[j])))
  }
  table <- gt::tab_header(table, title = tr(title, lang))
  table <- gt::cols_align(table, align = "center", columns = -1L)
  table <- gt::tab_style(table, style = gt::cell_fill(color = "#d3d3d3"),
    locations = gt::cells_body(columns = "Pathogen"))
  table <- gt::tab_style(table, style = gt::cell_fill(color = "#d3d3d3"),
    locations = gt::cells_column_labels(columns = "Pathogen"))
  table <- gt::tab_style(table,
    style = list(gt::cell_text(color = "#17324a", weight = "bold"),
      gt::cell_fill(color = "#edf2f7")),
    locations = gt::cells_column_labels(columns = names(data)))
  table <- gt::tab_style(table,
    style = list(gt::cell_text(color = "#17324a", weight = "bold"),
      gt::cell_fill(color = "#d3d3d3")),
    locations = gt::cells_column_labels(columns = "Pathogen"))
  for (column in names(data)[-1L]) {
    rows <- which(data[[column]] == "NA")
    # R's snow3 is #CDC9C9; use its hex value for browser-compatible CSS.
    if (length(rows)) table <- gt::tab_style(table, style = gt::cell_fill(color = "#CDC9C9"),
      locations = gt::cells_body(columns = column, rows = rows))
  }

  trend_palette <- c(very_likely_upward = "#b2182b", likely_upward = "#ef8a62",
    upward = "#fddbc7", no_trend = "#FFFAFA", downward = "#d1e5f0",
    likely_downward = "#67a9cf", very_likely_downward = "#2166ac", sporadic = "#e5edff")
  trend_text <- c(very_likely_upward = "#FFFFFF", very_likely_downward = "#FFFFFF",
    sporadic = "#2458c6")
  detection_palette <- c(not_detected = "#dcfce7", detected = "#fed7aa")
  level_palette <- c(high = "#b2182b", medium = "gold", low = "green", detection_palette)
  level_text <- c(high = "#FFFFFF", low = "#000000")
  wlevel_palette <- c(very_low = "#cfeee9", low = "#b9e5a8", moderate = "#f9aa2b",
    high = "#f15a50", very_high = "#9c2f6f", detection_palette)

  for (j in seq_along(locations)) {
    location_metrics <- metrics[metrics$location == locations[j], , drop = FALSE]
    location_metrics <- location_metrics[match(selected, location_metrics$pathogen), , drop = FALSE]
    trend_column <- paste0("loc", j, "_trend")
    if (trend_column %in% names(data)) {
      for (key in names(trend_palette)) {
        rows <- which(!is.na(location_metrics$trend) & location_metrics$trend == key)
        if (length(rows)) {
          text_color <- if (key %in% names(trend_text)) unname(trend_text[[key]]) else "#17324a"
          table <- gt::tab_style(table,
            style = list(gt::cell_fill(color = unname(trend_palette[[key]])),
              gt::cell_text(color = text_color)),
            locations = gt::cells_body(columns = trend_column, rows = rows))
        }
      }
    }
    level_column <- paste0("loc", j, "_level")
    if (level_column %in% names(data)) {
      for (key in names(level_palette)) {
        rows <- which(!is.na(location_metrics$level) & location_metrics$level == key)
        if (length(rows)) {
          text_color <- if (key %in% names(level_text)) unname(level_text[[key]]) else "#17324a"
          table <- gt::tab_style(table,
            style = list(gt::cell_fill(color = unname(level_palette[[key]])),
              gt::cell_text(color = text_color)),
            locations = gt::cells_body(columns = level_column, rows = rows))
        }
      }
    }
    for (kind in intersect(c("wlevel", "wval"), kinds)) {
      level_column_new <- paste0("loc", j, "_", kind)
      for (key in names(wlevel_palette)) {
        rows <- which(!is.na(location_metrics[[kind]]) & location_metrics[[kind]] == key)
        if (length(rows)) table <- gt::tab_style(table,
          style = list(gt::cell_fill(color = unname(wlevel_palette[[key]])),
            gt::cell_text(color = if (key == "very_high") "#FFFFFF" else "#17324a")),
          locations = gt::cells_body(columns = level_column_new, rows = rows))
      }
      if (kind == "wval") {
        for (status in setdiff(unique(stats::na.omit(location_metrics$wval_status)),
            c("wval_local", "wval_county_local", "wval_not_applicable"))) {
          rows <- which(location_metrics$wval_status == status)
          table <- gt::tab_footnote(table, footnote = tr(status, lang),
            locations = gt::cells_body(columns = level_column_new, rows = rows))
        }
      }
    }
    for (metric_column in intersect(c(trend_column, level_column), names(data))) {
      nd_rows <- which(data[[metric_column]] == tr("nd", lang))
      if (length(nd_rows)) table <- gt::tab_style(table,
        style = list(gt::cell_fill(color = "#e5edff"),
          gt::cell_text(color = "#2458c6", weight = "bold")),
        locations = gt::cells_body(columns = metric_column, rows = nd_rows))
    }

    column <- paste0("loc", j, "_pc")
    if (!column %in% names(data)) next
    positive <- which(!is.na(location_metrics$pc_status) & location_metrics$pc_status == "numeric" &
      is.finite(location_metrics$pc) & location_metrics$pc > 0)
    negative <- which(!is.na(location_metrics$pc_status) & location_metrics$pc_status == "numeric" &
      is.finite(location_metrics$pc) & location_metrics$pc < 0)
    special <- which(!is.na(location_metrics$pc_status) &
      location_metrics$pc_status %in% c("nd", "sporadic", "low_baseline"))
    if (length(positive)) table <- gt::tab_style(table,
      style = list(gt::cell_text(color = "#b42318", weight = "bold"),
        gt::cell_fill(color = "#fde2df")),
      locations = gt::cells_body(columns = column, rows = positive))
    if (length(negative)) table <- gt::tab_style(table,
      style = list(gt::cell_text(color = "#167342", weight = "bold"),
        gt::cell_fill(color = "#e1f2e5")),
      locations = gt::cells_body(columns = column, rows = negative))
    if (length(special)) table <- gt::tab_style(table,
      style = list(gt::cell_text(color = "#2458c6", weight = "bold"),
        gt::cell_fill(color = "#e5edff")),
      locations = gt::cells_body(columns = column, rows = special))
  }
  methods <- method_text(lang, config)
  source_notes <- character()
  if ("pc" %in% kinds) source_notes <- c(source_notes, methods$pc)
  if ("trend" %in% kinds) source_notes <- c(source_notes, methods$trend)
  if ("wval" %in% kinds) {
    source_notes <- c(source_notes, tr("comparison_note", lang), tr("concentration_units", lang),
      tr("wval_method", lang))
  }
  if (length(source_notes)) {
    table <- gt::tab_source_note(table, source_note = paste(source_notes, collapse = " "))
  }
  if (any(c("level", "wlevel") %in% kinds)) {
    table <- gt::tab_source_note(table, source_note = tr("detection_table_note", lang))
  }
  if (isTRUE(show_latest_sample)) {
    sample_rows <- metrics[metrics$pathogen %in% selected & metrics$location %in% locations,
      c("location", "pathogen", "last_sample_date"), drop = FALSE]
    sample_grid <- expand.grid(pathogen = selected, location = locations, stringsAsFactors = FALSE)
    sample_rows <- sample_rows[match(paste(sample_grid$pathogen, sample_grid$location, sep = "\r"),
      paste(sample_rows$pathogen, sample_rows$location, sep = "\r")), , drop = FALSE]
    sample_labels <- pathogen_labels(config, lang)
    sample_lines <- character()
    for (location in setdiff(locations, "Modesto")) {
      location_rows <- sample_rows[sample_rows$location == location, , drop = FALSE]
      dates <- unique(location_rows$last_sample_date)
      if (nrow(location_rows) && length(dates) == 1L && !is.na(dates)) {
        sample_lines <- c(sample_lines, paste0(location, ": ", format(dates)))
      } else if (nrow(location_rows)) {
        formatted_dates <- rep("NA", nrow(location_rows))
        available <- !is.na(location_rows$last_sample_date)
        formatted_dates[available] <- format(location_rows$last_sample_date[available])
        sample_lines <- c(sample_lines, paste0(location, " - ",
          unname(sample_labels[location_rows$pathogen]), ": ", formatted_dates))
      }
    }
    sample_heading <- if (lang == "es") "Fecha de la última muestra" else "Latest sample date"
    sample_note <- gt::html(paste0("<div style='max-height:120px;overflow-y:auto;'>",
      "<strong>", sample_heading, "</strong><br>", paste(sample_lines, collapse = "<br>"), "</div>"))
    table <- gt::tab_source_note(table, source_note = sample_note)
  }
  gt::tab_options(table, table.width = gt::pct(100), table.font.size = gt::px(14),
    table_body.vlines.style = "solid", table_body.vlines.color = "#cbd5e1",
    column_labels.vlines.style = "solid", column_labels.vlines.color = "#cbd5e1")
}


# ---- www/main.R ----
# Build the shared model once from validated input data.
build_app_model <- function(data, config, today = Sys.Date(), trend_test = mk_test) {
  # Rebuild from _raw so an old cache can never supply trimmed derived values.
  if (!identical(attr(data, "preprocessing_settings"), list(config$preprocessing, config$moving_days,
      config$concentration_basis)) ||
      any(data$Collection_Date > as.Date(today), na.rm = TRUE)) {
    data <- prepare_data(data, config, as_of_date = today)
  }
  wval_samples <- attr(data, "wval_samples")
  report_end <- reporting_end(data, today)
  data <- data[data$Collection_Date <= today, , drop = FALSE]
  analysis_end <- max(data$Collection_Date, na.rm = TRUE)
  city_locations <- unique(c(config$reporting_cities, unlist(config$summary_counties, use.names = FALSE)))
  city_metrics <- calculate_metrics(data, city_locations, config, analysis_end,
    trend_test = trend_test, pc_end_date = report_end)
  county_data <- aggregate_counties(data, config)
  county_metrics <- calculate_metrics(county_data, names(config$counties), config, analysis_end,
    county = TRUE, trend_test = trend_test, pc_end_date = report_end)
  county_metrics <- apply_county_rules(county_metrics, city_metrics, config)
  city_metrics <- add_wval_metrics(city_metrics, wval_samples, as.Date(today), config)
  county_metrics <- add_wval_metrics(county_metrics, wval_samples, as.Date(today), config, city_metrics)
  list(data = data, report_end = report_end, analysis_end = analysis_end, city_metrics = city_metrics,
    county_metrics = county_metrics)
}

# ---- www/percentiles.R ----
# Only the retained Influenza plot lives here. Outbreak/percentile plots were removed.
influenza_plot_data <- function(data, city, start_date, end_date) {
  columns <- paste0(c("Influenza_A_norm_PMMoV", "InfA_H1_norm_PMMoV",
    "InfA_H3_norm_PMMoV", "InfA_H5_norm_PMMoV"), "_10")
  x <- data[data$City == city & data$Collection_Date >= start_date &
    data$Collection_Date <= end_date, , drop = FALSE]
  if (!nrow(x)) return(data.frame(date = as.Date(character()), value = numeric(), series = character()))
  # A missing column (e.g. an old cache) becomes an all-NA series instead of an error.
  values <- vapply(columns, function(column) {
    if (column %in% names(x)) suppressWarnings(as.numeric(x[[column]])) else rep(NA_real_, nrow(x))
  }, numeric(nrow(x)))
  values <- matrix(values, nrow = nrow(x), dimnames = list(NULL, columns))
  # A partial subtype total would understate the sum: require all three.
  total <- rowSums(values[, 2:4, drop = FALSE], na.rm = FALSE)
  series <- c("Influenza A", "H1", "H3", "H5", "flu_sum")
  values <- cbind(values, total)
  plot_data <- data.frame(date = rep(x$Collection_Date, times = 5L),
    value = as.numeric(values), series = rep(series, each = nrow(x)), stringsAsFactors = FALSE)
  minima <- tapply(plot_data$value, plot_data$series, function(y) {
    y <- y[is.finite(y)]
    if (length(y)) min(y) else NA_real_
  })
  plot_data$value <- plot_data$value - unname(minima[plot_data$series])
  plot_data
}

fluA_plot <- function(data, city, start_date, end_date, lang) {
  plot_data <- influenza_plot_data(data, city, start_date, end_date)
  plot_data$series[plot_data$series == "flu_sum"] <- tr("flu_sum", lang)
  ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = value, color = series)) +
    ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2::scale_x_date(limits = c(as.Date(start_date), as.Date(end_date))) +
    ggplot2::labs(title = paste(city, tr("flu_title", lang), sep = " — "),
      x = tr("sample_date", lang), y = tr("concentration", lang), color = tr("series", lang)) +
    ggplot2::theme_minimal()
}


