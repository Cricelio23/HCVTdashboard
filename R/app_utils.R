# ---- www/Help.R ----
# Text belongs to the presentation layer, not to numerical calculations.
method_text <- function(lang, config) {
  if (identical(lang, "es")) return(list(
    pc = "Calculamos el cambio porcentual (PC) comparando el promedio de las mediciones de las últimas dos semanas con el promedio de las dos semanas anteriores. Las mediciones por debajo del límite de detección se incluyen como la mitad de ese límite.",
    level = c(
      "Para patógenos no estacionales, los límites de las categorías bajo, medio y alto son los terciles de todos los datos disponibles para ese patógeno en cada sitio o condado.",
      "Para patógenos estacionales, los límites de bajo, medio y alto son los tercios del percentil 95 de todos los datos disponibles para ese patógeno en cada sitio o condado.",
      "Si no hubo al menos una muestra positiva en los últimos 7 días y al menos tres en los últimos 14 días, se asigna la categoría Bajo."
    ),
    trend = "La prueba de Mann-Kendall (MK) es no paramétrica y evalúa si existe una tendencia monótona en una serie temporal. Aplicamos MK a los datos suavizados de N/PMMoV con una ventana retrospectiva de 14 días desde el último dato disponible. Interpretamos valores positivos de tau con p ≤ 0.1, p ≤ 0.05 y p ≤ 0.005 como tendencias ascendentes, probablemente ascendentes y muy probablemente ascendentes, respectivamente. Los valores negativos de tau con esos umbrales se interpretan como tendencias descendentes, probablemente descendentes y muy probablemente descendentes.",
    criteria = c(
      "Criterio: se necesitan al menos tres detecciones positivas en los últimos 14 días para reportar una tendencia.",
      "Criterio: se necesitan al menos tres detecciones positivas en los últimos 14 días para reportar el cambio porcentual.",
      "NA: si no hay observaciones del patógeno durante los últimos 14 días, todas las métricas (PC, Nivel y Tendencia) se reportan como NA.",
      "NA: si no se cumple el criterio anterior y no hubo datos disponibles en los últimos 7 días.",
      "Esporádico: se etiqueta como detección esporádica si no se cumple el criterio anterior, pero hubo al menos una detección positiva en los últimos 14 días.",
      "ND: no detectado (es decir, por debajo del límite de detección) si no hubo detecciones en los últimos 14 días.",
      "+LL: aumento a niveles bajos. Se usa si se cumple el criterio, pero no hubo detecciones positivas o no hubo datos en las dos semanas anteriores.",
      "+Marcado: el cambio porcentual es mayor de 500%.",
      "Ciudad: si no hubo muestras positivas en los últimos 7 días y hubo al menos tres muestras positivas en los últimos 14 días, se asigna el nivel Bajo.",
      "Condado: si todas las ciudades del condado tienen nivel Bajo, el condado se clasifica como Bajo."
    ),
    no_sea_lab = "Para patógenos no estacionales, los límites de las categorías Bajo, Medio y Alto se definen con los terciles de todos los datos disponibles para ese patógeno en cada sitio o condado.",
    sea_lab = "Para patógenos estacionales, los límites de las categorías Bajo, Medio y Alto se definen como los tercios del valor del percentil 95 de todos los datos disponibles para ese patógeno en cada sitio o condado.",
    pc_cri = "Si no hubo al menos una muestra positiva en los últimos 7 días y al menos tres muestras positivas en los últimos 14 días, se asigna la categoría Bajo.",
    na_14 = "Si no hay observaciones del patógeno durante los últimos 14 días, todas las métricas (PC, Nivel y Tendencia) se reportan como NA.",
    city_des_1 = "Nivel de ciudad: si no hubo muestras positivas en los últimos 7 días y hubo al menos tres muestras positivas en los últimos 14 días, se asigna el nivel Bajo.",
    county_des_1 = "Nivel de condado: si todas las ciudades del condado tienen nivel Bajo, el condado se clasifica como Bajo.",
    data = "Las tendencias agregadas por condado se calculan usando el promedio recortado de 10 días, alineado a la derecha, para cada sitio (objetivo/PMMoV recortado). Para cada fecha, el valor suavizado recortado del sitio i en el día t se multiplica por la población de la planta, pop_i. Después, se suman esos productos y se dividen entre la población total de todos los sitios seleccionados. Así se obtiene el promedio ponderado recortado del condado para el día t.",
    equation = "$$\\text{objetivo/PMMoV}_{t} = \\frac{\\sum_{i=1}^{m}(\\text{objetivo/PMMoV recortado}_{i,t} \\times pop_i)}{\\sum_{i=1}^{m} pop_i}$$",
    rsi = "El índice de fuerza relativa (RSI) calcula la razón entre los movimientos recientes al alza y el movimiento absoluto del precio. Fue desarrollado por J. Welles Wilder.",
    slope_pc = "La pendiente se calcula con una regresión lineal de mínimos cuadrados sobre los valores transformados con log10 de N/PMMoV frente al día. Se usa una ventana retrospectiva de 14 días y se extraen intervalos de confianza (IC) de 90%, 95% y 99% para cada estimación de PC. Los límites superiores positivos indican tendencias ascendentes, probablemente ascendentes y muy probablemente ascendentes; los límites inferiores negativos indican las tendencias descendentes correspondientes."
  ))
  list(
    pc = "We calculate Percent Change (PC) using the average of measurements over the past two weeks compared to the average of measurements from the prior two weeks. Results below the limit of detection are included in the averages as half the limit of detection.",
    level = c(
      "For non-seasonal pathogens, the boundaries of the low, medium, and high labels are defined as the tertiles of all data available for that pathogen within a given site or county.",
      "For seasonal pathogens, the boundaries of the low, medium, and high labels are defined as the thirds of the 95th percentile value for all of the data available for that pathogen within a given site or county.",
      "If there has not been at least one positive sample in the last 7 days and at least three positive samples in the last 14 days, a Low categorization is assigned."
    ),
    trend = "The Mann-Kendall (MK) trend test is a nonparametric test that evaluates whether there is a monotonic trend in a time-series dataset. We applied the MK trend test to smoothed N/PMMoV data and used a 14-day look-back period from the last datapoint available. We interpreted positive values of the test statistic, tau, with p ≤ 0.1, p ≤ 0.05, and p ≤ 0.005 as upward, likely upward, and very likely upward trends, respectively. We interpreted negative tau values with the same thresholds as downward, likely downward, and very likely downward trends, respectively.",
    criteria = c(
      "Criteria: need at least three positive detections in the last 14 days to report a trend.",
      "Criteria: need at least three positive detections in the last 14 days to report a percent change.",
      "NA: if there are no pathogen observations in the last 14 days, all metrics (PC, Level, and Trend) are reported as NA.",
      "NA: if the above criteria are not met and there were no data available in the last 7 days.",
      "Sporadic: labeled as sporadic detections if the above criteria are not met, but there is at least 1 positive detection in the last 14 days.",
      "ND: non-detect (i.e., below the limit of detection) if there were no detections in the last 14 days.",
      "+LL: increasing but at low levels. If the above criteria are met but there were no positive detections or no data in the prior 14 days.",
      "+Sharply: percent change is greater than 500%.",
      "City level: If there haven't been any positive samples in the last 7 days and at least three positive samples in the last 14 days, a low categorization is assigned.",
      "County level: If all cities within the county are at a low level, classify the county itself as being at a low level."
    ),
    no_sea_lab = "For non-seasonal pathogens, the boundaries of the low, medium, and high labels are defined as the tertiles of all data available for that pathogen within a given site or county.",
    sea_lab = "For seasonal pathogens, the boundaries of the low, medium, and high labels are defined as the thirds of the 95th percentile value for all of the data available for that pathogen within a given site or county.",
    pc_cri = "If there have not been at least one positive sample in the last 7 days and at least three positive samples in the last 14 days, a low categorization is assigned.",
    na_14 = "If there are no pathogen observations in the last 14 days, all metrics (PC, Level, and Trend) are reported as NA.",
    city_des_1 = "City level: If there haven't been any positive samples in the last 7 days and at least three positive samples in the last 14 days, a low categorization is assigned.",
    county_des_1 = "County level: If all cities within the county are at a low level, classify the county itself as being at a low level.",
    data = "Aggregated trend lines for counties are calculated using the right-aligned 10-day trimmed average for each site (trimmed target/PMMoV). For each date, an individual site's trimmed smoothed value, i, for day t is multiplied by the plant's population, pop_i. These products are summed and divided by the total population of all selected sites to obtain the county trimmed weighted average for day t.",
    equation = "$$\\text{target/PMMoV}_{t} = \\frac{\\sum_{i=1}^{m}(\\text{trimmed target/PMMoV}_{i,t} \\times pop_i)}{\\sum_{i=1}^{m} pop_i}$$",
    rsi = "The Relative Strength Index (RSI) calculates a ratio of recent upward price movements to the absolute price movement. It was developed by J. Welles Wilder.",
    slope_pc = "The slope is calculated from a least-squares linear regression of log10-transformed N/PMMoV data versus day. We use a 14-day look-back period and extract 90%, 95%, and 99% confidence intervals (CIs) for each PC estimate. Positive upper CIs indicate upward, likely upward, and very likely upward trends; negative lower CIs indicate the corresponding downward trends."
  )
}

table_legend_ui <- function(lang) {
  text <- method_text(lang, config = NULL)
  shiny::tags$details(class = "table-legend",
    shiny::tags$summary(
      shiny::tags$span(tr("legend_title", lang)),
      shiny::tags$span(class = "table-legend-chevron", `aria-hidden` = "true", "▾")),
    shiny::div(class = "table-legend-content",
      shiny::tags$h4(tr("legend_levels", lang)),
      shiny::tags$p(text$no_sea_lab),
      shiny::tags$p(text$sea_lab),
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
    value = get_column(if (county) paste0(pathogen, "_10") else pathogen),
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

calculate_metric <- function(series, pathogen, end_date, config, trend_test = mk_test) {
  result <- list(pc = NA_real_, pc_status = NA_character_, level = NA_character_, trend = NA_character_)
  recent <- series[series$date >= end_date - 13L & series$date <= end_date, , drop = FALSE]
  if (!any(is.finite(recent$raw))) return(result)
  week <- recent[recent$date >= end_date - 6L, , drop = FALSE]
  n_positive <- sum(recent$raw > 0, na.rm = TRUE)
  has_week <- any(is.finite(week$raw))
  enough <- n_positive >= config$min_positive

  if (enough) {
    prior <- series[series$date >= end_date - 27L & series$date < end_date - 13L, , drop = FALSE]
    current_mean <- finite_mean(recent$value)
    prior_mean <- finite_mean(prior$value)
    if (is.finite(current_mean)) {
      if (!is.finite(prior_mean) || prior_mean == 0) {
        result$pc_status <- "low_baseline"
      } else {
        result$pc <- round(100 * (current_mean - prior_mean) / prior_mean, 1)
        result$pc_status <- "numeric"
      }
    }
  } else if (has_week) result$pc_status <- if (n_positive) "sporadic" else "nd"

  if (has_week) {
    if (!enough || !any(week$raw > 0, na.rm = TRUE)) {
      result$level <- "low"
    } else {
      baseline_end <- end_date - config$baseline_exclude
      historical <- series$value[series$date <= baseline_end &
        series$date >= baseline_end - config$baseline_days + 1L]
      historical <- historical[is.finite(historical)]
      last_value <- recent$smooth[is.finite(recent$smooth)]
      if (length(historical) && length(last_value)) {
        cuts <- if (pathogen %in% config$seasonal) {
          as.numeric(stats::quantile(historical, .95)) * c(1/3, 2/3)
        } else as.numeric(stats::quantile(historical, c(.33, .66)))
        latest <- tail(last_value, 1L)
        result$level <- if (latest <= cuts[1]) "low" else if (latest > cuts[2]) "high" else "medium"
      }
    }
  }
  if (enough) {
    values <- series$smooth[series$date >= end_date - config$trend_days + 1L]
    values <- values[is.finite(values)]
    if (length(values) >= 3L) {
      result$trend <- if (length(unique(values)) == 1L) "no_trend" else trend_test(values)
    }
  } else if (has_week) result$trend <- if (n_positive) "sporadic" else "no_trend"
  result
}

calculate_metrics <- function(data, locations, config, end_date, county = FALSE, trend_test = mk_test) {
  grid <- expand.grid(pathogen = names(config$pathogens), location = locations, stringsAsFactors = FALSE)
  grid$pc <- rep(NA_real_, nrow(grid))
  grid$pc_status <- grid$level <- grid$trend <- rep(NA_character_, nrow(grid))
  for (i in seq_len(nrow(grid))) {
    series <- metric_series(data, grid$location[i], grid$pathogen[i], end_date, county)
    value <- calculate_metric(series, grid$pathogen[i], end_date, config, trend_test)
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
      for (suffix in c("_raw", "_10")) {
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
    if (length(cities) && all(!is.na(city_rows$level) & city_rows$level == "low")) {
      county_metrics$level[i] <- "low"
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
      } else metric_label(x[[kind]], lang)
    }
  }
  result
}

build_metric_table <- function(metrics, selected, locations, kinds, config, lang, title) {
  data <- metric_table_data(metrics, selected, locations, kinds, config, lang)
  table <- gt::gt(data)
  table <- gt::cols_label(table, Pathogen = tr("pathogen", lang))
  for (j in seq_along(locations)) {
    columns <- paste0("loc", j, "_", kinds)
    labels <- as.list(stats::setNames(tr(kinds, lang), columns))
    table <- gt::cols_label(table, .list = labels)
    table <- gt::tab_spanner(table, label = locations[j], columns = columns)
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
    if (length(rows)) table <- gt::tab_style(table, style = gt::cell_fill(color = "#CDC9C9"),
      locations = gt::cells_body(columns = column, rows = rows))
  }

  trend_palette <- c(very_likely_upward = "#b2182b", likely_upward = "#ef8a62",
    upward = "#fddbc7", no_trend = "#FFFAFA", downward = "#d1e5f0",
    likely_downward = "#67a9cf", very_likely_downward = "#2166ac", sporadic = "#e5edff")
  trend_text <- c(very_likely_upward = "#FFFFFF", very_likely_downward = "#FFFFFF",
    sporadic = "#2458c6")
  level_palette <- c(high = "#b2182b", medium = "gold", low = "green")
  level_text <- c(high = "#FFFFFF", low = "#000000")

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
  table <- gt::tab_source_note(table, source_note = paste(source_notes, collapse = " "))
  gt::tab_options(table, table.width = gt::pct(100), table.font.size = gt::px(14))
}


# ---- www/main.R ----
# Build the shared model once from validated input data.
build_app_model <- function(data, config, today = Sys.Date(), trend_test = mk_test) {
  report_end <- reporting_end(data, today)
  data <- data[data$Collection_Date <= today, , drop = FALSE]
  city_locations <- unique(c(config$reporting_cities, unlist(config$summary_counties, use.names = FALSE)))
  city_metrics <- calculate_metrics(data, city_locations, config, report_end, trend_test = trend_test)
  county_data <- aggregate_counties(data, config)
  county_metrics <- calculate_metrics(county_data, names(config$counties), config, report_end,
    county = TRUE, trend_test = trend_test)
  county_metrics <- apply_county_rules(county_metrics, city_metrics, config)
  list(data = data, report_end = report_end, city_metrics = city_metrics,
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
  values <- as.matrix(x[, columns, drop = FALSE])
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


