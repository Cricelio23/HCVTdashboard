city_pathogen_group <- function(group, config) {
  switch(group,
    seasonal = config$seasonal,
    nonseasonal = setdiff(names(config$pathogens), config$seasonal),
    respiratory = config$respiratory,
    gastrointestinal = config$gastrointestinal,
    names(config$pathogens))
}

city_pathogen_card_colors <- function(metric, key = NA_character_, value = NA_real_) {
  missing <- c(background = "#eef1f3", text = "#17324a", border = "#CDC9C9")
  if (is.na(key)) return(missing)
  if (metric == "trend") {
    borders <- c(very_likely_upward = "#b2182b", likely_upward = "#ef8a62",
      upward = "#fddbc7", no_trend = "#d9d9d9", downward = "#d1e5f0",
      likely_downward = "#67a9cf", very_likely_downward = "#2166ac", sporadic = "#8aa8e8")
    backgrounds <- c(very_likely_upward = "#f3d4d7", likely_upward = "#f9ded4",
      upward = "#fdf0e9", no_trend = "#FFFAFA", downward = "#edf4f8",
      likely_downward = "#dceaf3", very_likely_downward = "#d4e0ee", sporadic = "#f0f4ff")
    text <- if (key %in% c("very_likely_upward", "likely_upward", "upward")) "#8c1d27" else
      if (key %in% c("very_likely_downward", "likely_downward", "downward")) "#174f83" else
        if (key == "sporadic") "#2458c6" else "#17324a"
    if (!key %in% names(backgrounds)) return(missing)
    return(c(background = unname(backgrounds[[key]]), text = text,
      border = unname(borders[[key]])))
  }
  if (metric == "pc") {
    if (key == "numeric" && is.finite(value)) {
      if (value > 0) return(c(background = "#fdebea", text = "#8f1c14", border = "#b42318"))
      if (value < 0) return(c(background = "#edf7ef", text = "#125c35", border = "#167342"))
      return(c(background = "#FFFAFA", text = "#17324a", border = "#d9d9d9"))
    }
    if (key %in% c("nd", "sporadic", "low_baseline")) {
      return(c(background = "#f0f4ff", text = "#2458c6", border = "#8aa8e8"))
    }
    return(missing)
  }
  backgrounds <- c(low = "#edf7ef", medium = "#fff8dc", high = "#fdebea")
  borders <- c(low = "green", medium = "gold", high = "#b2182b")
  text <- c(low = "#125c35", medium = "#6f5700", high = "#8f1c14")
  if (!key %in% names(backgrounds)) return(missing)
  c(background = unname(backgrounds[[key]]), text = unname(text[[key]]), border = unname(borders[[key]]))
}

city_pathogen_card <- function(label, value, class, colors) {
  style <- paste0("background:", colors[["background"]], ";border-left-color:",
    colors[["border"]], ";color:", colors[["text"]], ";")
  shiny::div(class = paste("metric-card", class), style = style,
    shiny::div(class = "metric-card-label", style = paste0("color:", colors[["text"]], ";"), label),
    shiny::div(class = "metric-card-value", style = paste0("color:", colors[["text"]], ";"), value))
}

city_pathogen_plot <- function(data, city, pathogen, start_date, end_date, config, lang,
    ww_record = NULL) {
  series <- metric_series(data, city, pathogen, end_date)
  cuts <- level_cutoffs(series, pathogen, end_date, config)
  visible <- series[series$date >= start_date & series$date <= end_date, , drop = FALSE]
  observed <- visible[is.finite(visible$raw), , drop = FALSE]
  if (!nrow(observed)) return(NULL)
  positive <- observed$raw[observed$raw > 0]
  series_positive <- series$raw[is.finite(series$raw) & series$raw > 0]
  record_lod <- if (is.list(ww_record)) ww_record$lod else NULL
  lod <- if (length(record_lod) == 1L && isTRUE(is.finite(record_lod))) record_lod else
    if (length(series_positive)) min(series_positive) else 1
  quantified <- positive[positive > lod]
  raw_quantile <- if (length(quantified)) as.numeric(stats::quantile(quantified, config$city_pathogen$raw_cap_prob,
    na.rm = TRUE, names = FALSE, type = config$preprocessing$quantile_type)) else lod
  model_curve <- if (is.list(ww_record) && is.data.frame(ww_record$smooth)) ww_record$smooth else NULL
  if (!is.null(model_curve) && "date" %in% names(model_curve)) {
    model_curve$date <- as.Date(model_curve$date)
    model_curve <- model_curve[model_curve$date >= start_date & model_curve$date <= end_date, , drop = FALSE]
  }
  candidates <- c(raw_quantile, visible$smooth[visible$smooth > 0], cuts)
  if (!is.null(model_curve) && "smooth_hi" %in% names(model_curve)) candidates <- c(candidates, 10^model_curve$smooth_hi)
  if (!is.null(model_curve) && "level_smooth" %in% names(model_curve)) candidates <- c(candidates, 10^model_curve$level_smooth)
  y_limit <- max(candidates[is.finite(candidates)], na.rm = TRUE)
  if (!is.finite(y_limit) || y_limit <= 0) y_limit <- max(lod * 10, 1)

  observed$truncated <- observed$raw > y_limit
  observed$nondetect <- observed$raw <= lod
  observed$plot_raw <- ifelse(observed$nondetect, lod, pmin(observed$raw, y_limit))
  observed$plot_y <- observed$plot_raw
  regular <- observed[!observed$truncated & !observed$nondetect, , drop = FALSE]
  nondetect <- observed[observed$nondetect, , drop = FALSE]
  truncated <- observed[observed$truncated, , drop = FALSE]

  bands <- NULL
  if (all(is.finite(cuts)) && cuts[1] <= cuts[2]) {
    bands <- data.frame(ymin = c(0, cuts[1], cuts[2]),
      ymax = c(cuts[1], cuts[2], y_limit), level = c("low", "medium", "high"))
    bands <- bands[bands$ymax > bands$ymin, , drop = FALSE]
  }
  labels <- c(raw = tr("raw_observations", lang), nondetect = tr("nondetect_at_lod", lang),
    moving = tr("moving_geometric_mean", lang),
    model = tr("model_smoothed_curve", lang), truncated = tr("above_plotting_limit", lang))
  units <- if (identical(city, "Modesto")) "copies/L" else "gc/g dry weight"
  plot <- ggplot2::ggplot()
  if (!is.null(bands) && nrow(bands)) {
    plot <- plot + ggplot2::geom_rect(data = bands,
      ggplot2::aes(xmin = start_date, xmax = end_date, ymin = ymin, ymax = ymax, fill = level),
      inherit.aes = FALSE, alpha = .28) +
      ggplot2::geom_hline(yintercept = cuts, color = "#6f7f89", linetype = "dashed", linewidth = .45)
  }
  if (nrow(regular)) {
    plot <- plot + ggplot2::geom_point(data = regular,
      ggplot2::aes(x = date, y = plot_y, color = labels[["raw"]]), size = 1.9)
  }
  if (nrow(nondetect)) {
    plot <- plot + ggplot2::geom_point(data = nondetect,
      ggplot2::aes(x = date, y = plot_y, color = labels[["nondetect"]]),
      shape = 1, size = 2.2, stroke = .9)
  }
  if (nrow(truncated)) {
    plot <- plot + ggplot2::geom_point(data = truncated,
      ggplot2::aes(x = date, y = plot_y, color = labels[["truncated"]],
        shape = labels[["truncated"]]), size = 3)
  }
  moving <- visible
  moving$segment <- cumsum(!is.finite(moving$smooth))
  moving <- moving[is.finite(moving$smooth) & moving$smooth > 0, , drop = FALSE]
  if (nrow(moving)) {
    plot <- plot + ggplot2::geom_line(data = moving,
      ggplot2::aes(x = date, y = smooth, color = labels[["moving"]], group = segment), linewidth = .8)
  }
  if (!is.null(model_curve) && all(c("date", "level_smooth") %in% names(model_curve))) {
    model_curve$date <- as.Date(model_curve$date)
    model_curve <- model_curve[model_curve$date >= start_date & model_curve$date <= end_date &
      is.finite(model_curve$level_smooth), , drop = FALSE]
    if (all(c("smooth_lo", "smooth_hi") %in% names(model_curve)) && nrow(model_curve)) {
      model_curve$smooth_lo_conc <- 10^model_curve$smooth_lo
      model_curve$smooth_hi_conc <- 10^model_curve$smooth_hi
      plot <- plot + ggplot2::geom_ribbon(data = model_curve,
        ggplot2::aes(x = date, ymin = smooth_lo_conc, ymax = smooth_hi_conc),
        inherit.aes = FALSE, fill = "#62a8c7", alpha = .18)
    }
    if (nrow(model_curve)) {
      model_curve$provisional <- if ("provisional" %in% names(model_curve)) model_curve$provisional else FALSE
      model_curve$segment <- cumsum(!is.finite(model_curve$level_smooth))
      model_curve$level_conc <- 10^model_curve$level_smooth
      stable_curve <- model_curve[!model_curve$provisional, , drop = FALSE]
      provisional_curve <- model_curve[model_curve$provisional, , drop = FALSE]
      if (nrow(stable_curve)) plot <- plot + ggplot2::geom_line(data = stable_curve,
        ggplot2::aes(x = date, y = level_conc, color = labels[["model"]], group = segment), linewidth = 1)
      if (nrow(provisional_curve)) plot <- plot + ggplot2::geom_line(data = provisional_curve,
        ggplot2::aes(x = date, y = level_conc, color = labels[["model"]], group = segment),
        linewidth = 1, linetype = "dashed", show.legend = FALSE)
    }
  }
  if (nrow(truncated)) {
    plot <- plot + ggplot2::scale_shape_manual(
      values = stats::setNames(17, labels[["truncated"]]), name = NULL)
  }
  plot +
    ggplot2::scale_fill_manual(values = c(low = "#b9e5a8", medium = "#f9df86", high = "#f7aaa4"),
      labels = metric_label(c("low", "medium", "high"), lang), name = tr("viral_activity_level", lang)) +
    ggplot2::scale_color_manual(values = stats::setNames(c("#172b3a", "#426b8a",
      "#087eae", "#117766", "#9c2f2f"), unname(labels)), name = NULL) +
    ggplot2::scale_x_date(limits = c(start_date, end_date), expand = ggplot2::expansion(mult = c(.005, .01))) +
    ggplot2::scale_y_continuous(limits = c(0, y_limit), labels = scales::label_number(),
      expand = ggplot2::expansion(mult = c(0, .05), add = c(0, 0))) +
    ggplot2::labs(x = tr("sample_date", lang),
      y = paste0(tr("wastewater_concentration", lang), " (", units, ")")) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank())
}

city_pathogen_growth_plot <- function(ww_record, city, pathogen, start_date, end_date, config, lang) {
  delta <- config$city_pathogen$growth_delta[pathogen]
  if (!length(delta) || is.na(delta)) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = .5,
      label = tr("growth_probability_unavailable", lang), size = 4) +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(title = tr("growth_probability", lang), x = tr("sample_date", lang), y = NULL) +
      ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank()))
  }
  published <- if (is.list(ww_record)) ww_record$published else NULL
  title_key <- if (delta < .15) "growth_threshold_25" else "growth_threshold_62"
  if (!is.data.frame(published) || !nrow(published)) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = .5,
      label = tr("insufficient_history", lang), size = 4) +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(title = tr(title_key, lang), x = tr("sample_date", lang), y = tr("growth_probability", lang)) +
      ggplot2::theme_minimal(base_size = 13))
  }
  published$date <- as.Date(published$date)
  published <- published[published$date >= start_date & published$date <= end_date, , drop = FALSE]
  alerts <- published[!is.na(published$alert) & published$alert, , drop = FALSE]
  if (nrow(alerts)) {
    alerts$xmin <- alerts$date - 0.5
    alerts$xmax <- alerts$date + 0.5
    alerts$ymin <- 0
    alerts$ymax <- 1
  }
  plot <- ggplot2::ggplot()
  if (nrow(alerts)) plot <- plot + ggplot2::geom_rect(data = alerts,
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = "#f2c66d", alpha = .24)
  plot + ggplot2::geom_hline(yintercept = .5, linetype = "dashed", color = "#667784") +
    ggplot2::geom_line(data = published, ggplot2::aes(x = date, y = P_growth),
      color = "#087eae", linewidth = .9, na.rm = TRUE) +
    ggplot2::scale_x_date(limits = c(start_date, end_date), expand = ggplot2::expansion(mult = c(.005, .01))) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .25),
      labels = scales::label_percent(accuracy = 1)) +
    ggplot2::labs(title = tr(title_key, lang), x = tr("sample_date", lang),
      y = tr("growth_probability", lang)) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}

city_pathogen_growth_status <- function(record, pathogen, lang, config = NULL) {
  configured <- !is.null(config) && pathogen %in% names(config$city_pathogen$growth_delta)
  record_delta <- if (is.list(record)) record$delta else NULL
  supported <- configured || (length(record_delta) == 1L && isTRUE(is.finite(record_delta)))
  if (!supported) return(tr("growth_probability_unavailable", lang))
  if (is.null(record) || !is.list(record)) return(tr("insufficient_history", lang))
  published <- record$published
  if (!is.data.frame(published) || !nrow(published)) {
    message <- if (identical(record$status, "few_quantified")) tr("few_quantified", lang) else tr("insufficient_history", lang)
    return(message)
  }
  last <- published[nrow(published), , drop = FALSE]
  if (!is.finite(last$P_growth)) {
    message <- if (identical(last$status, "few_quantified")) tr("few_quantified", lang) else
      if (identical(last$status, "insufficient_history")) tr("insufficient_history", lang) else tr("no_growth_data", lang)
    return(message)
  }
  alert <- if (isTRUE(last$alert)) tr("alert_active", lang) else tr("alert_inactive", lang)
  value <- paste0(format(round(100 * last$P_growth, 1), nsmall = 1), "% — ", alert,
    " (", format(last$date, "%Y-%m-%d"), ")")
  smooth_dates <- if (is.data.frame(record$smooth) && "date" %in% names(record$smooth)) as.Date(record$smooth$date) else as.Date(character())
  if (length(smooth_dates) && max(smooth_dates) - min(smooth_dates) < 183) {
    paste(value, tr("recent_series_note", lang))
  } else value
}

city_pathogen_clean_legend <- function(widget, lang) {
  replacements <- c(
    "level: low" = metric_label("low", lang),
    "level: medium" = metric_label("medium", lang),
    "level: high" = metric_label("high", lang),
    "(low," = metric_label("low", lang),
    "(medium," = metric_label("medium", lang),
    "(high," = metric_label("high", lang),
    stats::setNames(tr("raw_observations", lang), tr("raw_observations", lang)),
    stats::setNames(tr("above_plotting_limit", lang), tr("above_plotting_limit", lang)),
    stats::setNames(tr("moving_geometric_mean", lang), tr("moving_geometric_mean", lang))
  )
  widget$x$data <- lapply(widget$x$data, function(trace) {
    name <- if (is.null(trace$name)) "" else as.character(trace$name)
    if (grepl(tr("nondetect_at_lod", lang), name, fixed = TRUE)) {
      trace$name <- tr("nondetect_at_lod", lang)
      trace$legendgroup <- trace$name
      return(trace)
    }
    if (grepl(tr("model_smoothed_curve", lang), name, fixed = TRUE)) {
      trace$name <- tr("model_smoothed_curve", lang)
      trace$legendgroup <- trace$name
      return(trace)
    }
    for (pattern in names(replacements)) {
      if (grepl(pattern, name, fixed = TRUE)) {
        trace$name <- unname(replacements[[pattern]])
        trace$legendgroup <- unname(replacements[[pattern]])
        break
      }
    }
    trace
  })
  widget
}

mod_city_pathogen_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h2(shiny::textOutput(ns("title"))),
    shiny::div(class = "city-pathogen-controls",
      shiny::selectInput(ns("city"), label = NULL, choices = character()),
      shiny::selectInput(ns("group"), label = NULL, choices = character()),
      shiny::selectInput(ns("pathogen"), label = NULL, choices = character()),
      shiny::div(class = "city-pathogen-window",
        shiny::radioButtons(ns("window"), label = NULL,
          choices = c("1 month" = 30L, "6 months" = 183L, "1 year" = 365L,
            "2 years" = 730L, "ALL" = 0L), selected = 730L, inline = TRUE))),
    shiny::div(class = "metric-cards",
      shiny::uiOutput(ns("trend_card")), shiny::uiOutput(ns("pc_card")),
      shiny::uiOutput(ns("level_card"))),
    shiny::div(class = "city-pathogen-plot",
      plotly::plotlyOutput(ns("plot"), width = "100%", height = "560px"))
  )
}

mod_city_pathogen_server <- function(id, model, lang, config) {
  shiny::moduleServer(id, function(input, output, session) {
    output$title <- shiny::renderText(tr("city_pathogen_title", lang()))
    shiny::observe({
      city <- shiny::isolate(if (!is.null(input$city) && input$city %in% config$reporting_cities) input$city else
        config$city_pathogen$default_city)
      shiny::updateSelectInput(session, "city", label = tr("selected_city", lang()),
        choices = config$reporting_cities, selected = city)
      group_choices <- stats::setNames(c("all", "seasonal", "nonseasonal", "respiratory", "gastrointestinal"),
        tr(c("all", "seasonal", "nonseasonal", "respiratory", "gastrointestinal"), lang()))
      group <- shiny::isolate(if (!is.null(input$group) && input$group %in% unname(group_choices)) input$group else "all")
      shiny::updateSelectInput(session, "group", label = tr("pathogen_group", lang()),
        choices = group_choices, selected = group)
      window_choices <- stats::setNames(unname(config$city_pathogen$windows),
        tr(names(config$city_pathogen$windows), lang()))
      shiny::updateRadioButtons(session, "window", label = tr("lookback_window", lang()),
        choices = window_choices, selected = shiny::isolate(if (is.null(input$window))
          config$city_pathogen$default_window else input$window), inline = TRUE)
    })
    shiny::observe({
      available <- city_pathogen_group(if (is.null(input$group)) "all" else input$group, config)
      choices <- pathogen_choices(config, lang())
      choices <- choices[unname(choices) %in% available]
      current_pathogen <- shiny::isolate(input$pathogen)
      selected <- if (!is.null(current_pathogen) && current_pathogen %in% unname(choices)) current_pathogen else
        if (config$city_pathogen$default_pathogen %in% unname(choices)) config$city_pathogen$default_pathogen else
          unname(choices[1L])
      shiny::updateSelectInput(session, "pathogen", label = tr("selected_pathogen", lang()),
        choices = choices, selected = selected)
    })
    selected_metric <- shiny::reactive({
      value <- model()
      value$city_metrics[value$city_metrics$location == input$city &
        value$city_metrics$pathogen == input$pathogen, , drop = FALSE]
    })
    output$trend_card <- shiny::renderUI({
      row <- selected_metric()
      key <- if (nrow(row)) row$trend[1] else NA_character_
      city_pathogen_card(tr("trend", lang()), metric_label(key, lang()), "trend",
        city_pathogen_card_colors("trend", key))
    })
    output$pc_card <- shiny::renderUI({
      row <- selected_metric()
      value <- if (nrow(row)) format_pc(row$pc[1], row$pc_status[1], lang()) else "NA"
      status <- if (nrow(row)) row$pc_status[1] else NA_character_
      numeric_value <- if (nrow(row)) row$pc[1] else NA_real_
      city_pathogen_card(tr("percent_change", lang()), value, "pc",
        city_pathogen_card_colors("pc", status, numeric_value))
    })
    output$level_card <- shiny::renderUI({
      row <- selected_metric()
      key <- if (nrow(row)) row$level[1] else NA_character_
      city_pathogen_card(tr("viral_activity_level", lang()),
        metric_label(key, lang()), "level", city_pathogen_card_colors("level", key))
    })
    output$plot <- plotly::renderPlotly({
      value <- model()
      shiny::req(input$city, input$pathogen, input$window)
      days <- suppressWarnings(as.integer(input$window))
      end_date <- value$analysis_end
      start_date <- if (is.na(days) || days <= 0L) min(value$data$Collection_Date, na.rm = TRUE) else
        end_date - days + 1L
      plot <- city_pathogen_plot(value$data, input$city, input$pathogen,
        start_date, end_date, config, lang())
      shiny::validate(shiny::need(!is.null(plot), tr("no_plot_data", lang())))
      widget <- city_pathogen_clean_legend(plotly::ggplotly(plot), lang())
      widget <- plotly::layout(widget, autosize = TRUE)
      plotly::config(widget, responsive = TRUE)
    })
  })
}
