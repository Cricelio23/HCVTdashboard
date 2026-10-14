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

city_pathogen_plot <- function(data, city, pathogen, start_date, end_date, config, lang) {
  series <- metric_series(data, city, pathogen, end_date)
  cuts <- level_cutoffs(series, pathogen, end_date, config)
  visible <- series[series$date >= start_date & series$date <= end_date, , drop = FALSE]
  observed <- visible[is.finite(visible$raw), , drop = FALSE]
  if (!nrow(observed)) return(NULL)

  raw_quantile <- as.numeric(stats::quantile(observed$raw, config$city_pathogen$raw_cap_prob,
    na.rm = TRUE, names = FALSE, type = config$preprocessing$quantile_type))
  candidates <- c(raw_quantile, visible$smooth, cuts)
  y_limit <- max(candidates[is.finite(candidates)], na.rm = TRUE)
  if (!is.finite(y_limit) || y_limit <= 0) y_limit <- 1

  observed$truncated <- observed$raw > y_limit
  observed$plot_raw <- pmin(observed$raw, y_limit)
  tolerance <- sqrt(.Machine$double.eps) * pmax(1, abs(observed$raw))
  observed$was_winsorized <- is.finite(observed$value) & observed$raw - observed$value > tolerance
  winsorized <- observed[observed$was_winsorized, , drop = FALSE]
  regular <- observed[!observed$truncated, , drop = FALSE]
  truncated <- observed[observed$truncated, , drop = FALSE]

  bands <- NULL
  if (all(is.finite(cuts)) && cuts[1] <= cuts[2]) {
    bands <- data.frame(ymin = c(0, cuts[1], cuts[2]),
      ymax = c(cuts[1], cuts[2], y_limit), level = c("low", "medium", "high"))
  }
  labels <- c(raw = tr("raw_observations", lang), winsorized = tr("winsorized_observations", lang),
    smooth = tr("moving_geometric_mean", lang), truncated = tr("above_plotting_limit", lang))
  units <- if (identical(city, "Modesto")) "copies/L" else "gc/g dry weight"
  plot <- ggplot2::ggplot()
  if (!is.null(bands)) {
    plot <- plot + ggplot2::geom_rect(data = bands,
      ggplot2::aes(xmin = start_date, xmax = end_date, ymin = ymin, ymax = ymax, fill = level),
      inherit.aes = FALSE, alpha = .28) +
      ggplot2::geom_hline(yintercept = cuts, color = "#6f7f89", linetype = "dashed", linewidth = .45)
  }
  if (nrow(regular)) {
    plot <- plot + ggplot2::geom_point(data = regular,
      ggplot2::aes(x = date, y = plot_raw, color = labels[["raw"]]), size = 1.9)
  }
  if (nrow(winsorized)) {
    plot <- plot + ggplot2::geom_point(data = winsorized,
      ggplot2::aes(x = date, y = value, color = labels[["winsorized"]]), size = 2.2)
  }
  if (nrow(truncated)) {
    plot <- plot + ggplot2::geom_point(data = truncated,
      ggplot2::aes(x = date, y = plot_raw, color = labels[["truncated"]],
        shape = labels[["truncated"]]), size = 3)
  }
  smooth <- visible
  smooth$segment <- cumsum(!is.finite(smooth$smooth))
  smooth <- smooth[is.finite(smooth$smooth), , drop = FALSE]
  if (nrow(smooth)) {
    plot <- plot + ggplot2::geom_line(data = smooth,
      ggplot2::aes(x = date, y = smooth, color = labels[["smooth"]], group = segment), linewidth = .8)
  }
  if (nrow(truncated)) {
    plot <- plot + ggplot2::scale_shape_manual(
      values = stats::setNames(17, labels[["truncated"]]), name = NULL)
  }
  plot +
    ggplot2::scale_fill_manual(values = c(low = "#b9e5a8", medium = "#f9df86", high = "#f7aaa4"),
      labels = metric_label(c("low", "medium", "high"), lang), name = tr("viral_activity_level", lang)) +
    ggplot2::scale_color_manual(values = stats::setNames(c("#172b3a", "#e31a1c", "#087eae", "#9c2f2f"),
      unname(labels[c("raw", "winsorized", "smooth", "truncated")])), name = NULL) +
    ggplot2::scale_x_date(limits = c(start_date, end_date), expand = ggplot2::expansion(mult = c(.005, .01))) +
    ggplot2::scale_y_continuous(limits = c(0, y_limit), labels = scales::label_number()) +
    ggplot2::labs(x = tr("sample_date", lang),
      y = paste0(tr("wastewater_concentration", lang), " (", units, ")")) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank())
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
            "2 years" = 730L, "ALL" = 0L), selected = 183L, inline = TRUE))),
    shiny::div(class = "metric-cards",
      shiny::uiOutput(ns("trend_card")), shiny::uiOutput(ns("pc_card")),
      shiny::uiOutput(ns("level_card"))),
    shiny::div(class = "city-pathogen-plot",
      plotly::plotlyOutput(ns("plot"), height = "560px"))
  )
}

mod_city_pathogen_server <- function(id, model, lang, config) {
  shiny::moduleServer(id, function(input, output, session) {
    output$title <- shiny::renderText(tr("city_pathogen_title", lang()))
    shiny::observe({
      city <- if (!is.null(input$city) && input$city %in% config$reporting_cities) input$city else
        config$city_pathogen$default_city
      shiny::updateSelectInput(session, "city", label = tr("selected_city", lang()),
        choices = config$reporting_cities, selected = city)
      group_choices <- stats::setNames(c("all", "seasonal", "nonseasonal", "respiratory", "gastrointestinal"),
        tr(c("all", "seasonal", "nonseasonal", "respiratory", "gastrointestinal"), lang()))
      group <- if (!is.null(input$group) && input$group %in% unname(group_choices)) input$group else "all"
      shiny::updateSelectInput(session, "group", label = tr("pathogen_group", lang()),
        choices = group_choices, selected = group)
      window_choices <- stats::setNames(unname(config$city_pathogen$windows),
        tr(names(config$city_pathogen$windows), lang()))
      shiny::updateRadioButtons(session, "window", label = tr("lookback_window", lang()),
        choices = window_choices, selected = if (is.null(input$window))
          config$city_pathogen$default_window else input$window, inline = TRUE)
    })
    shiny::observe({
      available <- city_pathogen_group(if (is.null(input$group)) "all" else input$group, config)
      choices <- pathogen_choices(config, lang())
      choices <- choices[unname(choices) %in% available]
      selected <- if (!is.null(input$pathogen) && input$pathogen %in% unname(choices)) input$pathogen else
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
      end_date <- value$report_end
      start_date <- if (is.na(days) || days <= 0L) min(value$data$Collection_Date, na.rm = TRUE) else
        end_date - days + 1L
      plot <- city_pathogen_plot(value$data, input$city, input$pathogen,
        start_date, end_date, config, lang())
      shiny::validate(shiny::need(!is.null(plot), tr("no_plot_data", lang())))
      plotly::ggplotly(plot)
    })
  })
}
