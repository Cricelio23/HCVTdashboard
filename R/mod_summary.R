summary_narrative <- function(metrics, cities, config, lang) {
  labels <- pathogen_labels(config, lang)
  rows <- metrics[metrics$location %in% cities, , drop = FALSE]
  rows <- rows[order(match(rows$pathogen, names(config$pathogens)),
    match(rows$location, cities)), , drop = FALSE]
  if (!nrow(rows)) return(shiny::p(tr("no_data", lang)))
  shiny::tagList(lapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, , drop = FALSE]
    pathogen <- unname(labels[row$pathogen])
    city <- row$location
    if (!is.na(row$pc_status) && row$pc_status == "numeric" && is.finite(row$pc)) {
      direction <- if (row$pc > 0) "summary_up" else if (row$pc < 0) "summary_down" else "summary_flat"
      arrow <- if (row$pc > 0) "↑" else if (row$pc < 0) "↓" else "→"
      change <- shiny::tags$span(class = paste("summary-change",
        if (row$pc > 0) "up" else if (row$pc < 0) "down" else "flat"),
        paste0(format(abs(row$pc), trim = TRUE, nsmall = 1), "% ", arrow))
      sentence <- sprintf(tr("summary_numeric", lang), city, pathogen,
        tr(direction, lang), "@@CHANGE@@")
      parts <- strsplit(sentence, "@@CHANGE@@", fixed = TRUE)[[1]]
      shiny::tags$p(class = "pathogen-summary", shiny::tags$strong(pathogen),
        shiny::tags$br(), parts[1], change, if (length(parts) > 1L) parts[2])
    } else {
      key <- if (identical(row$pc_status, "nd")) "summary_nd" else
        if (identical(row$pc_status, "sporadic")) "summary_sporadic" else
          if (identical(row$pc_status, "low_baseline")) "summary_baseline" else "summary_missing"
      shiny::tags$p(class = "pathogen-summary", shiny::tags$strong(pathogen),
        shiny::tags$br(), sprintf(tr(key, lang), city, pathogen))
    }
  }))
}

mod_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  counties <- c("Merced", "Stanislaus", "Yolo")
  shiny::tagList(
    shiny::h2(shiny::textOutput(ns("title"))),
    shiny::p(shiny::textOutput(ns("intro"))),
    lapply(seq_along(counties), function(i) shiny::div(class = "summary-county",
      shiny::h3(shiny::textOutput(ns(paste0("county_title_", i)))),
      shiny::uiOutput(ns(paste0("county_summary_", i))))),
    shiny::div(class = "summary-county",
      shiny::h3(shiny::textOutput(ns("mpox_title"))),
      shiny::uiOutput(ns("mpox_summary")))
  )
}

mod_summary_server <- function(id, model, lang, config) {
  shiny::moduleServer(id, function(input, output, session) {
    output$title <- shiny::renderText(tr("summary_title", lang()))
    output$intro <- shiny::renderText(tr("summary_intro", lang()))
    for (i in seq_along(config$summary_counties)) local({
      index <- i
      county <- names(config$summary_counties)[index]
      output[[paste0("county_title_", index)]] <- shiny::renderText(
        paste(county, tr("county", lang())))
      output[[paste0("county_summary_", index)]] <- shiny::renderUI({
        value <- model()
        summary_narrative(value$city_metrics, config$summary_counties[[county]], config, lang())
      })
    })
    output$mpox_title <- shiny::renderText(tr("mpox_title", lang()))
    output$mpox_summary <- shiny::renderUI({
      value <- model()
      key <- "MPXV_G2R_norm_PMMoV"
      metrics <- value$city_metrics[value$city_metrics$pathogen == key, , drop = FALSE]
      summary_narrative(metrics, config$reporting_cities, config, lang())
    })
  })
}
