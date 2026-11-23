mod_influenza_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h2(shiny::textOutput(ns("title"))),
    shiny::div(class = "influenza-controls",
      shiny::div(class = "influenza-city",
        shiny::selectInput(ns("city"), label = "", choices = character())),
      shiny::div(class = "influenza-window",
        shiny::radioButtons(ns("window"), label = "", choices = stats::setNames(c(60, 120, 365, 0),
          tr(c("days60", "days120", "days365", "all_history"), "en")), selected = 120, inline = TRUE))),
    plotly::plotlyOutput(ns("plot"), height = "520px")
  )
}

mod_influenza_server <- function(id, model, lang, config) {
  shiny::moduleServer(id, function(input, output, session) {
    output$title <- shiny::renderText(tr("influenza_title", lang()))
    shiny::observe({
      city <- shiny::isolate(if (!is.null(input$city) && input$city %in% config$reporting_cities)
        input$city else "Merced")
      window <- shiny::isolate(if (is.null(input$window)) "120" else input$window)
      shiny::updateSelectInput(session, "city", label = tr("selected_city", lang()),
        choices = config$reporting_cities, selected = city)
      shiny::updateRadioButtons(session, "window", label = tr("window", lang()),
        choices = stats::setNames(c(60, 120, 365, 0), tr(c("days60", "days120", "days365", "all_history"), lang())),
        selected = window, inline = TRUE)
    })
    output$plot <- plotly::renderPlotly({
      value <- model()
      end <- value$analysis_end
      days <- suppressWarnings(as.integer(input$window))
      city <- if (is.null(input$city)) "Merced" else input$city
      start <- if (is.na(days) || days <= 0L) min(value$data$Collection_Date) else end - days + 1L
      plotly::ggplotly(fluA_plot(value$data, city, start, end, lang()))
    })
  })
}
