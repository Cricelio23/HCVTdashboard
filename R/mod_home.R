mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h2(shiny::textOutput(ns("title"))),
    shiny::div(class = "info-card", shiny::h3(shiny::textOutput(ns("background_title"))),
      shiny::p(shiny::textOutput(ns("intro"))),
      shiny::strong(shiny::textOutput(ns("period_title"))), " ",
      shiny::textOutput(ns("period"))),
    shiny::uiOutput(ns("methods"))
  )
}

mod_home_server <- function(id, model, lang, config) {
  shiny::moduleServer(id, function(input, output, session) {
    output$title <- shiny::renderText(tr("home_title", lang()))
    output$intro <- shiny::renderText(tr("home_intro", lang()))
    output$background_title <- shiny::renderText(tr("info_background", lang()))
    output$period_title <- shiny::renderText(tr("report_period", lang()))
    output$period <- shiny::renderText({
      value <- model()
      paste(format(value$report_end - 13L), "—", format(value$report_end))
    })
    output$methods <- shiny::renderUI({
      text <- method_text(lang(), config)
      card <- function(title_key, paragraphs) shiny::div(class = "info-card",
        shiny::h3(tr(title_key, lang())), lapply(paragraphs, shiny::tags$p))
      data_card <- shiny::div(class = "info-card",
        shiny::h3(tr("info_data", lang())),
        shiny::tags$p(text$data),
        shiny::div(class = "math-equation", shiny::HTML(text$equation)))
      shiny::withMathJax(shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, card("info_pc", text$pc)),
          shiny::column(6, card("info_level", text$level))),
        shiny::fluidRow(
          shiny::column(6, card("info_trend", text$trend)),
          shiny::column(6, card("info_rsi", text$rsi))),
        shiny::fluidRow(
          shiny::column(6, card("info_slope_pc", text$slope_pc)),
          shiny::column(6, card("info_criteria", text$criteria))),
        shiny::fluidRow(shiny::column(12, data_card))
      ))
    })
  })
}
