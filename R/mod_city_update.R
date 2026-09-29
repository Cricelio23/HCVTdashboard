mod_city_update_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h2(shiny::textOutput(ns("title"))),
    shiny::strong(shiny::textOutput(ns("selector_label"))),
    shiny::div(class = "pathogen-controls",
      shiny::actionButton(ns("all"), label = "", class = "btn-default"),
      shiny::actionButton(ns("seasonal"), label = "", class = "btn-default"),
      shiny::actionButton(ns("nonseasonal"), label = "", class = "btn-default"),
      shiny::actionButton(ns("respiratory"), label = "", class = "btn-default"),
      shiny::actionButton(ns("gastrointestinal"), label = "", class = "btn-default"),
      shiny::actionButton(ns("clear"), label = "", class = "btn-default")),
    shiny::div(class = "pathogen-selector",
      shiny::checkboxGroupInput(ns("pathogens"), label = NULL, choices = character())),
    shiny::h3(shiny::textOutput(ns("pc_title"))), gt::gt_output(ns("pc_table")),
    shiny::h3(shiny::textOutput(ns("trend_title"))), gt::gt_output(ns("trend_table")),
    shiny::uiOutput(ns("legend"))
  )
}

mod_city_update_server <- function(id, model, lang, config) {
  shiny::moduleServer(id, function(input, output, session) {
    output$title <- shiny::renderText(tr("city_title", lang()))
    output$selector_label <- shiny::renderText(tr("selected_pathogens", lang()))
    output$pc_title <- shiny::renderText(tr("pc_level", lang()))
    output$trend_title <- shiny::renderText(tr("trend_level", lang()))
    output$legend <- shiny::renderUI(table_legend_ui(lang()))
    shiny::observe({
      labels <- pathogen_choices(config, lang())
      selected <- shiny::isolate(input$pathogens)
      if (is.null(selected)) selected <- config$selected
      shiny::updateCheckboxGroupInput(session, "pathogens",
        label = NULL, choices = labels,
        selected = intersect(selected, unname(labels)))
      for (key in c("all", "seasonal", "nonseasonal", "respiratory", "gastrointestinal", "clear")) {
        shiny::updateActionButton(session, key, label = tr(key, lang()))
      }
    })
    shiny::observeEvent(input$all, shiny::updateCheckboxGroupInput(session, "pathogens",
      selected = unname(pathogen_choices(config, lang()))))
    shiny::observeEvent(input$seasonal, shiny::updateCheckboxGroupInput(session, "pathogens",
      selected = intersect(config$seasonal, names(config$pathogens))))
    shiny::observeEvent(input$nonseasonal, shiny::updateCheckboxGroupInput(session, "pathogens",
      selected = setdiff(names(config$pathogens), config$seasonal)))
    shiny::observeEvent(input$respiratory, shiny::updateCheckboxGroupInput(session, "pathogens",
      selected = intersect(config$respiratory, names(config$pathogens))))
    shiny::observeEvent(input$gastrointestinal, shiny::updateCheckboxGroupInput(session, "pathogens",
      selected = intersect(config$gastrointestinal, names(config$pathogens))))
    shiny::observeEvent(input$clear, shiny::updateCheckboxGroupInput(session, "pathogens", selected = character()))
    output$pc_table <- gt::render_gt({
      value <- model()
      selected <- if (is.null(input$pathogens)) character() else input$pathogens
      build_metric_table(value$city_metrics, selected,
        config$reporting_cities, c("pc", "level"), config, lang(), "pc_level")
    })
    output$trend_table <- gt::render_gt({
      value <- model()
      selected <- if (is.null(input$pathogens)) character() else input$pathogens
      build_metric_table(value$city_metrics, selected,
        config$reporting_cities, c("trend", "level"), config, lang(), "trend_level")
    })
  })
}
