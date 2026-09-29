mod_county_update_ui <- function(id) {
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
    gt::gt_output(ns("table")),
    shiny::uiOutput(ns("legend"))
  )
}

mod_county_update_server <- function(id, model, lang, config) {
  shiny::moduleServer(id, function(input, output, session) {
    output$title <- shiny::renderText(tr("county_title", lang()))
    output$selector_label <- shiny::renderText(tr("selected_pathogens", lang()))
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
    output$table <- gt::render_gt({
      value <- model()
      selected <- if (is.null(input$pathogens)) character() else input$pathogens
      build_metric_table(value$county_metrics, selected,
        names(config$counties), c("trend", "pc", "level"), config, lang(), "county_metrics")
    })
  })
}
