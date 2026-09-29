app_server <- function(input, output, session, app) {
  cfg <- app$cfg
  if (app$auth_enabled) {
    shinymanager::secure_server(check_credentials = shinymanager::check_credentials(
      data.frame(user = app$auth_user, password = app$auth_password, stringsAsFactors = FALSE)))
  }
  lang <- shiny::reactive(if (identical(input$language, "es")) "es" else "en")
  state <- shiny::reactiveValues(model = app$model, error = app$error,
    params = app$params, updated_at = app$updated_at, source = app$source)
  model <- shiny::reactive({
    shiny::validate(shiny::need(is.null(state$error), tr("load_error", lang())))
    state$model
  })
  output$dashboard_title <- shiny::renderText(tr("dashboard_title", lang()))
  output$header_dates <- shiny::renderUI({
    params <- state$params
    date_text <- function(x) {
      if (is.null(x) || length(x) != 1L || is.na(x)) return("NA")
      format(as.Date(x), "%m/%d/%Y")
    }
    compiled <- if (is.null(params$compiled_on)) Sys.Date() else params$compiled_on
    scan <- if (is.null(params$last_scan_sample)) as.Date(NA) else params$last_scan_sample
    modesto <- if (is.null(params$last_modesto_sample)) as.Date(NA) else params$last_modesto_sample
    update_time <- state$updated_at
    update_label <- if (is.null(update_time) || length(update_time) != 1L || is.na(update_time)) {
      paste0(tr("last_update", lang()), ": ", tr("no_update", lang()))
    } else {
      paste0(tr("last_update", lang()), ": ", format(update_time, "%m/%d/%Y %H:%M"))
    }
    shiny::tagList(
      shiny::div(class = "header-primary-dates",
        shiny::span(shiny::tags$strong(tr("compiled_on", lang())), date_text(compiled)),
        shiny::span(update_label)),
      shiny::div(class = "header-source-dates",
        shiny::span(shiny::tags$strong(tr("last_scan_sample", lang())), date_text(scan)),
        shiny::span(shiny::tags$strong(tr("last_modesto_sample", lang())), date_text(modesto)))
    )
  })
  shiny::observe({
    shiny::updateActionButton(session, "refresh_data", label = tr("refresh_data", lang()))
  })
  shiny::observeEvent(input$refresh_data, {
    shiny::showNotification(tr("updating_data", lang()), id = "data-refresh",
      duration = NULL, type = "message")
    refreshed <- read_all_data(config = cfg, force_refresh = TRUE)
    shiny::removeNotification("data-refresh")
    if (is.null(refreshed$error) && !is.null(refreshed$model) &&
        (is.null(refreshed$refresh_error) || identical(refreshed$source, "live"))) {
      state$model <- refreshed$model
      state$error <- NULL
      state$params <- refreshed$params
      state$updated_at <- refreshed$updated_at
      state$source <- refreshed$source
      message_key <- if (is.null(refreshed$refresh_error)) "update_success" else "update_success_cache_failed"
      shiny::showNotification(tr(message_key, lang()),
        type = if (is.null(refreshed$refresh_error)) "message" else "warning")
    } else if (is.null(refreshed$error) && identical(refreshed$source, "cache")) {
      state$model <- refreshed$model
      state$error <- NULL
      state$params <- refreshed$params
      state$updated_at <- refreshed$updated_at
      state$source <- refreshed$source
      shiny::showNotification(tr("update_failed_cached", lang()), type = "warning")
    } else {
      shiny::showNotification(tr(if (is.null(state$model)) "update_failed" else "update_failed_cached", lang()),
        type = "error")
    }
  }, ignoreInit = TRUE)
  output$navigation <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(id = "navigation_tab", selected = "county",
      shinydashboard::menuItem(tr("county_title", lang()), tabName = "county", icon = shiny::icon("table")),
      shinydashboard::menuItem(tr("city_title", lang()), tabName = "city", icon = shiny::icon("city")),
      shinydashboard::menuItem(tr("summary_title", lang()), tabName = "summary", icon = shiny::icon("list")),
      shinydashboard::menuItem(tr("influenza_title", lang()), tabName = "influenza", icon = shiny::icon("chart-line")),
      shinydashboard::menuItem(tr("home_title", lang()), tabName = "home", icon = shiny::icon("house")))
  })
  mod_county_update_server("county", model, lang, cfg)
  mod_city_update_server("city", model, lang, cfg)
  mod_summary_server("summary", model, lang, cfg)
  mod_influenza_server("influenza", model, lang, cfg)
  mod_home_server("home", model, lang, cfg)
}
