app_server <- function(input, output, session, app) {
  cfg <- app$cfg
  if (app$auth_enabled) {
    shinymanager::secure_server(check_credentials = shinymanager::check_credentials(
      data.frame(user = app$auth_user, password = app$auth_password, stringsAsFactors = FALSE)))
  }
  lang <- shiny::reactive(if (identical(input$language, "es")) "es" else "en")
  # This state is created once in app_init(), so every session in this R process
  # sees the same refreshed model and update timestamp.
  state <- app$state
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
      paste0(tr("last_update", lang()), ": ",
        format(update_time, "%m/%d/%Y %H:%M %Z", tz = cfg$data_refresh$timezone))
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

  worker_sources <- normalizePath(file.path("R", c("01_config.R", "i18n.R",
    "app_utils.R", "utils_percentiles.R", "utils_wval.R", "dashboard_export/ww_trend.R",
    "ww_publication_store.R", "02_read_data.R")), winslash = "/", mustWork = FALSE)
  apply_refresh <- function(refreshed, today) {
    valid <- is.null(refreshed$error) && !is.null(refreshed$model)

    if (valid && identical(refreshed$source, "live")) {
      state$last_daily_check <- today
      state$model <- refreshed$model
      state$error <- NULL
      state$params <- refreshed$params
      state$updated_at <- refreshed$updated_at
      state$source <- refreshed$source
      state$refresh_error <- refreshed$refresh_error
      return(if (is.null(refreshed$refresh_error)) "success" else "cache_failed")
    }

    state$refresh_error <- if (!is.null(refreshed$refresh_error)) refreshed$refresh_error else refreshed$error
    if (is.null(shiny::isolate(state$model)) && valid) {
      state$model <- refreshed$model
      state$error <- NULL
      state$params <- refreshed$params
      state$updated_at <- refreshed$updated_at
      state$source <- refreshed$source
    }
    if (valid && identical(refreshed$source, "cache")) "cached" else "failed"
  }

  # Returns a promise that resolves to the refresh status string.
  refresh_shared_data <- function(force = FALSE) {
    now <- Sys.time()
    today <- as.Date(now, tz = cfg$data_refresh$timezone)
    last_daily_check <- shiny::isolate(state$last_daily_check)
    due <- is.null(last_daily_check) || length(last_daily_check) != 1L ||
      is.na(last_daily_check) || !identical(as.Date(last_daily_check), today)
    # After a failed automatic attempt, retry at most every 30 minutes.
    last_attempt <- shiny::isolate(state$last_attempt)
    cooling_down <- !is.null(last_attempt) && length(last_attempt) == 1L && !is.na(last_attempt) &&
      difftime(now, last_attempt, units = "mins") < 30
    if (!force && (!due || cooling_down)) return(promises::promise_resolve("not_due"))
    if (isTRUE(shiny::isolate(state$refreshing))) return(promises::promise_resolve("in_progress"))

    state$refreshing <- TRUE
    state$last_attempt <- now
    # The download runs in a background R process so it never blocks any session.
    worker <- promises::future_promise({
      for (path in worker_sources) sys.source(path, envir = environment())
      read_all_data(config = cfg, force_refresh = TRUE)
    }, globals = list(worker_sources = worker_sources, cfg = cfg), seed = NULL)
    promises::then(worker,
      onFulfilled = function(refreshed) {
        state$refreshing <- FALSE
        apply_refresh(refreshed, today)
      },
      onRejected = function(error) {
        state$refreshing <- FALSE
        state$refresh_error <- conditionMessage(error)
        "failed"
      })
  }

  # A new session checks once. The shared date ensures that only the first session
  # opened on a given Pacific Time day attempts the automatic download.
  shiny::observeEvent(TRUE, {
    shiny::isolate(refresh_shared_data(force = FALSE))
    update_time <- shiny::isolate(state$updated_at)
    formatted_time <- if (is.null(update_time) || length(update_time) != 1L || is.na(update_time)) {
      tr("no_update", lang())
    } else {
      format(update_time, "%m/%d/%Y %H:%M %Z", tz = cfg$data_refresh$timezone)
    }
    shiny::showNotification(
      shiny::div(class = "startup-update-message", shiny::icon("clock"),
        shiny::span(sprintf(tr("startup_update_notice", lang()), formatted_time,
          tr("refresh_data", lang())))),
      id = "startup-update-notice", type = "message", duration = 12, closeButton = TRUE
    )
  }, once = TRUE)

  shiny::observeEvent(input$refresh_data, {
    shiny::showNotification(tr("updating_data", lang()), id = "data-refresh",
      duration = NULL, type = "message")
    promises::then(refresh_shared_data(force = TRUE), onFulfilled = function(result) {
      shiny::removeNotification("data-refresh")
      if (identical(result, "success")) {
        shiny::showNotification(tr("update_success", lang()), type = "message")
      } else if (identical(result, "cache_failed")) {
        shiny::showNotification(tr("update_success_cache_failed", lang()), type = "warning")
      } else if (identical(result, "cached")) {
        shiny::showNotification(tr("update_failed_cached", lang()), type = "warning")
      } else if (identical(result, "in_progress")) {
        shiny::showNotification(tr("updating_data", lang()), type = "message")
      } else {
        shiny::showNotification(tr(if (is.null(shiny::isolate(state$model))) "update_failed" else "update_failed_cached",
          lang()), type = "error")
      }
    })
  }, ignoreInit = TRUE)
  output$navigation <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(id = "navigation_tab", selected = "city",
      shinydashboard::menuItem(tr("city_title", lang()), tabName = "city", icon = shiny::icon("city")),
      shinydashboard::menuItem(tr("county_title", lang()), tabName = "county", icon = shiny::icon("table")),
      shinydashboard::menuItem(tr("city_pathogen_title", lang()), tabName = "city_pathogen",
        icon = shiny::icon("chart-line")),
      shinydashboard::menuItem(tr("summary_title", lang()), tabName = "summary", icon = shiny::icon("list")),
      shinydashboard::menuItem(tr("influenza_title", lang()), tabName = "influenza", icon = shiny::icon("chart-line")),
      shinydashboard::menuItem(tr("home_title", lang()), tabName = "home", icon = shiny::icon("house")))
  })
  mod_county_update_server("county", model, lang, cfg)
  mod_city_update_server("city", model, lang, cfg)
  mod_summary_server("summary", model, lang, cfg)
  mod_influenza_server("influenza", model, lang, cfg)
  mod_city_pathogen_server("city_pathogen", model, lang, cfg)
  mod_home_server("home", model, lang, cfg)
}
