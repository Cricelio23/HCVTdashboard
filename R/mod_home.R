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
      wlevel_description <- if (identical(lang(), "es")) paste(
        "PBWL clasifica la actividad en aguas residuales como Muy bajo, Bajo, Moderado, Alto o Muy alto.",
        "Las concentraciones se winsorizan al percentil 95 de las observaciones de los 21 días previos, incluido el día de muestreo, y se suavizan mediante una media geométrica móvil de 10 días, calculada como exp_mean_log = function(x) exp(mean(log(x), na.rm = T)).",
        "Solo las fechas con observaciones contribuyen a los percentiles históricos.",
        "El último valor suavizado disponible en los últimos 7 días se compara con una ventana histórica de 24 meses que termina 10 días antes de la fecha de referencia.",
        "Los cortes de las categorías son P40/P60/P90/P99 para COVID-19 y otros patógenos no estacionales; P60/P80/P95/P99.5 para influenza A y sus subtipos; P60/P80/P90/P99 para VRS; y P60/P80/P90/P99.5 para otros patógenos estacionales.",
        "Los valores iguales a un corte permanecen en la categoría inferior.",
        "Las referencias se actualizan el 1 de abril y el 1 de octubre para COVID-19, el 1 de agosto para influenza A y VRS, y con cada última muestra para los demás patógenos.",
        "La clasificación requiere al menos 3 días de muestreo positivos en 14 días, incluido uno en 7 días; de lo contrario, se asigna Muy bajo cuando existen observaciones recientes.",
        "La ausencia de observaciones recientes, de un valor suavizado o de datos históricos suficientes produce NA.",
        "PBWL del condado utiliza la serie ponderada por población. PBWL es una medida local, distinta del WVAL del CDC."
      ) else paste(
        "PBWL classifies wastewater activity as Very low, Low, Moderate, High, or Very high.",
        "Concentrations are winsorized at the 95th percentile of observations within the preceding 21 days, including the sampling day, and smoothed using a 10-day moving geometric mean, calculated as exp_mean_log = function(x) exp(mean(log(x), na.rm = T)).",
        "Only observed sampling dates contribute to the historical percentiles.",
        "The latest smoothed value available within the last 7 days is compared with a 24-month historical window ending 10 days before the reference date.",
        "Category cutoffs are P40/P60/P90/P99 for COVID-19 and other non-seasonal pathogens; P60/P80/P95/P99.5 for influenza A and its subtypes; P60/P80/P90/P99 for RSV; and P60/P80/P90/P99.5 for other seasonal pathogens.",
        "Values equal to a cutoff remain in the lower category.",
        "References update on April 1 and October 1 for COVID-19, August 1 for influenza A and RSV, and with each latest sample for other pathogens.",
        "Classification requires at least 3 positive sampling days within 14 days, including one within 7 days; otherwise, recent observations receive Very low.",
        "Missing recent observations, a missing smoothed value, or insufficient historical data yield NA.",
        "County PBWL uses the population-weighted county series. PBWL is a local measure, distinct from CDC WVAL."
      )
      data_card <- shiny::div(class = "info-card",
        shiny::h3(tr("info_data", lang())),
        shiny::tags$p(text$data),
        shiny::div(class = "math-equation", shiny::HTML(text$equation)))
      shiny::withMathJax(shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, card("info_pc", text$pc)),
          shiny::column(6, card("info_trend", text$trend))
          ),
        # shiny::fluidRow(
        #   #shiny::column(6, card("info_slope_pc", text$slope_pc)),
        #   #shiny::column(6, card("info_rsi", text$rsi))
        #   ),
        shiny::fluidRow(
          shiny::column(6, card("info_level", text$level)),
          shiny::column(6, card("info_criteria", text$criteria))),
        shiny::fluidRow(
          shiny::column(6, card("info_wlevel", wlevel_description)),
          shiny::column(6, card("wval", tr("wval_method", lang())))),
        
        shiny::fluidRow(shiny::column(12, data_card))
      ))
    })
  })
}
