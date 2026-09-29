app_ui <- function(app) {
  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
      title = shiny::tags$img(src = "main_logo.svg", class = "header-main-logo",
        alt = "Healthy Central Valley Together"),
      shiny::tags$li(class = "dropdown header-title-menu",
        shiny::div(class = "header-title-block",
          shiny::h1(shiny::textOutput("dashboard_title")),
          shiny::tags$a(href = "https://healthycvtogether.org/data/",
            "https://healthycvtogether.org/data/", target = "_blank", rel = "noopener"),
          shiny::uiOutput("header_dates"))),
      shiny::tags$li(class = "dropdown language-menu",
        shiny::div(class = "header-tools",
          shiny::div(class = "language-toggle",
            shiny::radioButtons("language", label = NULL,
              choices = c("EN" = "en", "ES" = "es"), selected = "en", inline = TRUE)),
          shiny::actionButton("refresh_data", label = NULL, icon = shiny::icon("refresh"),
            class = "btn btn-default refresh-data-button", title = "Update data")
          )
        )
      ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenuOutput("navigation")
    ),
    shinydashboard::dashboardBody(
      shiny::tags$head(shiny::tags$style(shiny::HTML(" 
        html, body, .wrapper, .content-wrapper, .right-side, section.content {
          min-height: 100%; background: #edf2f7 !important;
        }
        .main-header { display: flex; flex-wrap: nowrap; align-items: stretch;
          height: auto !important; max-height: none !important; }
        .main-header .logo { display: flex; align-items: center; justify-content: center;
          flex: 0 0 250px; width: 250px !important; min-height: 122px; height: auto;
          line-height: normal; padding: 10px;
          background: #fff !important; color: #004976 !important; font-size: 25px; font-weight: 800; }
        .header-main-logo { display: block; width: 230px; max-height: 56px; object-fit: contain; }
        .main-header .navbar { display: flex; align-items: center; flex: 1 1 auto;
          min-width: 0; min-height: 122px; height: auto; margin-left: 0 !important;
          background: #fff !important; }
        .main-header .navbar .sidebar-toggle { display: none !important; }
        .main-header .navbar-custom-menu { float: none !important; position: static;
          width: 100%; height: auto; }
        .main-header .navbar-custom-menu > .navbar-nav { display: grid; float: none;
          grid-template-columns: minmax(0, 1fr) auto; align-items: center; width: 100%; }
        .header-title-menu { position: static !important; float: none !important;
          grid-column: 1; grid-row: 1; min-width: 0; }
        .header-title-block { position: static; width: 100%; height: auto; padding: 8px 12px;
          text-align: center; color: #004976; }
        .header-title-block h1 { margin: 0 0 3px; color: #004976; font-size: clamp(20px, 2vw, 27px);
          line-height: 1.15; font-weight: 700; overflow-wrap: anywhere; }
        .header-title-block a { color: #287ca4; font-size: 17px; }
        .header-title-block > .shiny-html-output { display: flex; flex-direction: column;
          align-items: center; gap: 2px; margin-top: 4px; color: #004976; font-size: 13px; }
        .header-primary-dates { display: flex; justify-content: center; gap: 8px 22px;
          flex-wrap: wrap; }
        .header-source-dates { display: flex; justify-content: center; gap: 8px 22px;
          flex-wrap: wrap; white-space: nowrap; }
        .header-source-dates strong { margin-right: 4px; }
        .language-menu { display: flex !important; align-items: center; justify-content: center;
          flex-wrap: wrap; grid-column: 2; grid-row: 1; float: none !important;
          gap: 12px; height: auto; padding: 10px 16px; color: #004976; }
        .header-org-logo { display: block; max-height: 48px; width: auto; max-width: 155px; }
        .uc-merced-logo { max-width: 92px; }
        .uc-davis-logo { max-width: 155px; }
        .sophia { max-width: 50px; }
        .sophia { filter: brightness(0) saturate(100%) invert(20%) sepia(54%) saturate(1000%)
          hue-rotate(160deg) brightness(91%) contrast(105%); }
        .language-menu .form-group { margin: 0; }
        .language-toggle .shiny-input-container { width: auto; }
        .language-toggle .shiny-options-group { display: flex; align-items: center; }
        .language-toggle .radio { margin: 0; }
        .language-toggle .radio + .radio { border-left: 1px solid #004976; }
        .language-toggle .radio label { margin: 0; padding: 1px 8px; color: #004976;
          font-size: 13px; font-weight: 800; cursor: pointer; }
        .language-toggle input[type=radio] { position: absolute; opacity: 0; }
        .header-tools { display: flex; flex-direction: column; align-items: center; gap: 5px; }
        .refresh-data-button { height: 34px; color: #004976 !important; background: #fff !important;
          border: 1px solid #004976 !important; border-radius: 5px; font-weight: 800; }
        .refresh-data-button:hover { background: #edf6fb !important; }
        .main-sidebar, .left-side { position: relative !important; top: auto !important;
          left: auto !important; width: 100%; height: 49px; min-height: 49px !important; padding-top: 0;
          overflow: visible; background: #fff; box-shadow: none; border-top: 1px solid #e3ebf0; }
        .sidebar { height: auto !important; padding-bottom: 0; overflow: visible; }
        .sidebar-menu { display: flex; align-items: center; padding: 0 10px; background: transparent; }
        .sidebar-menu > li { width: auto; }
        .sidebar-menu > li > a { padding: 14px 17px; color: #004976; font-weight: 700; white-space: nowrap; }
        .sidebar-menu > li > a:hover { color: #003b5c; background: #f1f7fa; }
        .sidebar-menu > li.active > a { margin: 7px 2px; padding: 8px 15px;
          color: #004976 !important; background: #eaf4f9 !important; border-bottom: 2px solid #287ca4;
          border-radius: 4px 4px 0 0; }
        .content-wrapper, .right-side { display: block !important; visibility: visible !important;
          min-height: 100vh; margin-left: 0 !important; padding-top: 0;
          background: #edf2f7 !important; }
        .main-footer { margin-left: 0 !important; }
        section.content, .content { display: block !important; visibility: visible !important;
          min-height: 60vh; padding: 20px 22px 40px; color: #17324a; }
        .tab-content > .tab-pane.active { display: block !important; visibility: visible !important; }
        .data-status { margin: 0 0 18px; padding: 11px 16px; border: 0; border-radius: 6px;
          box-shadow: 0 2px 8px rgba(23,55,78,.08); }
        .pathogen-selector, .pathogen-selector .form-group, .pathogen-selector .shiny-options-group {
          width: 100%; max-width: 100%; }
        .pathogen-selector .shiny-options-group { display: flex; flex-direction: row;
          flex-wrap: wrap; align-items: flex-start; gap: 9px 10px; }
        .pathogen-selector .checkbox { flex: 0 0 auto; width: auto; margin: 0 !important; padding: 0; }
        .pathogen-selector .checkbox label { display: inline-flex; align-items: center; gap: 8px;
          min-height: 42px; padding: 7px 15px; border: 1px solid #b9cbd8; border-radius: 24px;
          color: #17415c; background: #fff; font-weight: 650; cursor: pointer; }
        .pathogen-selector .checkbox label:has(input:checked) { color: #fff; background: #087eae; border-color: #087eae; }
        .pathogen-selector .checkbox label:has(input:focus-visible) { outline: 3px solid #f3b63f; outline-offset: 2px; }
        .pathogen-selector .checkbox input { position: absolute; opacity: 0; width: 1px; height: 1px; }
        .pathogen-selector .checkbox label:before { content: ''; display: inline-flex; align-items: center;
          justify-content: center; width: 23px; height: 23px; flex: 0 0 23px; border-radius: 50%;
          color: #087eae; background: #fff; border: 1px solid #cad8e2; }
        .pathogen-selector .checkbox label:has(input:checked):before { content: '✓'; border-color: #fff; }
        .pathogen-controls { display: flex; flex-wrap: wrap; gap: 8px; margin: 8px 0 15px; }
        .pathogen-controls .btn { border-color: #c3d1dc; border-radius: 5px; padding: 8px 14px;
          color: #173b55; background: #fff; font-weight: 600; }
        .pathogen-controls .btn:hover { background: #e4f2f8; }
        .influenza-controls { display: flex; align-items: flex-end; flex-wrap: wrap;
          gap: 10px 24px; margin-bottom: 12px; }
        .influenza-city { flex: 0 0 240px; }
        .influenza-city .form-group { margin-bottom: 0; }
        .influenza-window { flex: 1 1 520px; min-width: 0; }
        .influenza-window .form-group { margin-bottom: 0; }
        .influenza-window .shiny-options-group { display: flex; flex-wrap: wrap;
          align-items: center; gap: 8px 20px; }
        .influenza-window .radio { margin: 0 !important; }
        .summary-county { margin: 22px 0; padding: 22px 24px; background: #fff; border-radius: 8px;
          box-shadow: 0 3px 12px rgba(23,55,78,.08); }
        .summary-county h3 { margin: 0 0 18px; color: #173c59; font-size: 24px; }
        .pathogen-summary { padding: 10px 0; border-bottom: 1px solid #e8edf1; line-height: 1.55; }
        .pathogen-summary:last-child { border-bottom: 0; }
        .summary-change { font-weight: 700; white-space: nowrap; }
        .summary-change.up { color: #c93432; }
        .summary-change.down { color: #27824d; }
        .summary-change.flat { color: #506778; }
        .info-card { height: 100%; margin: 12px 0; padding: 22px; border-radius: 8px;
          background: #fff; box-shadow: 0 3px 12px rgba(23,55,78,.08); }
        .info-card h3 { margin-top: 0; color: #173c59; }
        .math-equation { overflow-x: auto; padding: 12px 0; text-align: center; }
        .table-legend { margin: 14px 0 20px; padding: 12px 16px; border: 1px solid #cbd6df;
          border-radius: 6px; background: #fff; }
        .table-legend > summary { display: flex; align-items: center; justify-content: space-between;
          color: #173c59; font-weight: 700; cursor: pointer; list-style: none; }
        .table-legend > summary::-webkit-details-marker { display: none; }
        .table-legend-chevron { color: #287ca4; font-size: 18px; line-height: 1; }
        .table-legend[open] .table-legend-chevron { transform: rotate(180deg); }
        .table-legend-content { max-height: 280px; overflow-y: auto; padding: 8px 4px 0; }
        .table-legend-content h4 { margin: 12px 0 6px; color: #173c59; font-weight: 700; }
        .table-legend-content p { margin: 6px 0; }
        .partner-logo-footer { display: flex; align-items: center; justify-content: center;
          flex-wrap: wrap; gap: 32px; width: 100%; min-height: 96px; margin-top: 28px;
          padding: 18px 24px; background: #fff; border-top: 1px solid #dce6ed; }
        .partner-logo-footer a { display: inline-flex; align-items: center; justify-content: center; }
        .partner-logo-footer img { display: block; width: auto; max-width: 185px; max-height: 54px;
          object-fit: contain; }
        .partner-logo-footer .sophia { filter: brightness(0) saturate(100%) invert(20%) sepia(54%)
          saturate(1500%) hue-rotate(160deg) brightness(91%) contrast(105%); }
        @media (max-width: 1000px) {
          .main-header { flex-wrap: wrap; }
          .main-header .logo { flex: 0 0 100%; width: 100% !important; min-height: 58px;
            justify-content: flex-start; height: auto; padding: 4px 12px; }
          .header-main-logo { width: 210px; max-height: 42px; }
          .main-header .navbar { flex: 0 0 100%; width: 100%; min-height: 0; height: auto; }
          .main-header .navbar-custom-menu > .navbar-nav { grid-template-columns: minmax(0, 1fr);
            grid-template-rows: auto auto; }
          .language-menu { grid-column: 1; grid-row: 1; gap: 8px; padding: 7px 10px; }
          .header-org-logo { max-width: 115px; max-height: 35px; }
          .uc-merced-logo { max-width: 72px; }
          .uc-davis-logo { max-width: 115px; }
          .sophia { max-width: 88px; }
          .partner-logo-footer { gap: 18px; padding: 14px 10px; }
          .partner-logo-footer img { max-width: 140px; max-height: 42px; }
          .header-title-menu { grid-column: 1; grid-row: 2; }
          .header-title-block { padding: 2px 10px 10px; }
          .header-title-block h1 { font-size: clamp(18px, 4vw, 24px); }
          .header-title-block a { font-size: 14px; }
          .header-title-block > .shiny-html-output { gap: 2px; margin-top: 3px; font-size: 11px; }
          .header-source-dates { gap: 2px 10px; white-space: normal; }
          .main-sidebar, .left-side { height: 48px; min-height: 48px !important; }
          .sidebar-menu { overflow-x: auto; }
          .sidebar-menu > li > a { padding: 14px 11px; font-size: 12px; }
          .content-wrapper, .right-side { padding-top: 0; }
          .content { padding: 15px 12px 30px; }
          .pathogen-selector .checkbox label { min-height: 39px; padding: 6px 11px; }
        }
      "))),
      shinydashboard::tabItems(
        shinydashboard::tabItem("county", mod_county_update_ui("county")),
        shinydashboard::tabItem("city", mod_city_update_ui("city")),
        shinydashboard::tabItem("summary", mod_summary_ui("summary")),
        shinydashboard::tabItem("influenza", mod_influenza_ui("influenza")),
        shinydashboard::tabItem("home", mod_home_ui("home"))
      ),
      shiny::div(class = "partner-logo-footer",
        shiny::tags$a(href = "https://www.ucmerced.edu/", target = "_blank",
          rel = "noopener noreferrer", title = "UC Merced",
          shiny::tags$img(src = "ucmlogo.jpg", alt = "UC Merced")),
        shiny::tags$a(href = "https://www.ucdavis.edu/", target = "_blank",
          rel = "noopener noreferrer", title = "UC Davis",
          shiny::tags$img(src = "uc-logo-gold.png", alt = "UC Davis")),
        shiny::tags$a(href = "https://sofiaresearchteam.org/home", target = "_blank",
          rel = "noopener noreferrer", title = "Sofia Research Team",
          shiny::tags$img(src = "sophia.svg", class = "sophia", alt = "Sofia Research Team")))
    ),
    title = "Healthy Central Valley Together Wastewater Dashboard"
  )
  if (app$auth_enabled) ui <- shinymanager::secure_app(ui)
  ui
}
