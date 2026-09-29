source("R/app_init.R")

# Prepare resources
app <- app_init()

# Run dashboard
shiny::shinyApp(
  ui = app_ui(app = app),
  server = function(input, output, session) {
    app_server(
      input = input,
      output = output,
      session = session,
      app = app
    )
  }
)
