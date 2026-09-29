# Initialize packages, configuration, helpers, modules, and the shared data model.
app_init <- function() {
  target_env <- parent.frame()
  sources <- c(
    "R/00_packages.R",
    "R/01_config.R",
    "R/i18n.R",
    "R/app_utils.R",
    "R/02_read_data.R",
    "R/mod_county_update.R",
    "R/mod_city_update.R",
    "R/mod_summary.R",
    "R/mod_influenza.R",
    "R/mod_home.R",
    "R/app_ui.R",
    "R/app_server.R"
  )
  for (path in sources) sys.source(path, envir = target_env)

  base_config <- target_env$app_config()
  data_list <- target_env$read_all_data(config = base_config)
  cfg <- target_env$get_config(params = data_list$params, config = base_config)
  auth_user <- Sys.getenv("HCVT_AUTH_USER")
  auth_password <- Sys.getenv("HCVT_AUTH_PASSWORD")
  if (xor(nzchar(auth_user), nzchar(auth_password))) {
    stop("Set both HCVT_AUTH_USER and HCVT_AUTH_PASSWORD, or neither.")
  }

  list(
    cfg = cfg,
    data = data_list$data,
    params = data_list$params,
    updated_at = data_list$updated_at,
    refresh_error = data_list$refresh_error,
    i18n = data_list$i18n,
    model = data_list$model,
    source = data_list$source,
    error = data_list$error,
    auth_enabled = nzchar(auth_user) && nzchar(auth_password),
    auth_user = auth_user,
    auth_password = auth_password
  )
}
