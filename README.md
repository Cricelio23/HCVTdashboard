# HCVT wastewater dashboard

## App layout

- `app.R` assembles the dashboard and connects the section modules.
- `R/app_init.R` loads app code and prepares the app resource object.
- `R/00_packages.R` loads the R packages used by the app. Keep new package dependencies here.
- `R/01_config.R` holds pathogen names, reporting cities, counties, and calculation settings.
- `R/02_read_data.R` downloads, validates, and prepares source data.
- `R/i18n.R` contains English and Spanish labels.
- `R/mod_*.R` contains one Shiny UI/server module for each dashboard section.
- `R/app_utils.R` contains shared metric calculations, model construction, summary helpers, the Influenza plot, and calculation notes.
- `R/app_ui.R` and `R/app_server.R` define the top-level dashboard interface and server.

When adding a section, create `mod_<name>_ui(id)` and `mod_<name>_server(id, ...)`, namespace every module input and output with `shiny::NS(id)`, add translated text to both language dictionaries, and add its source and UI/server calls in `R/app_init.R`, `R/app_ui.R`, and `R/app_server.R`.

## Data settings

The app defaults to `HCVT_DATA_MODE=auto`: it tries the WastewaterSCAN and CDPH sources, then uses the bundled `data.csv` snapshot if the download or source validation fails. Set `HCVT_DATA_MODE=live` to require live data, or `HCVT_DATA_MODE=snapshot` to use only the snapshot. Set `HCVT_DATA_FILE` to use a snapshot at another path.

To enable login on a hosted deployment, set both `HCVT_AUTH_USER` and `HCVT_AUTH_PASSWORD` as deployment environment variables or secrets. Leave both unset to run without the optional login screen. Do not commit credentials.

For Posit Connect Cloud, deploy the project directory containing `app.R`, `R/`, `www/`, and `data.csv`. The hosting environment needs outbound access to the configured data sources when live data is enabled. Package dependencies are listed in `R/00_packages.R`; install or restore them in the deployment environment rather than at app startup.
