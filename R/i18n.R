# Stable keys are used in calculations; translation happens only at rendering.
translations <- list(
  en = c(
    county_title = "General Update at County Level", city_title = "General Update at City Level",
    summary_title = "Percent Change Summary", influenza_title = "Influenza", home_title = "Information",
    dashboard_title = "Healthy Central Valley Together Update", compiled_on = "Last compiled on",
    last_update = "Last successful update", refresh_data = "Update data",
    no_update = "No successful update yet",
    updating_data = "Updating data…", update_success = "Data updated successfully.",
    update_success_cache_failed = "Data updated, but the local cache could not be saved.",
    update_failed_cached = "Update failed. Showing the last successfully saved data.",
    update_failed = "Update failed and no saved data could be loaded.",
    last_scan_sample = "Last SCAN sample", last_modesto_sample = "Last Modesto sample",
    selected_pathogens = "Selected Pathogens", all = "All", seasonal = "Seasonal", nonseasonal = "Non-seasonal",
    respiratory = "Respiratory", gastrointestinal = "Gastrointestinal",
    clear = "Clear", select_pathogen = "Select at least one pathogen.", pathogen = "Pathogen",
    pc = "PC", level = "Level", trend = "Trend", county = "County", city = "City",
    pc_level = "Percent change and current wastewater level", trend_level = "Trend and current wastewater level",
    county_metrics = "Trend, percent change and current wastewater level",
    report_period = "Reporting period", prior_period = "Comparison period", last_sample = "Latest sample",
    snapshot_status = "Using the bundled data snapshot; these are not live data.",
    live_status = "Data downloaded for this process.",
    load_error = "Data could not be loaded. Please contact the dashboard administrator.",
    no_data = "No data available", no_observations = "No observations in this period.",
    selected_city = "Selected city", window = "Look-back window", days60 = "60 days",
    days120 = "120 days", days365 = "365 days", all_history = "All available data",
    sample_date = "Sample collection date", concentration = "Concentration above period minimum",
    flu_title = "Influenza A vs H1 + H3 + H5", flu_sum = "Sum: H1 + H3 + H5",
    series = "Series", about = "About the dashboard", methods = "Calculation methods",
    low = "Low", medium = "Medium", high = "High", sporadic = "Sporadic", nd = "ND",
    info_background = "Background", info_pc = "Percent Change Calculation",
    info_level = "Current Wastewater Level", info_trend = "Trend Test", info_data = "Data and aggregation",
    info_criteria = "Criteria", info_rsi = "The Relative Strength Index",
    info_slope_pc = "Percent change in the last 14 days",
    legend_title = "Table notes and criteria", legend_levels = "Transmission levels",
    summary_numeric = "%1$s's average %2$s concentrations %3$s by %4$s between the most recent two weeks and the two weeks prior to that.",
    summary_up = "increased", summary_down = "decreased", summary_flat = "did not change",
    summary_nd = "%1$s's average %2$s concentrations showed no detections in the most recent two weeks.",
    summary_sporadic = "%1$s's average %2$s concentrations showed sporadic detections in the most recent two weeks.",
    summary_baseline = "%1$s's average %2$s concentrations increased from a low or undetected baseline in the most recent two weeks.",
    summary_missing = "%1$s's average %2$s concentrations do not have enough recent data for a comparison.",
    low_baseline = "+LL", sharply = "+Sharply", no_trend = "No Trend",
    upward = "Upward", likely_upward = "Likely upward", very_likely_upward = "Very likely upward",
    downward = "Downward", likely_downward = "Likely downward", very_likely_downward = "Very likely downward",
    stable = "No percent change", increase = "Increased by", decrease = "Decreased by",
    no_detection = "No detections", summary_intro = "Current two-week period compared with the prior two weeks.",
    mpox_title = "Mpox Clade II update", loading = "Loading data",
    home_intro = "To help prevent the spread of COVID-19, flu, RSV, and other infectious diseases, Healthy Central Valley Together tests wastewater from communities' sewage treatment plants.",
    pathogen_adeno = "Human Adenovirus Group F", pathogen_ev = "Enterovirus D68",
    pathogen_hav = "Hepatitis A Virus", pathogen_rsv = "RSV", pathogen_measles = "Measles",
    pathogen_mpoxi = "Mpox Clade Ib", pathogen_mpoxii = "Mpox Clade II"
  ),
  es = c(
    county_title = "Actualización general por condado", city_title = "Actualización general por ciudad",
    summary_title = "Resumen del cambio porcentual", influenza_title = "Influenza", home_title = "Información",
    dashboard_title = "Actualización de Healthy Central Valley Together", compiled_on = "Última compilación",
    last_update = "Última actualización", refresh_data = "Actualizar datos",
    no_update = "Aún no hay una actualización exitosa",
    updating_data = "Actualizando datos…", update_success = "Datos actualizados correctamente.",
    update_success_cache_failed = "Datos actualizados, pero no se pudo guardar la copia local.",
    update_failed_cached = "Falló la actualización. Se muestran los últimos datos guardados correctamente.",
    update_failed = "Falló la actualización y no se pudieron cargar datos guardados.",
    last_scan_sample = "Última muestra de SCAN", last_modesto_sample = "Última muestra de Modesto",
    selected_pathogens = "Patógenos seleccionados", all = "Todos", seasonal = "Estacionales", nonseasonal = "No estacionales",
    respiratory = "Respiratorios", gastrointestinal = "Gastrointestinales",
    clear = "Limpiar", select_pathogen = "Selecciona al menos un patógeno.", pathogen = "Patógeno",
    pc = "PC", level = "Nivel", trend = "Tendencia", county = "Condado", city = "Ciudad",
    pc_level = "Cambio porcentual y nivel actual en aguas residuales", trend_level = "Tendencia y nivel actual en aguas residuales",
    county_metrics = "Tendencia, cambio porcentual y nivel actual en aguas residuales",
    report_period = "Período de reporte", prior_period = "Período de comparación", last_sample = "Última muestra",
    snapshot_status = "Se usa la copia de datos incluida; no son datos en vivo.",
    live_status = "Datos descargados para este proceso.",
    load_error = "No se pudieron cargar los datos. Contacta al administrador del tablero.",
    no_data = "Sin datos disponibles", no_observations = "No hay observaciones en este período.",
    selected_city = "Ciudad seleccionada", window = "Ventana de análisis", days60 = "60 días",
    days120 = "120 días", days365 = "365 días", all_history = "Todos los datos disponibles",
    sample_date = "Fecha de recolección de la muestra", concentration = "Concentración sobre el mínimo del período",
    flu_title = "Influenza A frente a H1 + H3 + H5", flu_sum = "Suma: H1 + H3 + H5",
    series = "Serie", about = "Acerca del tablero", methods = "Métodos de cálculo",
    low = "Bajo", medium = "Medio", high = "Alto", sporadic = "Esporádico", nd = "ND",
    info_background = "Antecedentes", info_pc = "Cálculo del cambio porcentual",
    info_level = "Nivel actual en aguas residuales", info_trend = "Prueba de tendencia",
    info_data = "Datos y agregación", info_criteria = "Criterios",
    info_rsi = "Índice de fuerza relativa", info_slope_pc = "Cambio porcentual en los últimos 14 días",
    legend_title = "Notas y criterios de las tablas", legend_levels = "Niveles de transmisión",
    summary_numeric = "Las concentraciones promedio de %2$s en %1$s %3$s un %4$s entre las últimas dos semanas y las dos semanas anteriores.",
    summary_up = "aumentaron", summary_down = "disminuyeron", summary_flat = "no cambiaron",
    summary_nd = "No se detectó %2$s en las muestras recientes de %1$s.",
    summary_sporadic = "Las muestras recientes de %1$s mostraron detecciones esporádicas de %2$s.",
    summary_baseline = "Las concentraciones de %2$s en %1$s aumentaron desde un nivel previo bajo o no detectable.",
    summary_missing = "No hay suficientes datos recientes de %2$s en %1$s para calcular el cambio.",
    low_baseline = "+LL", sharply = "+Marcado", no_trend = "Sin tendencia",
    upward = "Ascendente", likely_upward = "Probablemente ascendente", very_likely_upward = "Muy probablemente ascendente",
    downward = "Descendente", likely_downward = "Probablemente descendente", very_likely_downward = "Muy probablemente descendente",
    stable = "Sin cambio porcentual", increase = "Aumentó", decrease = "Disminuyó",
    no_detection = "Sin detecciones", summary_intro = "Período actual de dos semanas comparado con las dos semanas anteriores.",
    mpox_title = "Actualización de Mpox clado II", loading = "Cargando datos",
    home_intro = "Para ayudar a prevenir la propagación de COVID-19, influenza, VRS y otras enfermedades infecciosas, Healthy Central Valley Together analiza las aguas residuales de las plantas comunitarias de tratamiento.",
    pathogen_adeno = "Adenovirus humano grupo F", pathogen_ev = "Enterovirus D68",
    pathogen_hav = "Virus de la hepatitis A", pathogen_rsv = "VRS", pathogen_measles = "Sarampión",
    pathogen_mpoxi = "Mpox clado Ib", pathogen_mpoxii = "Mpox clado II"
  )
)

tr <- function(key, lang = "en") {
  dictionary <- translations[[if (lang %in% names(translations)) lang else "en"]]
  value <- unname(dictionary[key])
  value[is.na(value)] <- key[is.na(value)]
  value
}

pathogen_labels <- function(config, lang) {
  labels <- config$pathogens
  keys <- c(HAdV_F_norm_PMMoV = "pathogen_adeno", EV.D68_norm_PMMoV = "pathogen_ev",
    HAV_norm_PMMoV = "pathogen_hav", RSV_norm_PMMoV = "pathogen_rsv", MeV_norm_PMMoV = "pathogen_measles",
    MPXV_dD14.16_norm_PMMoV = "pathogen_mpoxi", MPXV_G2R_norm_PMMoV = "pathogen_mpoxii")
  labels[names(keys)] <- tr(unname(keys), lang)
  labels
}

pathogen_choices <- function(config, lang) {
  stats::setNames(names(config$pathogens), unname(pathogen_labels(config, lang)))
}

metric_label <- function(key, lang) {
  result <- rep("NA", length(key))
  valid <- !is.na(key)
  result[valid] <- tr(key[valid], lang)
  result
}

format_pc <- function(value, status, lang) {
  result <- rep("NA", length(status))
  numeric_rows <- !is.na(status) & status == "numeric" & is.finite(value)
  result[numeric_rows] <- paste0(ifelse(value[numeric_rows] > 0, "+", ""), format(
    round(value[numeric_rows], 1), trim = TRUE, scientific = FALSE), "%")
  sharp <- numeric_rows & value > 500
  result[sharp] <- tr("sharply", lang)
  special <- !is.na(status) & status %in% c("nd", "sporadic", "low_baseline")
  result[special] <- tr(status[special], lang)
  result
}
