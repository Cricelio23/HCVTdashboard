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
    wlevel = "PBWL", very_low = "Very low", moderate = "Moderate", very_high = "Very high",
    info_wlevel = "Percentile-based wastewater level (PBWL)",
    wval = "WVAL", level_comparison = "Comparison: Level, PBWL and WVAL",
    comparison_note = "Level has three categories; PBWL and WVAL have five. WVAL is calculated locally from unnormalized samples and is not an official CDC result. The county median is a local extension and requires all constituent cities. Other pathogens: Not applicable.",
    concentration_units = "All metrics use unnormalized concentrations. WastewaterSCAN cities use gc/g dry weight; Modesto uses CDPH copies/L. Values and thresholds are calculated separately within each site and must not be compared numerically across those units.",
    wval_method = "Local WVAL assumes one continuous laboratory method per site. Zeros are replaced with half the smallest positive reference value; NA remains missing. It uses log concentrations, P99 winsorization, a P10 baseline and standard deviation over 24 months, then exp((log concentration - baseline)/SD) and a weekly arithmetic mean. The table uses the current completed reporting week and shows NA when that week has no sample. References update in April/October for COVID-19 and August for influenza A/RSV. August 2026 cutoffs: COVID-19 2.6/4.9/7.9/11.6; influenza A 2.4/5.5/10.2/15.6; RSV 1.7/3.4/5.4/8.1.",
    wval_week = "WVAL reporting week: %s to %s.",
    wval_not_applicable = "Not applicable", wval_local = "Local calculation", wval_county_local = "Local county median",
    wval_missing_inputs = "WVAL: unnormalized concentrations or required source fields are unavailable; refresh data if using an older cache.",
    wval_missing_metadata = "WVAL: laboratory, method, assay, matrix or concentration units are missing.",
    wval_missing_lod = "WVAL: a measured detection limit is unavailable. A proxy is not substituted.",
    wval_mixed_methods = "WVAL: multiple methods or assays occur in the same week; they require reconciliation.",
    wval_no_week = "WVAL: no samples in the reporting week.",
    wval_short_history = "WVAL: fewer than 56 days or eight sampled weeks of usable history.",
    wval_lab_reference = "WVAL: an established site reference is unavailable; the required laboratory-wide baseline and SD are not in these public feeds.",
    wval_season_ineligible = "WVAL: sampling began after the October 1 seasonal eligibility cutoff.",
    wval_zero_sd = "WVAL: the historical standard deviation is zero or invalid.",
    wval_quality = "WVAL: current samples have exclusion or unresolved quality flags.",
    wval_county_incomplete = "WVAL: one or more constituent cities lack a valid result for the same reporting week.",
    wval_no_positive_reference = "WVAL: the reference period has no positive concentration from which to replace zeros.",
    wlevel_description = "PBWL is a local adaptation, not CDC WVAL. Categories are Very low, Low, Moderate, High and Very high. The latest available %d-day smoothed value in the last %d days is compared with historical smoothed values on sampling dates over %d calendar months, ending %d days before the reference date. Values equal to a cutoff belong to the lower category.",
    wlevel_schedule = "References update on April 1 and October 1 for COVID-19, on August 1 for influenza A (including H1, H3 and H5) and RSV, and with the latest available sample for other pathogens. Historical corrections can change the cutoffs. County PBWL uses the existing population-weighted county series.",
    wlevel_cutoffs = "Percentiles: COVID-19: %s; influenza A and subtypes: %s; RSV: %s; other seasonal pathogens: %s; other non-seasonal pathogens: %s.",
    wlevel_criteria = "At least %d positive sampling days in the last %d days and one in the last %d days are required for percentile classification; otherwise PBWL is Very low when recent observations exist. NA indicates no observations in the last %d days, no recent smoothed value, or fewer than %d valid historical samples. Sparse history can make extreme percentiles unstable.",
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
    pathogen_wnv = "West Nile Virus", pathogen_tb = "M. Tuberculosis", pathogen_ndm = "blaNDM",
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
    wlevel = "PBWL", very_low = "Muy bajo", moderate = "Moderado", very_high = "Muy alto",
    info_wlevel = "Nivel en aguas residuales basado en percentiles (PBWL)",
    wval = "WVAL", level_comparison = "Comparación: Level, PBWL y WVAL",
    comparison_note = "Level tiene tres categorías; PBWL y WVAL tienen cinco. WVAL se calcula localmente con muestras sin normalizar y no es un resultado oficial del CDC. La mediana del condado es una extensión local y requiere todas sus ciudades. Otros patógenos: No aplica.",
    concentration_units = "Todas las métricas usan concentraciones sin normalizar. Las ciudades de WastewaterSCAN usan gc/g de peso seco; Modesto usa copias/L de CDPH. Los valores y umbrales se calculan por separado en cada sitio y no deben compararse numéricamente entre esas unidades.",
    wval_method = "El WVAL local supone un solo método de laboratorio continuo por sitio. Los ceros se sustituyen por la mitad del menor valor positivo de referencia; NA permanece faltante. Usa concentraciones logarítmicas, winsorización P99, referencia P10 y desviación estándar de 24 meses; después exp((log concentración - referencia)/DE) y un promedio aritmético semanal. La tabla usa la semana de reporte completa actual y muestra NA cuando esa semana no tiene muestra. La referencia se actualiza en abril/octubre para COVID-19 y agosto para influenza A/VRS. Umbrales de agosto de 2026: COVID-19 2.6/4.9/7.9/11.6; influenza A 2.4/5.5/10.2/15.6; VRS 1.7/3.4/5.4/8.1.",
    wval_week = "Semana de WVAL: %s a %s.",
    wval_not_applicable = "No aplica", wval_local = "Cálculo local", wval_county_local = "Mediana local del condado",
    wval_missing_inputs = "WVAL: faltan concentraciones sin normalizar o campos de origen; actualiza los datos si usas una caché anterior.",
    wval_missing_metadata = "WVAL: faltan datos de laboratorio, método, ensayo, matriz o unidades de concentración.",
    wval_missing_lod = "WVAL: falta un límite de detección medido. No se sustituye por una aproximación.",
    wval_mixed_methods = "WVAL: hay varios métodos o ensayos en la misma semana; requieren conciliación.",
    wval_no_week = "WVAL: no hay muestras en la semana de reporte.",
    wval_short_history = "WVAL: hay menos de 56 días u ocho semanas con muestras históricas utilizables.",
    wval_lab_reference = "WVAL: falta una referencia establecida del sitio; estas fuentes públicas no incluyen la referencia y DE de todo el laboratorio requeridas.",
    wval_season_ineligible = "WVAL: el muestreo comenzó después del límite estacional del 1 de octubre.",
    wval_zero_sd = "WVAL: la desviación estándar histórica es cero o inválida.",
    wval_quality = "WVAL: las muestras actuales tienen indicadores de exclusión o de calidad sin resolver.",
    wval_county_incomplete = "WVAL: una o más ciudades del condado no tienen un resultado válido de la misma semana.",
    wval_no_positive_reference = "WVAL: el período de referencia no tiene una concentración positiva para sustituir los ceros.",
    wlevel_description = "PBWL es una adaptación local, no el WVAL del CDC. Las categorías son Muy bajo, Bajo, Moderado, Alto y Muy alto. El último valor suavizado de %d días disponible en los últimos %d días se compara con valores históricos suavizados en fechas de muestreo durante %d meses calendario, hasta %d días antes de la fecha de referencia. Los valores iguales a un umbral pertenecen a la categoría inferior.",
    wlevel_schedule = "Las referencias se actualizan el 1 de abril y el 1 de octubre para COVID-19, el 1 de agosto para influenza A (incluidos H1, H3 y H5) y VRS, y con la última muestra disponible para los demás patógenos. Las correcciones históricas pueden cambiar los umbrales. PBWL del condado usa la serie existente ponderada por población.",
    wlevel_cutoffs = "Percentiles: COVID-19: %s; influenza A y subtipos: %s; VRS: %s; otros patógenos estacionales: %s; otros no estacionales: %s.",
    wlevel_criteria = "Se requieren al menos %d días de muestreo positivos en los últimos %d días y uno en los últimos %d días para clasificar por percentiles; en caso contrario, PBWL es Muy bajo si existen observaciones recientes. NA indica ausencia de observaciones en los últimos %d días, ausencia de un valor suavizado reciente o menos de %d muestras históricas válidas. Una historia escasa puede producir percentiles extremos inestables.",
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
    pathogen_wnv = "Virus del Nilo Occidental", pathogen_tb = "M. tuberculosis", pathogen_ndm = "blaNDM",
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
    MPXV_dD14.16_norm_PMMoV = "pathogen_mpoxi", MPXV_G2R_norm_PMMoV = "pathogen_mpoxii",
    WNV_norm_PMMoV = "pathogen_wnv", TB_RD9_norm_PMMoV = "pathogen_tb",
    NDM_norm_PMMoV = "pathogen_ndm")
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

format_wval <- function(value, category, status, week_end, unit, lang) {
  result <- rep("NA", length(status))
  applicable <- !is.na(status) & status == "wval_not_applicable"
  result[applicable] <- tr("wval_not_applicable", lang)
  valid <- is.finite(value) & !is.na(category)
  result[valid] <- metric_label(category[valid], lang)
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
