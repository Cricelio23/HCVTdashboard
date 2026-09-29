app_config <- function() {
  pathogens <- c(
    SC2_N_norm_PMMoV = "SARS-CoV-2", Noro_G2_norm_PMMoV = "Norovirus",
    HAdV_F_norm_PMMoV = "Human Adenovirus Group F", Rotavirus_norm_PMMoV = "Rotavirus",
    EV.D68_norm_PMMoV = "Enterovirus D68", HAV_norm_PMMoV = "Hepatitis A Virus",
    C_auris_norm_PMMoV = "Candida auris", RSV_norm_PMMoV = "RSV", HMPV_4_norm_PMMoV = "HMPV",
    InfA_H1_norm_PMMoV = "H1", InfA_H3_norm_PMMoV = "H3", InfA_H5_norm_PMMoV = "H5",
    Influenza_A_norm_PMMoV = "Influenza A", Influenza_B_norm_PMMoV = "Influenza B",
    MPXV_dD14.16_norm_PMMoV = "Mpox Clade Ib", MPXV_G2R_norm_PMMoV = "Mpox Clade II",
    Parvo_B19_norm_PMMoV = "Parvovirus", MeV_norm_PMMoV = "Measles")
  list(
    pathogens = pathogens,
    cities = c("Merced", "Los Banos", "Modesto", "Turlock", "Davis", "Esparto", "Winters", "Woodland"),
    reporting_cities = c("Merced", "Modesto", "Turlock", "Davis", "Woodland"),
    counties = list(Yolo = c("Davis", "Woodland")),
    summary_counties = list(Merced = "Merced", Stanislaus = c("Modesto", "Turlock"), Yolo = c("Davis", "Woodland")),
    selected = names(pathogens)[pathogens %in% c("SARS-CoV-2", "Norovirus", "Enterovirus D68",
      "Hepatitis A Virus", "Candida auris", "RSV", "HMPV", "Influenza A", "Influenza B")],
    seasonal = c("RSV_norm_PMMoV", "HMPV_4_norm_PMMoV", "InfA_H1_norm_PMMoV",
      "InfA_H3_norm_PMMoV", "InfA_H5_norm_PMMoV", "Influenza_A_norm_PMMoV",
      "Influenza_B_norm_PMMoV", "EV.D68_norm_PMMoV", "Rotavirus_norm_PMMoV"),
    respiratory = c("SC2_N_norm_PMMoV", "EV.D68_norm_PMMoV", "RSV_norm_PMMoV",
      "HMPV_4_norm_PMMoV", "InfA_H1_norm_PMMoV", "InfA_H3_norm_PMMoV",
      "InfA_H5_norm_PMMoV", "Influenza_A_norm_PMMoV", "Influenza_B_norm_PMMoV"),
    gastrointestinal = c("Noro_G2_norm_PMMoV", "HAdV_F_norm_PMMoV",
      "Rotavirus_norm_PMMoV", "HAV_norm_PMMoV"),
    trend_days = 10L, recent_days = 14L, min_positive = 3L, moving_days = 10L,
    baseline_days = 365L, baseline_exclude = 10L,
    scan_url = "https://storage.googleapis.com/wastewater-dev-data/scan.csv",
    cdph_url = "https://data.chhs.ca.gov/api/3/action/datastore_search",
    cdph_resource = "2742b824-3736-4292-90a9-7fad98e94c06",
    data_mode = Sys.getenv("HCVT_DATA_MODE", "auto"),
    snapshot = Sys.getenv("HCVT_DATA_FILE", "data.csv")
  )
}

get_config <- function(params = NULL, config = app_config()) {
  if (!is.null(params)) config$params <- params
  config
}
