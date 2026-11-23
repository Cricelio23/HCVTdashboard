# Checks dashboard_export/ww_trend.R against the functions of this project, on real series (Sacramento and two plants sampled three times a week).
# Run from the project root: Rscript dashboard_export/test_ww_trend.R      (stops with an error if anything differs)
suppressPackageStartupMessages(library(data.table))
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)
source("dashboard_export/ww_trend.R")
ww <- fread("data/processed/scan_ca_ww.csv"); ww[, date := as.Date(date)]
SITES <- c("Sacramento Regional Wastewater Treatment Plant | CA", "Hyperion Water Reclamation Plant (HWRP) | CA", "City of Davis Wastewater Treatment Plant | CA")
chk <- function(a, b, what, tol = 1e-8) { d <- max(abs(a - b), na.rm = TRUE); stopifnot(identical(is.na(a), is.na(b)), d < tol); cat(sprintf("   ok  %-46s max abs difference %.1e\n", what, d)) }
for (s in SITES) for (p in c("influenza", "rsv")) {
  z <- ww[site_id == s & pathogen == p][order(date)]; has <- !is.na(z$conc); lod <- z$lod[1]
  cat(sprintf("%s, %s (%d days)\n", substr(s, 1, 40), p, nrow(z)))
  # 1. preparation and whole-series fit
  tr <- ww_trend(z$date[has], z$conc[has], lod = lod); stopifnot(nrow(tr) == nrow(z), all(tr$date == z$date))
  chk(tr$y_log10, z$y_log10, "log10 value that enters the model")
  fit <- fit_ss(z$y_log10, "irw"); gp <- growth_probability(z$y_log10, fit); sm <- ss_smooth_slope(ss_state(z$y_log10, fit)); par <- attr(tr, "params")
  chk(c(par$sdB, par$noise), c(fit$sdB, fit$sdR), "parameters (sdB, noise)", 1e-6)
  chk(tr$P_growth, gp$P, "P_growth, whole-series fit", 1e-6); chk(tr$P_positive, gp$pg0, "P_positive, whole-series fit", 1e-6)
  chk(tr$level_filt, gp$level, "filtered level", 1e-6); chk(tr$level_smooth, sm$level, "smoothed level", 1e-6); chk(tr$slope_smooth, sm$slope, "smoothed slope", 1e-6)
  chk(as.integer(tr$alert), detect_rule(gp), "alert flag", 0.5)
  # 2. append-only updates, one per month, against the same blocks computed with the functions of the project (as in scripts/15_realtime_refit.R: parameters from the
  #    730 days before the first new date, filter with the data up to each day). A block starts on the first date after the last published one, which is the first day
  #    of the month only when the plant was sampled on the last day of the previous month.
  y <- z$y_log10; n <- length(y); W <- 730L; cut <- c(which(format(z$date, "%d") == "01" & seq_len(n) > W + 31L) - 1L, n); Pref <- rep(NA_real_, n)
  use0 <- which(has & seq_len(n) <= cut[1]); pub <- ww_realtime_update(NULL, z$date[use0], z$conc[use0], lod = lod)$published; lastpub <- max(use0); first_cmp <- lastpub + 1L
  for (j in 2:length(cut)) { use <- which(has & seq_len(n) <= cut[j]); if (max(use) == lastpub) next
    r <- lastpub + 1L; last <- max(use); i0 <- r - W; Pref[r:last] <- growth_probability(y[i0:last], fit_ss(y[i0:(r - 1L)], "irw"))$P[(r - i0 + 1L):(last - i0 + 1L)]
    old <- pub; res <- ww_realtime_update(pub, z$date[use], z$conc[use], lod = lod); pub <- res$published; lastpub <- last
    stopifnot(isTRUE(all.equal(pub[seq_len(nrow(old)), ], old, check.attributes = FALSE))) }
  m <- match(z$date[first_cmp:n], pub$date); stopifnot(!anyNA(m))
  chk(pub$P_growth[m], Pref[first_cmp:n], "P_growth, monthly append-only updates", 1e-6)
  cat("   ok  published rows and past alerts unchanged after every update\n")
  stopifnot(sum(res$smooth$provisional) == 21, all(c("level_smooth", "smooth_lo", "smooth_hi") %in% names(res$smooth)))
}
# 3. statuses
z <- ww[site_id == SITES[1] & pathogen == "influenza" & !is.na(conc)][order(date)]
r1 <- ww_realtime_update(NULL, z$date[1:200], z$conc[1:200], lod = z$lod[1]); stopifnot(r1$status == "insufficient_history", is.null(r1$published), !all(is.na(r1$smooth$level_smooth)))
r2 <- ww_realtime_update(NULL, z$date, z$conc, lod = z$lod[1]); r3 <- ww_realtime_update(r2$published, z$date, z$conc, lod = z$lod[1])
stopifnot(r2$status == "ok", r3$status == "no_new_data", identical(r2$published, r3$published))
zz <- copy(z)[, conc := 0]; zz$conc[c(5, 50)] <- z$lod[1] * 3; r4 <- ww_realtime_update(NULL, zz$date, zz$conc, lod = z$lod[1]); stopifnot(r4$status == "few_quantified", all(is.na(r4$published$P_growth)), !any(r4$published$alert))
# 4. short history with a prior: first call on a series, blocks of 30 days from day 61, against the same blocks with the filter of the project and the same penalised fit
ref_fit <- function(y, prior) { nll <- function(th) { v <- ss_filter(y, SDQ_IRW, exp(th[1]), exp(th[2])); if (!is.finite(v)) return(1e10); -v + 0.5 * sum(((th - prior$mu) / prior$sd)^2) }
  best <- NULL; for (s in list(log(c(0.005, 0.3)), log(c(0.01, 0.1)), log(c(0.002, 0.5)), prior$mu)) { o <- optim(s, nll, method = "Nelder-Mead", control = list(maxit = 600, reltol = 1e-8)); if (is.null(best) || o$value < best$value) best <- o }
  list(model = "irw", sdQ = SDQ_IRW, sdB = exp(best$par[1]), sdR = exp(best$par[2]), het = FALSE, msd2 = NULL, p0b = P0B, lim = NULL) }
for (s in SITES[1:2]) { z <- ww[site_id == s & pathogen == "influenza"][order(date)][1:500]; last <- max(which(!is.na(z$conc))); z <- z[1:last]; has <- !is.na(z$conc); y <- z$y_log10; n <- length(y)
  res <- ww_realtime_update(NULL, z$date[has], z$conc[has], lod = z$lod[1], prior = WW_PRIOR); pub <- res$published; stopifnot(res$status == "ok", pub$date[1] == z$date[61], nrow(pub) == n - 60)
  Pref <- rep(NA_real_, n); starts <- seq(61L, n, by = 30L)
  for (j in seq_along(starts)) { r <- starts[j]; e <- if (j < length(starts)) starts[j + 1] - 1L else n; Pref[r:e] <- growth_probability(y[1:e], ref_fit(y[1:(r - 1L)], WW_PRIOR))$P[r:e] }
  cat(sprintf("%s, influenza, first 500 days with prior\n", substr(s, 1, 40))); chk(pub$P_growth, Pref[61:n], "P_growth from day 61 with WW_PRIOR", 1e-5) }
pr <- ww_prior(c(0.002, 0.003, 0.004), c(0.3, 0.32, 0.35)); stopifnot(length(pr$mu) == 2, all(pr$sd >= 0.3))
cat("statuses ok (insufficient_history, no_new_data, few_quantified)\nALL CHECKS PASSED\n")
