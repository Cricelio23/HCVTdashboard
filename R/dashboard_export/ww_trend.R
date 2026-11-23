# ww_trend.R - Wastewater trend for a surveillance dashboard: smoothed level and real-time probability of growth.
# Self-contained (base R only). Copied from the project "wastewater_trend_benchmark" (R/methods_statespace.R, R/growth_probability.R) and checked against it
# with dashboard_export/test_ww_trend.R.
#
# MODEL (integrated random walk on the log10 concentration, one series = one plant and one pathogen)
#   y_t = x_t + e_t,          e_t ~ N(0, noise^2)     observation
#   x_t = x_{t-1} + b_{t-1}                           level (smooth trend)
#   b_t = b_{t-1} + v_t,      v_t ~ N(0, sdB^2)       slope, log10 per day
# Two parameters (sdB, noise), estimated by maximum likelihood. Kalman filter = what was known on each day; RTS smoother = retrospective curve.
#
# DATA RULES
#   * conc = 0 means "analysed, not detected": it enters as log10(LOD / 2). conc = NA or a date without a row = no sample that day (missing).
#   * One value per date; if a date is repeated the first non-missing value is used.
#   * lod: give the limit of detection of the laboratory if you have it. If NULL, the smallest positive concentration of the series is used.
#
# PROBABILITY OF GROWTH (real time)
#   P_growth_t   = P( x_t - x_{t-k} >= delta | data up to t )     k = 14 days
#   P_positive_t = P( x_t - x_{t-k} >  0     | data up to t )
#   delta = 0.21 log10 (62 % in 14 days, doubling in 20 days) for seasonal pathogens (influenza A, RSV); about 0.097 (25 % in 14 days) was better for SARS-CoV-2.
# ALERT RULE: an episode starts when P_growth >= 0.5 on 14 consecutive days and ends when P_positive <= 0.5 on 14 consecutive days.
#
# TWO ENTRY POINTS
#   ww_trend()            one fit on the whole series. Simple; the parameters use all the dates, so past values can move when data are added.
#   ww_realtime_update()  append-only. Each call estimates the parameters with the last `window` days before the new dates, computes the probability for the
#                         new dates only and appends it to the table already published. Published rows are never recomputed, so past probabilities and alerts
#                         do not change. It also returns the smoothed curve, whose last `provisional_days` days are flagged as provisional.
#                         For a new pathogen or plant pass prior = WW_PRIOR: the series starts after 60 days instead of two years.
#
# WHAT IS APPROXIMATE OR UNTESTED
#   * The uncertainty of the two parameters is not propagated: the probability is exact given the parameters.
#   * Without a prior the probability needs two years of history (min_history = 730). With a prior (WW_PRIOR) it was evaluated from day 61 on the first two years of 48
#     series: same detected seasons and summer starts as with the whole-series fit; in the first two months the probability differs more (5 % of the days across 0.5).
#   * When nearly all the values are non-detects the probability stays near 0.5 and is not informative; see `min_quantified`.

.WW_SDQ <- 1e-6; .WW_P0X <- 100; .WW_P0B <- 0.05^2

# Kalman filter. store = FALSE returns the log-likelihood; store = TRUE also the filtered (Mf, Pf) and predicted (Mp, Pp) moments. P columns: var(level), cov, var(slope).
.ww_filter <- function(y, sdB, sdR, store = FALSE) {
  n <- length(y); q1 <- .WW_SDQ^2; q2 <- sdB^2; r2 <- sdR^2; ll <- 0
  i1 <- which(!is.na(y))[1]
  m1 <- y[i1]; m2 <- 0; p11 <- .WW_P0X; p12 <- 0; p22 <- .WW_P0B
  if (store) { Mf <- Mp <- matrix(NA_real_, n, 2); Pf <- Pp <- matrix(NA_real_, n, 3) }
  for (t in i1:n) {
    m1 <- m1 + m2; a11 <- p11 + 2 * p12 + p22 + q1; a12 <- p12 + p22; a22 <- p22 + q2
    if (store) { Mp[t, ] <- c(m1, m2); Pp[t, ] <- c(a11, a12, a22) }
    if (!is.na(y[t])) {
      S <- a11 + r2; r <- y[t] - m1
      ll <- ll - 0.5 * (log(2 * pi) + log(S) + r * r / S)
      K1 <- a11 / S; K2 <- a12 / S; m1 <- m1 + K1 * r; m2 <- m2 + K2 * r
      p11 <- a11 - K1 * a11; p12 <- a12 - K1 * a12; p22 <- a22 - K2 * a12
    } else { p11 <- a11; p12 <- a12; p22 <- a22 }
    if (store) { Mf[t, ] <- c(m1, m2); Pf[t, ] <- c(p11, p12, p22) }
  }
  if (store) list(loglik = ll, Mf = Mf, Pf = Pf, Mp = Mp, Pp = Pp, i1 = i1) else ll
}

# Fit (Nelder-Mead on the log scale). prior = NULL: maximum likelihood. prior = list(mu = c(log sdB, log noise), sd = c(., .)): the log-likelihood plus independent
# normal priors on the two log parameters (maximum a posteriori), for series with a short history.
.ww_fit <- function(y, prior = NULL) {
  nll <- function(th) { v <- .ww_filter(y, exp(th[1]), exp(th[2])); if (!is.finite(v)) return(1e10); -v + if (is.null(prior)) 0 else 0.5 * sum(((th - prior$mu) / prior$sd)^2) }
  best <- NULL
  for (s in c(list(log(c(0.005, 0.3)), log(c(0.01, 0.1)), log(c(0.002, 0.5))), if (!is.null(prior)) list(prior$mu))) {
    o <- stats::optim(s, nll, method = "Nelder-Mead", control = list(maxit = 600, reltol = 1e-8))
    if (is.null(best) || o$value < best$value) best <- o
  }
  list(sdB = exp(best$par[1]), noise = exp(best$par[2]))
}

# Rauch-Tung-Striebel smoother: retrospective level and slope with their standard deviations.
.ww_smooth <- function(fl) {
  n <- nrow(fl$Mf); L <- S <- Lsd <- Ssd <- rep(NA_real_, n); Ft <- matrix(c(1, 1, 0, 1), 2, 2)
  sym <- function(v) matrix(c(v[1], v[2], v[2], v[3]), 2, 2)
  ms <- fl$Mf[n, ]; Ps <- sym(fl$Pf[n, ]); L[n] <- ms[1]; S[n] <- ms[2]; Lsd[n] <- sqrt(max(Ps[1, 1], 0)); Ssd[n] <- sqrt(max(Ps[2, 2], 0))
  if (n > fl$i1) for (t in (n - 1):fl$i1) {
    Pf <- sym(fl$Pf[t, ]); Pp <- sym(fl$Pp[t + 1, ]); J <- Pf %*% Ft %*% solve(Pp)
    ms <- fl$Mf[t, ] + J %*% (ms - fl$Mp[t + 1, ]); Ps <- Pf + J %*% (Ps - Pp) %*% t(J)
    L[t] <- ms[1]; S[t] <- ms[2]; Lsd[t] <- sqrt(max(Ps[1, 1], 0)); Ssd[t] <- sqrt(max(Ps[2, 2], 0))
  }
  list(level = L, level_sd = Lsd, slope = S, slope_sd = Ssd)
}

# Filtered probability of growth over k days: Kalman filter on the state augmented with a frozen copy of x_{t-k} (exact given the parameters).
.ww_growth <- function(y, sdB, sdR, k = 14L, delta = 0.21) {
  n <- length(y); fl <- .ww_filter(y, sdB, sdR, store = TRUE); q1 <- .WW_SDQ^2; q2 <- sdB^2; r2 <- sdR^2
  out <- data.frame(level = fl$Mf[, 1], slope = fl$Mf[, 2], P = NA_real_, pg0 = NA_real_)
  if (n <= k) return(out)
  t0 <- seq_len(n - k); t0 <- t0[!is.na(fl$Mf[t0, 1])]
  if (!length(t0)) return(out)
  a <- x <- fl$Mf[t0, 1]; b <- fl$Mf[t0, 2]
  caa <- cax <- cxx <- fl$Pf[t0, 1]; cab <- cxb <- fl$Pf[t0, 2]; cbb <- fl$Pf[t0, 3]
  for (j in seq_len(k)) {
    x <- x + b
    cax <- cax + cab; cxx <- cxx + 2 * cxb + cbb + q1; cxb <- cxb + cbb; cbb <- cbb + q2
    yj <- y[t0 + j]; m <- !is.na(yj); S <- cxx + r2; S[!m] <- 1
    Ka <- cax / S; Kx <- cxx / S; Kb <- cxb / S; inn <- ifelse(m, yj - x, 0); mm <- as.numeric(m)
    a <- a + Ka * inn; x <- x + Kx * inn; b <- b + Kb * inn
    caa <- caa - mm * Ka * cax; cab <- cab - mm * Ka * cxb
    cbb_n <- cbb - mm * Kb * cxb; cax_n <- cax - mm * Ka * cxx; cxb_n <- cxb - mm * Kx * cxb; cxx_n <- cxx - mm * Kx * cxx
    cax <- cax_n; cxb <- cxb_n; cxx <- cxx_n; cbb <- cbb_n
  }
  dm <- x - a; ds <- sqrt(pmax(caa + cxx - 2 * cax, 1e-12)); tt <- t0 + k
  out$P[tt] <- stats::pnorm((dm - delta) / ds); out$pg0[tt] <- stats::pnorm(dm / ds)
  out
}

# Alert rule (causal: the flag of a day depends only on that day and earlier ones).
.ww_alert <- function(P, pg0, rule) {
  n <- length(P); active <- logical(n); on <- FALSE; run <- runend <- 0L
  for (i in seq_len(n)) {
    run <- if (!is.na(P[i]) && P[i] >= rule$p_star) run + 1L else 0L
    runend <- if (!is.na(pg0[i]) && pg0[i] <= 0.5) runend + 1L else 0L
    if (!on && run >= rule$n_alpha) { on <- TRUE; runend <- 0L } else if (on && runend >= rule$n_end) on <- FALSE
    active[i] <- on
  }
  active
}

WW_RULE <- list(p_star = 0.5, n_alpha = 14L, n_end = 14L)

# Prior for series with a short history (a pathogen or a plant that has just started to be monitored). Centre: median of the 48 series of the project (influenza A and RSV,
# 24 WastewaterSCAN plants in California, solids, log10 copies/g): sdB 0.0028 log10/day, noise 0.32 log10. Width: SD of 0.6 and 0.4 on the log scale, about three times the
# spread between those series, so that the data take over within a few months. If your established series come from another laboratory or matrix, build your own prior with
# ww_prior() from their fitted parameters.
WW_PRIOR <- list(mu = c(log(0.0028), log(0.32)), sd = c(0.6, 0.4))

#' Prior from the parameters fitted to established series (e.g. attr(ww_trend(...), "params") of other pathogens or plants).
#' @param sdB,noise vectors with the fitted values, one per established series (at least 3)
#' @param widen factor applied to the spread between series (3 was evaluated); the SDs are never smaller than 0.3
ww_prior <- function(sdB, noise, widen = 3) {
  stopifnot(length(sdB) == length(noise), length(sdB) >= 3, all(sdB > 0), all(noise > 0))
  list(mu = c(stats::median(log(sdB)), stats::median(log(noise))), sd = pmax(widen * c(stats::sd(log(sdB)), stats::sd(log(noise))), 0.3))
}

#' Daily grid with the log10 value that enters the model.
#' @return data.frame(date, conc, nondetect, y_log10) with attribute "lod"
ww_prepare <- function(date, conc, lod = NULL) {
  date <- as.Date(date); conc <- as.numeric(conc); ok <- !is.na(date)
  x <- data.frame(date = date[ok], conc = conc[ok]); x <- x[order(x$date, is.na(x$conc)), ]; x <- x[!duplicated(x$date), ]
  if (!nrow(x) || all(is.na(x$conc))) stop("ww_prepare: no concentration values")
  x$conc[!is.na(x$conc) & x$conc < 1e-6] <- 0
  if (is.null(lod)) { pos <- x$conc[!is.na(x$conc) & x$conc > 0]; if (!length(pos)) stop("ww_prepare: no quantified value and no lod given"); lod <- min(pos) }
  out <- merge(data.frame(date = seq(min(x$date[!is.na(x$conc)]), max(x$date[!is.na(x$conc)]), by = "day")), x, by = "date", all.x = TRUE)
  out$nondetect <- !is.na(out$conc) & out$conc == 0
  out$y_log10 <- ifelse(is.na(out$conc), NA_real_, ifelse(out$nondetect, log10(lod / 2), log10(pmax(out$conc, 1e-6))))
  attr(out, "lod") <- lod
  out
}

#' Trend with ONE fit on the whole series (retrospective parameters).
#' @param delta growth threshold over k days on the log10 scale (0.21 seasonal pathogens; about 0.097 for SARS-CoV-2)
#' @param params optional list(sdB, noise) to skip the fit
#' @param prior NULL (maximum likelihood) or a prior such as WW_PRIOR, for a series with a short history
#' @return data.frame, one row per day: date, conc, nondetect, y_log10, level_smooth, smooth_lo, smooth_hi (95 %), slope_smooth (log10/day), level_filt,
#'   P_growth, P_positive, alert. Attributes: "params" (sdB, noise, lod) and "status" ("ok", "few_quantified").
ww_trend <- function(date, conc, lod = NULL, delta = 0.21, k = 14L, rule = WW_RULE, params = NULL, min_quantified = 20L, prior = NULL) {
  z <- ww_prepare(date, conc, lod); y <- z$y_log10
  if (is.null(params)) params <- .ww_fit(y, prior)
  gp <- .ww_growth(y, params$sdB, params$noise, k, delta); sm <- .ww_smooth(.ww_filter(y, params$sdB, params$noise, store = TRUE))
  z$level_smooth <- sm$level; z$smooth_lo <- sm$level - 1.96 * sm$level_sd; z$smooth_hi <- sm$level + 1.96 * sm$level_sd; z$slope_smooth <- sm$slope
  z$level_filt <- gp$level; z$P_growth <- gp$P; z$P_positive <- gp$pg0; z$alert <- .ww_alert(gp$P, gp$pg0, rule)
  attr(z, "params") <- list(sdB = params$sdB, noise = params$noise, lod = attr(z, "lod"))
  attr(z, "status") <- if (sum(!is.na(y) & !z$nondetect) < min_quantified) "few_quantified" else "ok"
  z
}

#' Real-time, append-only update for a dashboard.
#' Call it every time the data are refreshed, with ALL the data of the series and the table returned by the previous call (`published`; NULL the first time).
#' For the dates after the last published one: the parameters are estimated with the `window` days before the first new date, and the probabilities are filtered
#' with those parameters and the data up to each day. Rows already published are returned untouched.
#' The first call fills the history in blocks of `backfill_step` days, each with the parameters that would have been estimated at that moment.
#' @param published data.frame returned as $published by the previous call (store it between sessions, e.g. saveRDS), or NULL
#' @param window days used to estimate the parameters (730 = two years)
#' @param prior NULL, or a prior on the parameters such as WW_PRIOR (see ww_prior()). With a prior the series can start after 60 days of history: use it for a pathogen or a
#'   plant that has just started to be monitored. Keep passing the same prior on every call; once there are two years of data it has almost no effect.
#' @param min_history minimum days of history before the first probability is published. Evaluated: 730 without a prior, 60 with one
#' @param min_quantified minimum number of quantified (detected) values in the estimation window; below it the row is published with status "few_quantified" and no probability
#' @param provisional_days last days of the smoothed curve flagged as provisional (they move when new data arrive)
#' @return list(
#'   published = data.frame(date, P_growth, P_positive, level_filt, alert, sdB, noise, status, published_on)   frozen real-time values, one row per day,
#'   smooth    = data.frame(date, conc, nondetect, y_log10, level_smooth, smooth_lo, smooth_hi, slope_smooth, provisional)   current retrospective curve,
#'   params    = list(sdB, noise, lod) used for the newest block and for the smoothed curve,
#'   status    = "ok" | "insufficient_history" | "few_quantified" | "no_new_data")
ww_realtime_update <- function(published = NULL, date, conc, lod = NULL, delta = 0.21, k = 14L, rule = WW_RULE, window = 730L, prior = NULL,
                               min_history = if (is.null(prior)) 730L else 60L, min_quantified = if (is.null(prior)) 20L else 5L, backfill_step = 30L, provisional_days = 21L) {
  z <- ww_prepare(date, conc, lod); y <- z$y_log10; n <- length(y); lod_used <- attr(z, "lod")
  cols <- c("date", "P_growth", "P_positive", "level_filt", "alert", "sdB", "noise", "status", "published_on")
  if (!is.null(published) && nrow(published)) { published <- published[order(published$date), cols]; first_new <- match(TRUE, z$date > max(published$date)) }
  else { published <- NULL; first_new <- min_history + 1L }
  status <- "ok"; params <- NULL; new <- list()
  if (is.na(first_new) || first_new > n) status <- if (is.null(published)) "insufficient_history" else "no_new_data"
  else {
    starts <- if (is.null(published)) seq(first_new, n, by = backfill_step) else first_new
    for (j in seq_along(starts)) { r <- starts[j]; last <- if (j < length(starts)) starts[j + 1] - 1L else n; i0 <- max(1L, r - window); w <- i0:(r - 1L)
      blk <- data.frame(date = z$date[r:last], P_growth = NA_real_, P_positive = NA_real_, level_filt = NA_real_, alert = FALSE, sdB = NA_real_, noise = NA_real_, status = "ok", published_on = Sys.Date())
      if (length(w) < min_history) blk$status <- "insufficient_history"
      else if (sum(!is.na(y[w]) & !z$nondetect[w]) < min_quantified) blk$status <- "few_quantified"
      else { params <- .ww_fit(y[w], prior); gp <- .ww_growth(y[i0:last], params$sdB, params$noise, k, delta); kk <- (r - i0 + 1L):(last - i0 + 1L)
        blk$P_growth <- gp$P[kk]; blk$P_positive <- gp$pg0[kk]; blk$level_filt <- gp$level[kk]; blk$sdB <- params$sdB; blk$noise <- params$noise }
      new[[j]] <- blk; status <- blk$status[1] }
    published <- rbind(published, do.call(rbind, new))
  }
  if (!is.null(published)) { g <- merge(data.frame(date = seq(min(published$date), max(published$date), by = "day")), published, by = "date", all.x = TRUE)   # the rule runs on consecutive days
    published$alert <- .ww_alert(g$P_growth, g$P_positive, rule)[match(published$date, g$date)]; rownames(published) <- NULL }
  # smoothed curve with the newest parameters (or one fit on everything available when there is not enough history yet)
  if (is.null(params)) { last_ok <- if (!is.null(published)) utils::tail(which(!is.na(published$sdB)), 1) else integer(0)
    params <- if (length(last_ok)) list(sdB = published$sdB[last_ok], noise = published$noise[last_ok]) else if (sum(!is.na(y) & !z$nondetect) >= min_quantified) .ww_fit(y, prior) else NULL }
  smooth <- z[, c("date", "conc", "nondetect", "y_log10")]
  if (!is.null(params)) { sm <- .ww_smooth(.ww_filter(y, params$sdB, params$noise, store = TRUE))
    smooth$level_smooth <- sm$level; smooth$smooth_lo <- sm$level - 1.96 * sm$level_sd; smooth$smooth_hi <- sm$level + 1.96 * sm$level_sd; smooth$slope_smooth <- sm$slope }
  else smooth$level_smooth <- smooth$smooth_lo <- smooth$smooth_hi <- smooth$slope_smooth <- NA_real_
  smooth$provisional <- smooth$date > max(smooth$date) - provisional_days
  list(published = published, smooth = smooth, params = if (is.null(params)) NULL else list(sdB = params$sdB, noise = params$noise, lod = lod_used), status = status)
}
