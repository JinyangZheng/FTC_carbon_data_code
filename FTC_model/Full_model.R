
# Aggregate breakup occurs only while temperatures rise from zero to ten degrees.

breakup_warming_gate <- function(temp) {
  temp <- as.numeric(temp)
  if (!length(temp) || any(!is.finite(temp))) {
    stop("temp must contain finite hourly temperatures", call. = FALSE)
  }
  change <- c(0, diff(temp))
  as.integer(temp >= 0 & temp <= 10 & change > 0)
}


FTC_cfg <- list(
  T50 = -0.5,
  width = 0.25,
  T_warm = 0,
  T_cold = -10,
  dt_hours = 1
)

make_ftc_drivers <- function(temp, cfg = FTC_cfg) {
  raw <- plogis((cfg$T50 - temp) / cfg$width)
  raw_warm <- plogis((cfg$T50 - cfg$T_warm) / cfg$width)
  raw_cold <- plogis((cfg$T50 - cfg$T_cold) / cfg$width)

  freeze_effect <- (raw - raw_warm) / (raw_cold - raw_warm)
  freeze_effect <- pmin(1, pmax(0, freeze_effect))
  active_modifier <- 1 - freeze_effect
  warming_gate <- breakup_warming_gate(temp)

  data.frame(
    freeze_effect = freeze_effect,
    active_modifier = active_modifier,
    warming_gate = warming_gate
  )
}

# Hourly carbon pools and fluxes, explicit Euler integration with a one-hour step.
# x parameter order is listed below.
# 1-4 C.sor1.10, C.sor1.minus10, C.sor2.10, C.sor2.minus10
# 5-8 r1.10, r1.minus10, r2.10, r2.minus10
# 9-12 V1, V2, MBC.D, Enz.P (natural-log rates)
# 13-16 CUE.minus10, f1.f2, intercept_P.DOC, intercept_P.NOC
# 17-19 dormancy.ratio, inactive.ratio, MBC.D.ice
# x[15], x[16], x[19] are natural-log rates.
# Supply x, initial MBC/DOC, CUE at 10 degrees and hourly temperature externally.
# make_ftc_drivers(temp) provides the three driver vectors.
# Output pools retain original normalized carbon units and hourly fluxes.
Full_kernel <- function(x, MB0, DOC0, CUE10, time, temp,
                          active_modifier, freeze_effect, warming_gate,
                          enzyme_death_log = -7) {
  timesteps <- length(temp)
  if (length(x) != 19L || any(!is.finite(x)))
    stop("Invalid parameter vector", call. = FALSE)
  if (timesteps < 2L || timesteps > 5761L ||
      any(lengths(list(time, active_modifier, freeze_effect, warming_gate)) != timesteps) ||
      any(!is.finite(c(time, temp, active_modifier, freeze_effect, warming_gate))))
    stop("Hourly drivers must be aligned and finite", call. = FALSE)
  if (!identical(as.numeric(time), as.numeric(seq_len(timesteps) - 1L)) ||
      !identical(as.numeric(warming_gate), as.numeric(breakup_warming_gate(temp))))
    stop("Time or warming gate is inconsistent", call. = FALSE)
  pools <- matrix(0, timesteps, 18L, dimnames = list(NULL, c(
    "Hour", "Temp", "MBC", "MBC1", "MBC2", "Enz", "Enz1", "Enz2",
    "P.DOC", "P.NOC", "DOC1", "DOC2", "DOC", "NOC1", "NOC2", "NOC",
    "MBC_R", "Enz.deg")))
  pools[, 1L] <- time
  pools[, 2L] <- temp
  total_soc <- 1e6
  gas <- 8.314
  enzyme_initial <- MB0 * 0.02
  carbon_initial <- total_soc - MB0 - enzyme_initial
  protected_doc_initial <- DOC0 * 0.5
  available_doc_initial <- DOC0 * 0.5
  protected_noc_initial <- (carbon_initial - DOC0) * 0.5
  available_noc_initial <- (carbon_initial - DOC0) * 0.5

  csor1_intercept <- (x[1L] + x[2L]) / 2.0
  csor1_slope <- (x[1L] - x[2L]) / 20.0
  csor2_intercept <- (x[3L] + x[4L]) / 2.0
  csor2_slope <- (x[3L] - x[4L]) / 20.0
  r1_intercept <- (x[5L] + x[6L]) / 2.0
  r1_slope <- (x[5L] - x[6L]) / 20.0
  r2_intercept <- (x[7L] + x[8L]) / 2.0
  r2_slope <- (x[7L] - x[8L]) / 20.0

  vmax_doc <- exp(x[9L])
  vmax_noc <- exp(x[10L])
  enzyme_death_rate <- exp(enzyme_death_log)
  microbial_death_rate <- exp(x[11L])
  enzyme_production_rate <- exp(x[12L])
  cue_intercept <- (CUE10 + x[13L]) / 2.0
  cue_slope <- (CUE10 - x[13L]) / 20.0
  isolated_fraction <- x[14L]
  break_doc_rate <- exp(x[15L])
  break_noc_rate <- exp(x[16L])
  dormancy_ratio <- x[17L]
  inactive_ratio <- x[18L]
  frozen_death_rate <- exp(x[19L])

  doc_k_10 <- available_doc_initial * 0.2
  doc_k_minus10 <- available_doc_initial * 0.8
  doc_k_intercept <- (doc_k_10 + doc_k_minus10) / 2.0
  doc_k_slope <- (doc_k_10 - doc_k_minus10) / 20.0
  noc_k_10 <- available_noc_initial * 0.2
  noc_k_minus10 <- available_noc_initial * 0.8
  noc_k_intercept <- (noc_k_10 + noc_k_minus10) / 2.0
  noc_k_slope <- (noc_k_10 - noc_k_minus10) / 20.0

  pools[1L, 3L] <- MB0
  pools[1L, 4L] <- MB0 * (1.0 - dormancy_ratio)
  pools[1L, 5L] <- MB0 * dormancy_ratio
  pools[1L, 6L] <- enzyme_initial
  pools[1L, 7L] <- enzyme_initial * (1.0 - inactive_ratio)
  pools[1L, 8L] <- enzyme_initial * inactive_ratio
  pools[1L, 9L] <- protected_doc_initial
  pools[1L, 10L] <- protected_noc_initial
  pools[1L, 11L] <- available_doc_initial * (1.0 - isolated_fraction)
  pools[1L, 12L] <- available_doc_initial * isolated_fraction
  pools[1L, 13L] <- available_doc_initial + protected_doc_initial
  pools[1L, 14L] <- available_noc_initial * (1.0 - isolated_fraction)
  pools[1L, 15L] <- available_noc_initial * isolated_fraction
  pools[1L, 16L] <- available_noc_initial + protected_noc_initial
  pools[1L, 17L] <- 0.0
  pools[1L, 18L] <- 0.0

  for (step in seq_len(timesteps - 1L)) {
    temperature <- temp[step]
    active <- active_modifier[step]
    frozen <- freeze_effect[step]
    warming <- warming_gate[step]
    doc_k <- doc_k_slope * temperature + doc_k_intercept
    noc_k <- noc_k_slope * temperature + noc_k_intercept
    cue <- cue_slope * temperature + cue_intercept
    csor1 <- csor1_slope * temperature + csor1_intercept
    csor2 <- csor2_slope * temperature + csor2_intercept
    r1 <- r1_slope * temperature + r1_intercept
    r2 <- r2_slope * temperature + r2_intercept
    arrhenius <- exp(-47.0 / (gas * (temperature + 273.0)))

    mbc1 <- pools[step, 4L]
    mbc2 <- pools[step, 5L]
    enz1 <- pools[step, 7L]
    enz2 <- pools[step, 8L]
    p_doc <- pools[step, 9L]
    p_noc <- pools[step, 10L]
    doc1 <- pools[step, 11L]
    doc2 <- pools[step, 12L]
    noc1 <- pools[step, 14L]
    noc2 <- pools[step, 15L]

    same1 <- vmax_doc * doc1 / (doc_k + doc1) * arrhenius
    same2 <- vmax_doc * doc2 / (doc_k + doc2) * arrhenius
    mbc1_u1 <- mbc1 * same1
    mbc1_u2 <- mbc1 * same2 * active
    mbc1_death <- mbc1 * microbial_death_rate
    mbc1_frozen_death <- mbc1 * frozen_death_rate * frozen
    enz1_production <- mbc1 * enzyme_production_rate
    mbc1_respiration =
        mbc1_u1 * (1.0 - cue) + mbc1_u2 * (1.0 - cue)
    mbc1_growth <- mbc1_u1 * cue + mbc1_u2 * cue

    mbc2_u1 <- mbc2 * same1 * active
    mbc2_u2 <- mbc2 * same2 * active * active
    mbc2_death <- mbc2 * microbial_death_rate * active
    enz2_production <- mbc2 * enzyme_production_rate * active
    mbc2_respiration =
        mbc2_u1 * (1.0 - cue) + mbc2_u2 * (1.0 - cue)
    mbc2_growth <- mbc2_u1 * cue + mbc2_u2 * cue

    same3 <- vmax_noc * noc1 / (noc_k + noc1) * arrhenius
    same4 <- vmax_noc * noc2 / (noc_k + noc2) * arrhenius
    enz1_deg1 <- enz1 * same3
    enz1_deg2 <- enz1 * same4 * active
    enz1_degradation <- enz1_deg1 + enz1_deg2
    enz1_death <- enz1 * enzyme_death_rate
    enz2_deg1 <- enz2 * same3 * active
    enz2_deg2 <- enz2 * same4 * active * active
    enz2_degradation <- enz2_deg1 + enz2_deg2
    enz2_death <- enz2 * enzyme_death_rate
    enzyme_degradation <- enz1_degradation + enz2_degradation
    enzyme_death <- enz1_death + enz2_death

    sorption_doc1 <- csor1 * doc1
    sorption_doc2 <- csor1 * doc2 * active
    sorption_available_doc <- sorption_doc1 + sorption_doc2
    sorption_noc1 <- csor2 * noc1
    sorption_noc2 <- csor2 * noc2 * active
    sorption_available_noc <- sorption_noc1 + sorption_noc2
    desorption_doc <- r1 * p_doc
    desorption_noc <- r2 * p_noc
    bdocp =
        break_doc_rate * (1.0 - temperature / 10.0) * warming
    bsocp =
        break_noc_rate * (1.0 - temperature / 10.0) * warming
    break_doc <- min(p_doc, p_doc * bdocp)
    break_noc <- min(p_noc, p_noc * bsocp)

    d_mbc1 <- mbc1_growth -
        (mbc1_death + mbc1_frozen_death + enz1_production)
    d_mbc2 <- mbc2_growth - (mbc2_death + enz2_production)
    d_mbc <- d_mbc1 + d_mbc2
    d_enz1 <- enz1_production - enz1_death
    d_enz2 <- enz2_production - enz2_death
    d_enz <- enz1_production + enz2_production - enzyme_death
    d_p_doc <- sorption_available_doc -
        (break_doc + desorption_doc)
    d_p_noc <- sorption_available_noc -
        (break_noc + desorption_noc)

    mortality_input =
        mbc1_death + mbc2_death + mbc1_frozen_death
    d_doc1 <- mortality_input * 0.2 *
            (1.0 - isolated_fraction) +
        enzyme_degradation * (1.0 - isolated_fraction) +
        enzyme_death * (1.0 - isolated_fraction) +
        (break_doc + desorption_doc) * (1.0 - isolated_fraction) -
        (mbc1_u1 + mbc2_u1) - sorption_doc1
    d_doc2 <- mortality_input * 0.2 * isolated_fraction +
        enzyme_degradation * isolated_fraction +
        enzyme_death * isolated_fraction +
        (break_doc + desorption_doc) * isolated_fraction -
        (mbc1_u2 + mbc2_u2) - sorption_doc2
    d_doc <- d_doc1 + d_doc2 + d_p_doc
    d_noc1 <- mortality_input * 0.8 *
            (1.0 - isolated_fraction) +
        (break_noc + desorption_noc) * (1.0 - isolated_fraction) -
        enzyme_degradation * (1.0 - isolated_fraction) - sorption_noc1
    d_noc2 <- mortality_input * 0.8 * isolated_fraction +
        (break_noc + desorption_noc) * isolated_fraction -
        enzyme_degradation * isolated_fraction - sorption_noc2
    d_noc <- d_noc1 + d_noc2 + d_p_noc
    respiration <- mbc1_respiration + mbc2_respiration

    pools[step + 1L, 3L] <- pools[step, 3L] + d_mbc
    pools[step + 1L, 4L] <- pools[step, 4L] + d_mbc1
    pools[step + 1L, 5L] <- pools[step, 5L] + d_mbc2
    pools[step + 1L, 6L] <- pools[step, 6L] + d_enz
    pools[step + 1L, 7L] <- pools[step, 7L] + d_enz1
    pools[step + 1L, 8L] <- pools[step, 8L] + d_enz2
    pools[step + 1L, 9L] <- pools[step, 9L] + d_p_doc
    pools[step + 1L, 10L] <- pools[step, 10L] + d_p_noc
    pools[step + 1L, 11L] <- pools[step, 11L] + d_doc1
    pools[step + 1L, 12L] <- pools[step, 12L] + d_doc2
    pools[step + 1L, 13L] <- pools[step, 13L] + d_doc
    pools[step + 1L, 14L] <- pools[step, 14L] + d_noc1
    pools[step + 1L, 15L] <- pools[step, 15L] + d_noc2
    pools[step + 1L, 16L] <- pools[step, 16L] + d_noc
    pools[step + 1L, 17L] <- respiration
    pools[step + 1L, 18L] <- enzyme_degradation
  }

  pools
}
