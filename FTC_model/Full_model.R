Full_kernel <- function(initial, temp, p) {
  pools <- c("MBC", "ENZ", "SOCf", "SOCp", "DOCf", "DOCp")
  state <- as.numeric(initial[pools])
  k_temp <- p[["k_temp"]]
  if (is.null(k_temp)) k_temp <- p[["k_freeze"]]
  freeze <- -expm1(-k_temp * pmax(-temp, 0))
  warming <- c(FALSE, diff(temp) > 0) & temp >= 0 & temp <= 10
  thaw <- ifelse(warming, expm1(k_temp * temp), 0)
  interpolate <- function(endpoints, weight) {
    endpoints[[1L]] + weight * (endpoints[[2L]] - endpoints[[1L]])
  }
  weight <- (temp + 10) / 20
  cue <- interpolate(p$cue, weight)
  doc_sorption <- interpolate(p$doc_sorption, weight)
  doc_desorption <- interpolate(p$doc_desorption, weight)
  soc_sorption <- interpolate(p$soc_sorption, weight)
  soc_desorption <- interpolate(p$soc_desorption, weight)
  half_saturation <- interpolate(p$half_saturation_fraction, weight)
  doc_half <- initial[["DOCf"]] * half_saturation
  soc_half <- initial[["SOCf"]] * half_saturation
  breakup_coefficient <- function(values) {
    if ("-10" %in% names(values)) {
      return(interpolate(values[c("-10", "10")], weight))
    }
    interpolate(values, temp / 10)
  }
  doc_breakup <- breakup_coefficient(p$doc_breakup) * thaw
  soc_breakup <- breakup_coefficient(p$soc_breakup) * thaw

  gas_constant <- 0.008314462618
  arrhenius <- function(energy) exp(-energy / (gas_constant * (temp + 273)))
  uptake_rate <- p$doc_uptake_rate * arrhenius(p$uptake_activation_energy)
  catalysis_rate <- p$soc_catalysis_rate * arrhenius(p$catalysis_activation_energy)
  saturation <- function(substrate, half) {
    denominator <- half + substrate
    denominator[substrate == 0] <- 1
    substrate / denominator
  }

  states <- matrix(NA_real_, length(temp), length(pools), dimnames = list(NULL, pools))
  states[1L, ] <- state
  derived <- c("MBCact", "MBCdorm", "ENZact", "ENZinact", "DOCf_acc", "DOCf_iso",
               "SOCf_acc", "SOCf_iso")
  partition <- matrix(NA_real_, length(temp), length(derived), dimnames = list(NULL, derived))
  processes <- c("DOCupt", "SOCcat", "MBCdeath", "ENZprod", "ENZdecay", "DOCsor",
                 "DOCdes", "SOCsor", "SOCdes", "DOCbreak", "SOCbreak", "Rs")
  flux <- matrix(NA_real_, length(temp), length(processes), dimnames = list(NULL, processes))

  partition_pools <- function(s, freeze_fraction) {
    dormant <- s[["MBC"]] * p$dormancy_fraction * freeze_fraction
    inactive <- s[["ENZ"]] * p$enzyme_inactivation_fraction * freeze_fraction
    isolated_fraction <- p$isolation_fraction * freeze_fraction
    doc_isolated <- s[["DOCf"]] * isolated_fraction
    soc_isolated <- s[["SOCf"]] * isolated_fraction
    c(s[["MBC"]] - dormant, dormant, s[["ENZ"]] - inactive, inactive,
      s[["DOCf"]] - doc_isolated, doc_isolated, s[["SOCf"]] - soc_isolated, soc_isolated)
  }
  partition[1L, ] <- partition_pools(states[1L, ], freeze[1L])
  for (i in seq_len(length(temp) - 1L)) {
    s <- states[i, ]
    mbc_active <- partition[i, "MBCact"]
    enz_active <- partition[i, "ENZact"]
    doc_accessible <- partition[i, "DOCf_acc"]
    soc_accessible <- partition[i, "SOCf_acc"]

    uptake <- uptake_rate[i] * mbc_active * saturation(doc_accessible, doc_half[i])
    catalysis <- catalysis_rate[i] * enz_active * saturation(soc_accessible, soc_half[i])
    death <- (p$microbial_death_rate + p$freeze_mortality_rate * freeze[i]) * mbc_active
    production <- p$enzyme_production_rate * mbc_active
    decay <- p$enzyme_decay_rate * enz_active
    respiration <- (1 - cue[i]) * uptake

    doc_sorbed <- doc_sorption[i] * s[["DOCf"]]
    doc_desorbed <- doc_desorption[i] * s[["DOCp"]]
    soc_sorbed <- soc_sorption[i] * s[["SOCf"]]
    soc_desorbed <- soc_desorption[i] * s[["SOCp"]]
    doc_released <- doc_breakup[i] * s[["DOCp"]]
    soc_released <- soc_breakup[i] * s[["SOCp"]]
    flux[i, ] <- c(uptake, catalysis, death, production, decay, doc_sorbed,
                   doc_desorbed, soc_sorbed, soc_desorbed, doc_released, soc_released, respiration)

    death_to_doc <- death * p$mortality_doc_fraction
    net_soc_release <- soc_desorbed + soc_released - soc_sorbed
    net_doc_release <- doc_desorbed + doc_released - doc_sorbed
    states[i + 1L, ] <- s + c(
      MBC = cue[i] * uptake - death - production,
      ENZ = production - decay,
      SOCf = death - death_to_doc - catalysis + net_soc_release,
      SOCp = -net_soc_release,
      DOCf = catalysis + death_to_doc + decay - uptake + net_doc_release,
      DOCp = -net_doc_release
    )
    partition[i + 1L, ] <- partition_pools(states[i + 1L, ], freeze[i + 1L])
  }
  data.frame(Hour = seq_along(temp) - 1L, Temp = temp, states, partition, flux)
}
