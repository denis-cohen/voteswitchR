## quantities of interest (conditional expectations)
compute_ce_qois <- function(ce_obj,
                            y_structure,
                            base,
                            posterior_quantiles,
                            relative) {
  ## Auxiliary
  dyad_names <- y_structure$dyads$dyad_names
  gain <- y_structure$dyads$gain
  loss <- y_structure$dyads$loss
  retain <- y_structure$retain
  resid <- y_structure$resid
  sup_t <- c(retain, gain)
  sup_tm1 <- c(retain, loss)
  
  ## Denominator
  if (relative) {
    if (base == "t") {
      denom <- ce_obj[, sup_t,] %>%
        apply(c(1, 3), sum)
    } else if (base == "tm1") {
      denom <- ce_obj[, sup_tm1,] %>%
        apply(c(1, 3), sum)
    } else if (base == "avg") {
      denom <- 0.5 * (ce_obj[, sup_t,] + ce_obj[, sup_tm1,]) %>%
        apply(c(1, 3), sum)
    }
  } else {
    denom <- 1.0
  }
  
  ## ---- Overall quantities ---
  losses <-
    ((ce_obj[, loss,] %>% apply(c(1, 3), sum)) / denom) %>%
    apply(2, quantile, posterior_quantiles)
  
  gains <-
    ((ce_obj[, gain,] %>% apply(c(1, 3), sum)) / denom) %>%
    apply(2, quantile, posterior_quantiles)
  
  balance <- (((ce_obj[, gain, ] - ce_obj[, loss, ]) %>%
                 apply(c(1, 3), sum)) / denom) %>%
    apply(2, quantile, posterior_quantiles)
  
  volume <- (((ce_obj[, gain, ] + ce_obj[, loss, ]) %>%
                apply(c(1, 3), sum)) / denom) %>%
    apply(2, quantile, posterior_quantiles)
  
  retention <- (ce_obj[, retain, ] / denom) %>%
    apply(2, quantile, posterior_quantiles)
  
  ## Dyadic quantities
  dyadic_losses <-
    dyadic_gains <- dyadic_balances <- dyadic_volumes <- list()
  for (d in seq_along(gain)) {
    dyadic_losses[[dyad_names[d]]] <- (ce_obj[, loss[d],] / denom) %>%
      apply(2, quantile, posterior_quantiles)
    
    dyadic_gains[[dyad_names[d]]] <- (ce_obj[, gain[d],] / denom) %>%
      apply(2, quantile, posterior_quantiles)
    
    dyadic_balances[[dyad_names[d]]] <-
      ((ce_obj[, gain[d], ] - ce_obj[, loss[d], ]) / denom) %>%
      apply(2, quantile, posterior_quantiles)
    
    dyadic_volumes[[dyad_names[d]]] <-
      ((ce_obj[, gain[d],] + ce_obj[, loss[d],]) / denom) %>%
      apply(2, quantile, posterior_quantiles)
  }
  
  ## Return
  ce_qois <- list(
    losses = losses,
    gains = gains,
    balance = balance,
    volume = volume,
    retention = retention,
    dyadic_losses = dyadic_losses,
    dyadic_gains = dyadic_gains,
    dyadic_balances = dyadic_balances,
    dyadic_volumes = dyadic_volumes
  )
  return(ce_qois)
}