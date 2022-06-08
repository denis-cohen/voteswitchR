#' @title compute_ce_qois
#'
#' @description Internal function to compute substantively
#' meaningful quantities of interest (conditional expectations)
#'
#' @noRd

compute_ce_qois <- function(ce_obj,
                            y_structure,
                            base,
                            posterior_quantiles,
                            full_posterior,
                            relative) {
  ## Auxiliary
  dyad_names_gain_vec <- y_structure$dyad[y_structure$type == "gain"]
  dyad_names_loss_vec <- y_structure$dyad[y_structure$type == "loss"]
  dyad_names <- sort(unique(c(dyad_names_gain_vec, dyad_names_loss_vec)))
  gain_vec <- y_structure$pos[y_structure$type == "gain"]
  gain <- tapply(gain_vec, dyad_names_gain_vec, unique)
  loss_vec <- y_structure$pos[y_structure$type == "loss"]
  loss <- tapply(loss_vec, dyad_names_loss_vec, unique)
  retain <- y_structure$pos[y_structure$type == "retain"]
  sup_t <- c(retain, gain_vec)
  sup_tm1 <- c(retain, loss_vec)

  ## Denominator
  if (relative) {
    if (base == "t") {
      denom <- ce_obj[, sup_t, ] %>%
        apply(c(1, 3), sum)
    } else if (base == "tm1") {
      denom <- ce_obj[, sup_tm1, ] %>%
        apply(c(1, 3), sum)
    } else if (base == "avg") {
      denom <- 0.5 * (ce_obj[, sup_t, ] + ce_obj[, sup_tm1, ]) %>%
        apply(c(1, 3), sum)
    }
  } else {
    denom <- 1.0
  }

  ## ---- Overall quantities ---
  losses <-
    ((ce_obj[, loss_vec, , drop = FALSE] %>% apply(c(1, 3), sum)) / denom)

  gains <-
    ((ce_obj[, gain_vec, , drop = FALSE] %>% apply(c(1, 3), sum)) / denom)

  balance <-
    (((ce_obj[, gain_vec, , drop = FALSE] - ce_obj[, loss_vec, ]) %>%
      apply(c(1, 3), sum)) / denom)

  volume <-
    (((ce_obj[, gain_vec, , drop = FALSE] + ce_obj[, loss_vec, ]) %>%
      apply(c(1, 3), sum)) / denom)

  retention <-
    ((ce_obj[, retain, , drop = FALSE] %>% apply(c(1, 3), sum)) / denom)

  ## Dyadic quantities
  dyadic_losses <-
    dyadic_gains <- dyadic_balances <- dyadic_volumes <- list()
  for (d in dyad_names) {
    dyadic_losses[[d]] <-
      ((ce_obj[, loss[[d]], , drop = FALSE] %>%
        apply(c(1, 3), sum)) / denom)

    dyadic_gains[[d]] <-
      ((ce_obj[, gain[[d]], , drop = FALSE] %>%
        apply(c(1, 3), sum)) / denom)

    dyadic_balances[[d]] <-
      (((ce_obj[, gain[[d]], , drop = FALSE] -
        ce_obj[, loss[[d]], , drop = FALSE]) %>%
        apply(c(1, 3), sum)) / denom)

    dyadic_volumes[[d]] <-
      (((ce_obj[, gain[[d]], , drop = FALSE] +
        ce_obj[, loss[[d]], , drop = FALSE]) %>%
        apply(c(1, 3), sum)) / denom)
  }

  ## Return
  if (full_posterior) {
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
  } else {
    ce_qois <- list(
      losses = losses %>%
        apply(2, quantile, posterior_quantiles),
      gains = gains %>%
        apply(2, quantile, posterior_quantiles),
      balance = balance %>%
        apply(2, quantile, posterior_quantiles),
      volume = volume %>%
        apply(2, quantile, posterior_quantiles),
      retention = retention %>%
        apply(2, quantile, posterior_quantiles),
      dyadic_losses = dyadic_losses %>%
        lapply(apply, 2, quantile, posterior_quantiles),
      dyadic_gains = dyadic_gains %>%
        lapply(apply, 2, quantile, posterior_quantiles),
      dyadic_balances = dyadic_balances %>%
        lapply(apply, 2, quantile, posterior_quantiles),
      dyadic_volumes = dyadic_volumes %>%
        lapply(apply, 2, quantile, posterior_quantiles)
    )
  }

  return(ce_qois)
}
