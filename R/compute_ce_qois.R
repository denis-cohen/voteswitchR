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
                            relative) {
  ## Auxiliary
  `%>%` <- magrittr::`%>%`
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
    ((ce_obj[, loss_vec,] %>% apply(c(1, 3), sum)) / denom) %>%
    apply(2, quantile, posterior_quantiles)

  gains <-
    ((ce_obj[, gain_vec,] %>% apply(c(1, 3), sum)) / denom) %>%
    apply(2, quantile, posterior_quantiles)

  balance <- (((ce_obj[, gain_vec, ] - ce_obj[, loss_vec, ]) %>%
                 apply(c(1, 3), sum)) / denom) %>%
    apply(2, quantile, posterior_quantiles)

  volume <- (((ce_obj[, gain_vec, ] + ce_obj[, loss_vec, ]) %>%
                apply(c(1, 3), sum)) / denom) %>%
    apply(2, quantile, posterior_quantiles)

  retention <- (ce_obj[, retain, ] / denom) %>%
    apply(2, quantile, posterior_quantiles)

  ## Dyadic quantities
  dyadic_losses <-
    dyadic_gains <- dyadic_balances <- dyadic_volumes <- list()
  for (d in dyad_names) {
    dyadic_losses[[d]] <-
      ((ce_obj[, loss[[d]], , drop = FALSE] %>%
          apply(c(1, 3), sum)) / denom) %>%
      apply(2, quantile, posterior_quantiles)

    dyadic_gains[[d]] <-
      ((ce_obj[, gain[[d]], , drop = FALSE] %>%
          apply(c(1, 3), sum)) / denom) %>%
      apply(2, quantile, posterior_quantiles)

    dyadic_balances[[d]] <-
      (((ce_obj[, gain[[d]], , drop = FALSE] -
           ce_obj[, loss[[d]], , drop = FALSE]) %>%
          apply(c(1, 3), sum)) / denom) %>%
      apply(2, quantile, posterior_quantiles)

    dyadic_volumes[[d]] <-
      (((ce_obj[, gain[[d]], , drop = FALSE] +
           ce_obj[, loss[[d]], , drop = FALSE]) %>%
          apply(c(1, 3), sum)) / denom) %>%
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
