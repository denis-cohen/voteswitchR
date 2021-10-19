#' @title compute_me_qois
#'
#' @description Internal function to compute substantively
#' meaningful quantities of interest (average marginal effects)
#'
#' @noRd

compute_me_qois <- function(pr_obj_0,
                            pr_obj_1,
                            y_structure,
                            base,
                            posterior_quantiles,
                            predictor_shift,
                            relative) {
  ## Auxiliary
  dyad_names <- y_structure$dyads$dyad_names
  gain <- y_structure$dyads$gain
  loss <- y_structure$dyads$loss
  retain <- y_structure$retain
  sup_t <- c(retain, gain)
  sup_tm1 <- c(retain, loss)

  ## Denominator
  if (relative) {
    if (base == "t") {
      denom <- .5 * (pr_obj_0[, sup_t] + pr_obj_1[, sup_t]) %>%
        apply(1, sum)
    } else if (base == "tm1") {
      denom <- .5 * (pr_obj_0[, sup_tm1] + pr_obj_1[, sup_tm1]) %>%
        apply(1, sum)
    } else if (base == "avg") {
      denom <- .25 * (pr_obj_0[, sup_t] + pr_obj_0[, sup_tm1] +
                        pr_obj_1[, sup_t] + pr_obj_1[, sup_tm1]) %>%
        apply(1, sum)
    }
  } else {
    denom <- 1.0
  }

  ## ---- Overall quantities ---
  losses <-
    (((pr_obj_1[, loss] - pr_obj_0[, loss]) %>%
        apply(1, sum)) / denom) %>%
    quantile(posterior_quantiles) %>%
    `/`(predictor_shift)

  gains <-
    (((pr_obj_1[, gain] - pr_obj_0[, gain]) %>%
        apply(1, sum)) / denom) %>%
    quantile(posterior_quantiles) %>%
    `/`(predictor_shift)

  balance <- ((((pr_obj_1[, gain] - pr_obj_1[, loss]) -
                  (pr_obj_0[, gain] - pr_obj_0[, loss])
  ) %>%
    apply(1, sum)) / denom) %>%
    quantile(posterior_quantiles) %>%
    `/`(predictor_shift)

  volume <- ((((pr_obj_1[, gain] + pr_obj_1[, loss]) -
                 (pr_obj_0[, gain] + pr_obj_0[, loss])
  ) %>%
    apply(1, sum)) / denom) %>%
    quantile(posterior_quantiles) %>%
    `/`(predictor_shift)

  retention <-
    ((pr_obj_1[, retain] - pr_obj_0[, retain]) / denom) %>%
    quantile(posterior_quantiles) %>%
    `/`(predictor_shift)

  ## Dyadic quantities
  dyadic_losses <-
    dyadic_gains <- dyadic_balances <- dyadic_volumes <- list()
  for (d in seq_along(gain)) {
    dyadic_losses[[dyad_names[d]]] <-
      ((pr_obj_1[, loss[d]] - pr_obj_0[, loss[d]]) / denom) %>%
      quantile(posterior_quantiles) %>%
      `/`(predictor_shift)

    dyadic_gains[[dyad_names[d]]] <-
      ((pr_obj_1[, gain[d]] - pr_obj_0[, gain[d]]) / denom) %>%
      quantile(posterior_quantiles) %>%
      `/`(predictor_shift)

    dyadic_balances[[dyad_names[d]]] <-
      (((pr_obj_1[, gain[d]] - pr_obj_1[, loss[d]]) -
          (pr_obj_0[, gain[d]] - pr_obj_0[, loss[d]])) / denom) %>%
      quantile(posterior_quantiles) %>%
      `/`(predictor_shift)

    dyadic_volumes[[dyad_names[d]]] <-
      (((pr_obj_1[, gain[d]] + pr_obj_1[, loss[d]]) -
          (pr_obj_0[, gain[d]] + pr_obj_0[, loss[d]])) / denom) %>%
      quantile(posterior_quantiles) %>%
      `/`(predictor_shift)
  }

  ## Return
  me_qois <- list(
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
  return(me_qois)
}