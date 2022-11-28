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
                            full_posterior,
                            predictor_shift,
                            relative) {
  ## Auxiliary
  dyad_names_gain_vec <-
    y_structure$dyad[y_structure$type == "gain"]
  dyad_names_loss_vec <-
    y_structure$dyad[y_structure$type == "loss"]
  dyad_names <-
    sort(unique(c(
      dyad_names_gain_vec, dyad_names_loss_vec
    )))
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
    (((pr_obj_1[, loss_vec, drop = FALSE] -
      pr_obj_0[, loss_vec, drop = FALSE]) %>%
      apply(1, sum)) / denom) %>%
    `/`(predictor_shift)

  gains <-
    (((pr_obj_1[, gain_vec, drop = FALSE] -
      pr_obj_0[, gain_vec, drop = FALSE]) %>%
      apply(1, sum)) / denom) %>%
    `/`(predictor_shift)

  balance <- ((((pr_obj_1[, gain_vec, drop = FALSE] -
    pr_obj_1[, loss_vec, drop = FALSE]) -
    (pr_obj_0[, gain_vec, drop = FALSE] -
      pr_obj_0[, loss_vec, drop = FALSE])
  ) %>%
    apply(1, sum)) / denom) %>%
    `/`(predictor_shift)

  volume <- ((((pr_obj_1[, gain_vec, drop = FALSE] +
    pr_obj_1[, loss_vec, drop = FALSE]) -
    (pr_obj_0[, gain_vec, drop = FALSE] +
      pr_obj_0[, loss_vec, drop = FALSE])
  ) %>%
    apply(1, sum)) / denom) %>%
    `/`(predictor_shift)

  retention <-
    ((((
      pr_obj_1[, retain, drop = FALSE] -
        pr_obj_0[, retain, drop = FALSE]
    ) / denom) %>%
      apply(1, sum)) / denom) %>%
    `/`(predictor_shift)

  ## Dyadic quantities
  dyadic_losses <-
    dyadic_gains <- dyadic_balances <- dyadic_volumes <- list()
  for (d in dyad_names) {
    dyadic_losses[[d]] <-
      (((pr_obj_1[, loss[[d]], drop = FALSE] -
        pr_obj_0[, loss[[d]], drop = FALSE]) %>%
        apply(1, sum)) / denom) %>%
      `/`(predictor_shift)

    dyadic_gains[[d]] <-
      (((pr_obj_1[, gain[[d]], drop = FALSE] -
        pr_obj_0[, gain[[d]], drop = FALSE]) %>%
        apply(1, sum)) / denom) %>%
      `/`(predictor_shift)

    dyadic_balances[[d]] <-
      ((((pr_obj_1[, gain[[d]], drop = FALSE] -
        pr_obj_1[, loss[[d]], drop = FALSE]) -
        (pr_obj_0[, gain[[d]], drop = FALSE] -
          pr_obj_0[, loss[[d]], drop = FALSE])
      ) %>%
        apply(1, sum)) / denom) %>%
      `/`(predictor_shift)

    dyadic_volumes[[d]] <-
      ((((pr_obj_1[, gain[[d]], drop = FALSE] +
        pr_obj_1[, loss[[d]], drop = FALSE]) -
        (pr_obj_0[, gain[[d]], drop = FALSE] +
          pr_obj_0[, loss[[d]], drop = FALSE])
      ) %>%
        apply(1, sum)) / denom) %>%
      `/`(predictor_shift)
  }

  ## Return
  ## Average effects
  if (full_posterior) {
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
  } else {
    me_qois <- list(
      losses = losses %>%
        quantile(posterior_quantiles),
      gains = gains %>%
        quantile(posterior_quantiles),
      balance = balance %>%
        quantile(posterior_quantiles),
      volume = volume %>%
        quantile(posterior_quantiles),
      retention = retention %>%
        quantile(posterior_quantiles),
      dyadic_losses = dyadic_losses %>%
        lapply(quantile, posterior_quantiles),
      dyadic_gains = dyadic_gains %>%
        lapply(quantile, posterior_quantiles),
      dyadic_balances = dyadic_balances %>%
        lapply(quantile, posterior_quantiles),
      dyadic_volumes = dyadic_volumes %>%
        lapply(quantile, posterior_quantiles)
    )
  }

  ## Posterior probability of positive effect
  prob_pos <- list(
    losses = mean(losses > 0),
    gains = mean(gains > 0),
    balance = mean(balance > 0),
    volume = mean(volume > 0),
    retention = mean(retention > 0),
    dyadic_losses = dyadic_losses %>% lapply(function(x)
      mean(x > 0)),
    dyadic_gains = dyadic_gains %>% lapply(function(x)
      mean(x > 0)),
    dyadic_balances = dyadic_balances %>% lapply(function(x)
      mean(x > 0)),
    dyadic_volumes = dyadic_volumes %>% lapply(function(x)
      mean(x > 0))
  )

  return(
    list(
      me_qois = me_qois,
      prob_pos = prob_pos
    )
  )
}
