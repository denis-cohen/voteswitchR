#' @title Compute MAVCL quantities of interest
#'
#' @description Computes quantities of interest such as conditional expectations
#' and average marginal effects/first differences from a MAVCL estimation
#' object.
#'
#' @param mavcl_object An estimation object of class \code{mavcl_est}.
#' @param y_structure Structure of switching patterns, as generated
#' by [recode_switches()].
#' @param posterior_quantiles A numeric vector of quantile values for the
#' posterior quantile summaries.
#' @param full_posterior Logical; if \code{TRUE}, the full posterior draws
#' will be return alongside the posterior quantile summaries.
#' @param len_continuous_sequence Suggested length of the value sequence for
#' continuous predictors and/or moderators.
#' @param conditional_expectation Logical; if \code{TRUE}, conditional
#' expectations (a.k.a. "expected values" or "predicitions") will be returned.
#' @param average_marginal_effect Logical; if \code{TRUE}, average marginal
#' effects (continuous main predictor) or average first difference (categorical
#' main predictor) will be returned.
#' @param relative Logical; if \code{TRUE}, the returned quantities of interest
#' are normalized by \code{base}.
#' @param ame_shift Specifies type of (\code{tiny} or numerical) AME shift in
#' predictor.
#' @param base Specifies whether the denominator for the normalization of
#' relative quantities of interest. Can be \code{"t"} for parties' electorates
#' at t, \code{"tm1"} for parties' electorates at t - 1, or \code{"avg"} for
#' parties' average inter-election electorates.
#' @param atmeans Logical; if \code{TRUE}, quantities of interest will be
#' calculated for an average observation. If \code{FALSE}, quantities will be
#' calculated at the observed values of all observations and then averaged.
#' @param re_null Logical; if \code{TRUE},only the "fixed" portion of the model
#' coefficients will be used and the "random" portion discarded.
#'
#' @return Returns a nested list of class \code{voteswitchR_qoi} that contains
#' the requested quantities.
#'
#' @export

compute_qoi <- function(mavcl_object,
                        y_structure,
                        posterior_quantiles = c(.5, .025, .975),
                        full_posterior = FALSE,
                        len_continuous_sequence = 21L,
                        conditional_expectation = TRUE,
                        average_marginal_effect = TRUE,
                        ame_shift = "tiny",
                        base = c("t", "tm1", "avg"),
                        relative = TRUE,
                        atmeans = FALSE,
                        re_null = FALSE) {

  ## ---- Warnings ----
  if (base == "tm1") {
    warning("Quantities of interest for new parties will be NaN.")
  }

  if (atmeans & !re_null) {
    warning("Option 'atmeans = TRUE' ist only available with 're_null = TRUE'")
    cat("\n")
    prompt_text <-
      paste0("Set 're_null = TRUE'?
             Press ENTER to accept, type something to decline.")
    accept <- is.null(readline(prompt = prompt_text))
    if (accept) {
      re_null <- TRUE
    } else {
      error("Please change the function inputs.")
    }
  }

  ## ---- Attach MAVCL object ----
  main_predictor <- mavcl_object$main_predictor
  predictor_continuous <- mavcl_object$predictor_continuous
  moderator <- mavcl_object$moderator
  moderator_continuous <- mavcl_object$moderator_continuous
  is_imputed <- mavcl_object$is_imputed
  baseline <- mavcl_object$baseline

  if (!predictor_continuous) {
    predictor_levels <- mavcl_object$predictor_levels
  }
  if (!is.null(moderator) & !moderator_continuous) {
    moderator_levels <- mavcl_object$moderator_levels
  }

  ## Update y_structure to ensure baseline is in final position
  y_structure <- dplyr::bind_rows(
    y_structure %>%
      dplyr::filter(switch != baseline),
    y_structure %>%
      dplyr::filter(switch == baseline),
  ) %>%
    dplyr::mutate(pos = dplyr::row_number())

  ## Data objects and auxiliaries
  X <- mavcl_object$data[[1]]$X
  Y <- mavcl_object$data[[1]]$Y
  D <- mavcl_object$data[[1]]$D
  V <- mavcl_object$data[[1]]$V
  pars <- mavcl_object$pars
  pars_def <- c("beta", "nu")
  pars_ext <- c(pars[startsWith(pars, "nu_")])
  pars_all <- c(pars_def, pars_ext)
  which_empty <- Y < 0
  num_cat <- ncol(Y)
  num_catm1 <- ncol(Y) - 1L
  num_obs <- nrow(Y)
  num_coef <- ncol(X)
  x_names <- colnames(X)
  y_names <- colnames(Y)

  ## Estimates
  start <- seq(1, by = D, length = num_cat - 1L)
  if (class(mavcl_object$estimates) == "stanfit") {
    est <- rstan::extract(mavcl_object$estimates, pars = pars_all)
    if (D > 1) {
      est$nu <- lapply(start, function(i, array, D) {
        array[, , i:(i + D - 1)]
      },
      array = est$nu, D = D
      )
    }
  } else if (class(mavcl_object$estimates) == "list" &
    class(mavcl_object$estimates[[1]]) == "stanfit") {
    est <- list()
    for (p in pars_all) {
      est[[p]] <- do.call(abind::abind, c(
        sapply(mavcl_object$estimates,
          rstan::extract,
          pars = p
        ),
        along = 1L
      ))
    }
    if (D > 1) {
      est$nu <- lapply(start, function(i, array, D) {
        array[, , i:(i + D - 1)]
      },
      array = est$nu, D = D
      )
    }
  }
  num_sim <- dim(est$beta)[1]

  ## Predictor
  if (predictor_continuous) {
    ## Predictor position
    which_predictor <- which(x_names == main_predictor)

    ## Predictor range
    range_predictor <- range(X[, which_predictor])

    ## Predictor value sequence
    predictor_sequence <-
      pretty(range_predictor, n = len_continuous_sequence)

    ## Update length of predictor sequence
    len_predictor_sequence <- length(predictor_sequence)

    ## AME shift in predictor
    if (ame_shift == "tiny") {
      predictor_shift <- sd(X[, which_predictor]) * .001
    } else if (is.numeric(ame_shift)) {
      predictor_shift <- ame_shift
    }
  } else {
    pred_x_names <-
      c("(Intercept)", paste0(main_predictor, predictor_levels[-1]))
    which_predictor <-
      sapply(
        pred_x_names,
        function(x) {
          which(x_names == x)
        }
      )
    num_cat_predictor <- length(which_predictor)
    predictor_shift <- 1L
  }

  ## Moderator
  if (!is.null(moderator)) {
    if (moderator_continuous) {
      ## Moderator position
      which_moderator <- which(x_names == moderator)

      ## Moderator range
      range_moderator <- range(X[, which_moderator])

      ## Moderator value sequence
      moderator_sequence <-
        pretty(range_moderator, n = len_continuous_sequence)

      ## Update length of moderator sequence
      len_moderator_sequence <- length(moderator_sequence)

      ## Product term position
      which_prod_term <- which(x_names == paste(main_predictor,
        moderator,
        sep = ":"
      ))
      which_predictors <- c(
        which_predictor,
        which_moderator,
        which_prod_term
      )
    } else {
      num_cat_moderator <- length(moderator_levels)
      mod_x_names <-
        c("(Intercept)", paste0(moderator, moderator_levels[-1]))
      which_moderator <-
        sapply(mod_x_names, function(x) {
          which(x_names == x)
        })

      if (predictor_continuous) {
        ## Product term positions
        which_prod_term <-
          sapply(
            x_names[startsWith(x_names, main_predictor) &
              grepl(moderator, x_names)],
            function(x) {
              which(x_names == x)
            }
          )
        which_predictors <- c(
          which_predictor,
          which_prod_term
        )
      } else {
        pred_mod_names_tmp <-
          c(
            mod_x_names,
            paste0(main_predictor, predictor_levels[-1]),
            paste(
              rep(
                paste0(main_predictor, predictor_levels[-1]),
                num_cat_predictor - 1L
              ),
              rep(paste0(moderator, moderator_levels[-1]),
                each = num_cat_predictor - 1L
              ),
              sep = ":"
            )
          )
        pred_mod_names <-
          matrix(NA_character_, num_cat_predictor, num_cat_moderator)
        rownames(pred_mod_names) <- predictor_levels
        colnames(pred_mod_names) <- moderator_levels
        for (j in seq_len(num_cat_predictor)) {
          if (j == 1) {
            pred_mod_names[j, ] <-
              pred_mod_names_tmp[pred_mod_names_tmp %in% mod_x_names]
          } else {
            pred_mod_names[j, ] <-
              pred_mod_names_tmp[startsWith(pred_mod_names_tmp,
                                            pred_x_names[j])]
          }
        }

        which_predictors <- apply(pred_mod_names, 1:2, function(x) {
          which(x_names == x)
        })
      }
    }
  }


  ## ---- Predictor matrices ----
  ## Default
  if (atmeans) {
    x_default <- as.matrix(apply(X, 2, mean))
    num_replic <- 1L
  } else {
    x_default <- t(X)
    num_replic <- num_obs
  }

  ## ---- Linear predictor ----
  eta <- compute_eta(
    est,
    num_cat,
    num_catm1,
    x_default,
    re_null,
    num_sim,
    num_replic,
    D,
    pars_ext,
    V
  )

  ## ---- Conditional expectations ----
  if (conditional_expectation |
    (average_marginal_effect & !predictor_continuous)) {
    ## Copy of x_default
    x_tmp <- x_default

    if (is.null(moderator)) {
      if (predictor_continuous) {
        ## Initialize container
        ce_est <-
          array(NA, c(num_sim, num_cat, len_predictor_sequence))

        for (t in seq_along(predictor_sequence)) {
          ## Adjust matrix
          x_tmp[which_predictor, ] <- predictor_sequence[t]

          ## Compute eta
          eta <- compute_eta(
            est,
            num_cat,
            num_catm1,
            x_tmp,
            re_null,
            num_sim,
            num_replic,
            D,
            pars_ext,
            V
          )

          ## Compute conditional expectation
          for (s in seq_len(num_sim)) {
            ce_est[s, , t] <- compute_ce(eta, which_empty, s)
          }
        }

        ## Compute conditional expectation QOIs
        ce_qois <- compute_ce_qois(
          ce_obj = ce_est,
          y_structure = y_structure,
          base = base,
          posterior_quantiles = posterior_quantiles,
          full_posterior = full_posterior,
          relative = relative
        )
      } else {
        ## Initialize container
        ce_est <-
          array(NA, c(num_sim, num_cat, num_cat_predictor))

        ## Adjust matrices
        x_tmp <- replicate(num_cat_predictor, x_tmp)
        for (t in seq_len(num_cat_predictor)) {
          x_tmp[which_predictor, , t] <-
            as.numeric(seq_len(num_cat_predictor) %in% c(1, t)) %>%
            replicate(num_obs, .) %>%
            t()

          ## Compute eta
          eta <- compute_eta(
            est,
            num_cat,
            num_catm1,
            x_tmp[, , t],
            re_null,
            num_sim,
            num_replic,
            D,
            pars_ext,
            V
          )

          ## Compute conditional expectation
          for (s in seq_len(num_sim)) {
            ce_est[s, , t] <- compute_ce(eta, which_empty, s)
          }
        }

        ## Compute conditional expectation QOIs
        ce_qois <- compute_ce_qois(
          ce_obj = ce_est,
          y_structure = y_structure,
          base = base,
          posterior_quantiles = posterior_quantiles,
          full_posterior = full_posterior,
          relative = relative
        )

        ## Add predictor levels
        for (i in seq_along(ce_qois)) {
          if (is.matrix(ce_qois[[i]])) {
            colnames(ce_qois[[i]]) <- predictor_levels
          } else if (is.list(ce_qois[[i]])) {
            for (j in seq_along(ce_qois[[i]])) {
              colnames(ce_qois[[i]][[j]]) <- predictor_levels
            }
          }
        }
      }
    } else if (!is.null(moderator) & moderator_continuous) {
      if (predictor_continuous) {
        ## Initialize container
        ce_est <-
          array(
            NA,
            c(
              num_sim,
              num_cat,
              len_predictor_sequence,
              len_moderator_sequence
            )
          )

        for (t in seq_along(predictor_sequence)) {
          for (u in seq_along(moderator_sequence)) {
            ## Adjust matrix
            x_tmp[which_predictors, ] <- c(
              predictor_sequence[t],
              moderator_sequence[u],
              predictor_sequence[t] *
                moderator_sequence[u]
            )

            ## Compute eta
            eta <- compute_eta(
              est,
              num_cat,
              num_catm1,
              x_tmp,
              re_null,
              num_sim,
              num_replic,
              D,
              pars_ext,
              V
            )

            ## Compute conditional expectation
            for (s in seq_len(num_sim)) {
              ce_est[s, , t, u] <- compute_ce(eta, which_empty, s)
            }
          }
        }

        ## Compute conditional expectation QOIs
        dimnames(ce_est) <-
          list(NULL, y_names, predictor_sequence, moderator_sequence)

        ce_qois <- apply(ce_est,
          4,
          compute_ce_qois,
          y_structure = y_structure,
          base = base,
          posterior_quantiles = posterior_quantiles,
          full_posterior = full_posterior,
          relative = relative
        ) %>%
          reorder_qoi()
      } else {
        ## Initialize container
        ce_est <- array(NA, c(
          num_sim, num_cat, num_cat_predictor,
          len_moderator_sequence
        ))

        ## Adjust matrices
        x_tmp <- replicate(num_cat_predictor, x_tmp)
        for (t in seq_len(num_cat_predictor)) {
          for (u in seq_len(len_moderator_sequence)) {
            x_tmp[which_predictor, , t] <-
              as.numeric(seq_len(num_cat_predictor) %in% c(1, t)) %>%
              replicate(num_obs, .) %>%
              t()
            x_tmp[c(which_moderator, which_prod_term), , t] <-
              moderator_sequence[u] %*%
              as.numeric(seq_len(num_cat_predictor) %in% c(1, t)) %>%
              as.vector() %>%
              replicate(num_obs, .)

            ## Compute eta
            eta <- compute_eta(
              est,
              num_cat,
              num_catm1,
              x_tmp[, , t],
              re_null,
              num_sim,
              num_replic,
              D,
              pars_ext,
              V
            )

            ## Compute conditional expectation
            for (s in seq_len(num_sim)) {
              ce_est[s, , t, u] <- compute_ce(eta, which_empty, s)
            }
          }
        }

        ## Compute conditional expectation QOIs
        dimnames(ce_est) <-
          list(NULL, y_names, predictor_levels, moderator_sequence)

        ce_qois <- apply(ce_est,
          4,
          compute_ce_qois,
          y_structure = y_structure,
          base = base,
          posterior_quantiles = posterior_quantiles,
          full_posterior = full_posterior,
          relative = relative
        ) %>%
          reorder_qoi()
      }
    } else if (!is.null(moderator) &
      !moderator_continuous) {
      if (predictor_continuous) {
        ## Initialize containers
        ce_est <- array(
          NA,
          c(
            num_sim,
            num_cat,
            len_predictor_sequence,
            num_cat_moderator
          )
        )

        for (k in seq_along(predictor_sequence)) {
          for (t in seq_len(num_cat_moderator)) {
            ## Adjust matrix
            x_tmp <- x_default
            x_tmp[which_moderator, ] <-
              as.numeric(seq_len(num_cat_moderator) %in% c(1, t)) %>%
              replicate(num_replic, .)
            x_tmp[which_predictors, ] <-
              (as.numeric(seq_len(num_cat_moderator) %in% c(1, t)) *
                predictor_sequence[k]) %>%
              replicate(num_replic, .)

            ## Compute eta
            eta <- compute_eta(
              est,
              num_cat,
              num_catm1,
              x_tmp,
              re_null,
              num_sim,
              num_replic,
              D,
              pars_ext,
              V
            )

            ## Compute conditional expectation and cumulative expectations
            for (s in seq_len(num_sim)) {
              ce_est[s, , k, t] <- compute_ce(eta, which_empty, s)
            }
          }
        }
      } else {
        ## Initialize containers
        ce_est <- array(
          NA,
          c(
            num_sim,
            num_cat,
            num_cat_predictor,
            num_cat_moderator
          )
        )

        for (k in seq_len(num_cat_predictor)) {
          for (t in seq_len(num_cat_moderator)) {
            ## Adjust matrix
            x_tmp <- x_default
            x_tmp[which_predictors[, 1], ] <-
              as.numeric(which_predictors[, 1] %in% c(1, k)) %>%
              replicate(num_replic, .)
            x_tmp[which_predictors[1, ], ] <-
              as.numeric(which_predictors[1, ] %in%
                c(1, which_predictors[, t])) %>%
              replicate(num_replic, .)
            x_tmp[as.vector(which_predictors[2:num_cat_predictor,
                                             2:num_cat_moderator]), ] <- 0L
            if (k >= 2 & t >= 2) {
              x_tmp[which_predictors[k, t], ] <- 1L
            }

            ## Compute eta
            eta <- compute_eta(
              est,
              num_cat,
              num_catm1,
              x_tmp,
              re_null,
              num_sim,
              num_replic,
              D,
              pars_ext,
              V
            )

            ## Compute conditional expectation and cumulative expectations
            for (s in seq_len(num_sim)) {
              ce_est[s, , k, t] <- compute_ce(eta, which_empty, s)
            }
          }
        }
      }

      ## Compute conditional expectation QOIs
      if (predictor_continuous) {
        dimnames(ce_est) <-
          list(NULL, y_names, predictor_sequence, moderator_levels)
      } else {
        dimnames(ce_est) <-
          list(NULL, y_names, predictor_levels, moderator_levels)
      }

      ce_qois <- apply(ce_est,
        4,
        compute_ce_qois,
        y_structure = y_structure,
        base = base,
        posterior_quantiles = posterior_quantiles,
        full_posterior = full_posterior,
        relative = relative
      ) %>%
        reorder_qoi()
    }
  }

  ## ---- Average marginal effects ----
  if (average_marginal_effect) {
    ## Copy of x_default
    x_tmp_1 <- x_tmp_0 <- x_default

    ## Adjust matrix
    x_tmp_1[which_predictor, ] <-
      x_tmp_1[which_predictor, ] + predictor_shift

    if (is.null(moderator)) {
      if (predictor_continuous) {
        ## Counterfactual outcome-specific estimates
        eta_1 <- compute_eta(
          est,
          num_cat,
          num_catm1,
          x_tmp_1,
          re_null,
          num_sim,
          num_replic,
          D,
          pars_ext,
          V
        )
        eta_0 <- compute_eta(
          est,
          num_cat,
          num_catm1,
          x_tmp_0,
          re_null,
          num_sim,
          num_replic,
          D,
          pars_ext,
          V
        )

        pi_1 <- pi_0 <- array(NA, c(num_sim, num_cat))
        for (s in seq_len(num_sim)) {
          pi_1[s, ] <- compute_ce(eta_1, which_empty, s)
          pi_0[s, ] <- compute_ce(eta_0, which_empty, s)
        }

        ## Average marginal effects
        me_qois_aux <- compute_me_qois(pi_0,
          pi_1,
          y_structure = y_structure,
          base = base,
          posterior_quantiles = posterior_quantiles,
          full_posterior = full_posterior,
          predictor_shift = predictor_shift,
          relative = relative
        )
        me_prob_pos <- me_qois_aux$prob_pos
        me_qois <- me_qois_aux$me_qois
      } else {
        me_qois <- me_prob_pos <- list()

        for (i in seq_along(predictor_levels)) {
          for (j in seq_along(predictor_levels)) {
            if (i != j) {
              pair_name <- paste(predictor_levels[i],
                predictor_levels[j],
                sep = " - "
              )
              me_qois_aux <-
                compute_me_qois(
                  ce_est[, , i],
                  ce_est[, , j],
                  y_structure = y_structure,
                  base = base,
                  posterior_quantiles = posterior_quantiles,
                  full_posterior = full_posterior,
                  predictor_shift = 1,
                  relative = relative
                )
              me_prob_pos[[pair_name]] <- me_qois_aux$prob_pos
              me_qois[[pair_name]] <- me_qois_aux$me_qois
            }
          }
        }
      }
    } else if (!is.null(moderator) & moderator_continuous) {
      if (predictor_continuous) {
        ## Copy of x_default
        x_tmp_1 <- x_tmp_0 <- x_default

        ## Predictor columns
        pred_0 <- x_default[which_predictor, ]
        pred_1 <- pred_0 + predictor_shift

        ## Initialize container
        me_qois <- me_prob_pos <- list()

        for (u in seq_along(moderator_sequence)) {
          ## Moderator value as character
          u_char <- as.character(moderator_sequence[u])

          ## Adjust matrices
          x_tmp_0[which_predictors, ] <- rbind(
            pred_0,
            moderator_sequence[u],
            pred_0 * moderator_sequence[u]
          )
          x_tmp_1[which_predictors, ] <- rbind(
            pred_1,
            moderator_sequence[u],
            pred_1 * moderator_sequence[u]
          )

          ## Counterfactual outcome-specific estimates
          eta_1 <- compute_eta(
            est,
            num_cat,
            num_catm1,
            x_tmp_1,
            re_null,
            num_sim,
            num_replic,
            D,
            pars_ext,
            V
          )
          eta_0 <- compute_eta(
            est,
            num_cat,
            num_catm1,
            x_tmp_0,
            re_null,
            num_sim,
            num_replic,
            D,
            pars_ext,
            V
          )

          pi_1 <- pi_0 <- array(NA, c(num_sim, num_cat))
          for (s in seq_len(num_sim)) {
            pi_1[s, ] <- compute_ce(eta_1, which_empty, s)
            pi_0[s, ] <- compute_ce(eta_0, which_empty, s)
          }

          ## Average marginal effects
          me_qois_aux <-
            compute_me_qois(pi_0,
              pi_1,
              y_structure = y_structure,
              base = base,
              posterior_quantiles = posterior_quantiles,
              full_posterior = full_posterior,
              predictor_shift = predictor_shift,
              relative = relative
            )
          me_prob_pos[[u_char]] <- me_qois_aux$prob_pos
          me_qois[[u_char]] <- me_qois_aux$me_qois
        }
        me_prob_pos <- me_prob_pos %>% reorder_qoi()
        me_qois <- me_qois %>% reorder_qoi()
      } else {
        for (i in seq_along(predictor_levels)) {
          for (j in seq_along(predictor_levels)) {
            if (i != j) {
              pair_name <- paste(predictor_levels[i],
                predictor_levels[j],
                sep = " - "
              )
              me_qois[[pair_name]] <- me_prob_pos[[pair_name]] <- list()

              for (u in moderator_sequence) {
                me_qois_aux  <-
                  compute_me_qois(ce_est[, , i, u],
                    ce_est[, , j, u],
                    y_structure = y_structure,
                    base = base,
                    posterior_quantiles = posterior_quantiles,
                    full_posterior = full_posterior,
                    predictor_shift = 1,
                    relative = relative
                  )
                me_prob_pos[[pair_name]][[u]] <- me_qois_aux$prob_pos
                me_qois[[pair_name]][[u]] <- me_qois_aux$me_qois
              }
              me_prob_pos[[pair_name]] <- me_prob_pos[[pair_name]] %>%
                reorder_qoi()
              me_qois[[pair_name]] <- me_qois[[pair_name]] %>%
                reorder_qoi()
            }
          }
        }
      }
    } else if (!is.null(moderator) &
      !moderator_continuous) {
      if (predictor_continuous) {
        ## Compute average marginal effect
        pi_1 <-
          pi_0 <- array(NA, c(num_sim, num_cat, num_cat_moderator))
        me_qois <- me_prob_pos <- list()

        for (k in seq_along(moderator_levels)) {
          ## Adjust matrix
          x_tmp_1 <- x_tmp_0 <- x_default
          t_0 <- x_default[which_predictor, ]
          t_1 <- t_0 + predictor_shift
          x_tmp_0[which_moderator, ] <-
            as.numeric(seq_len(num_cat_moderator) %in% c(1, k)) %>%
            replicate(num_replic, .)
          x_tmp_0[which_predictors, ] <-
            as.matrix(as.numeric(1:length(which_predictors) %in% c(1, k))) %*%
            t_0
          x_tmp_1[which_moderator, ] <-
            as.numeric(seq_len(num_cat_moderator) %in% c(1, k)) %>%
            replicate(num_replic, .)
          x_tmp_1[which_predictors, ] <-
            as.matrix(as.numeric(1:length(which_predictors) %in% c(1, k))) %*%
            t_1

          ## Counterfactual outcome-specific estimates
          eta_1 <- compute_eta(
            est,
            num_cat,
            num_catm1,
            x_tmp_1,
            re_null,
            num_sim,
            num_replic,
            D,
            pars_ext,
            V
          )
          eta_0 <- compute_eta(
            est,
            num_cat,
            num_catm1,
            x_tmp_0,
            re_null,
            num_sim,
            num_replic,
            D,
            pars_ext,
            V
          )

          for (s in seq_len(num_sim)) {
            pi_1[s, , k] <- compute_ce(eta_1, which_empty, s)
            pi_0[s, , k] <- compute_ce(eta_0, which_empty, s)
          }

          ## Average marginal effects
          me_qois_aux <- compute_me_qois(
            pi_0[, , k],
            pi_1[, , k],
            y_structure = y_structure,
            base = base,
            posterior_quantiles = posterior_quantiles,
            full_posterior = full_posterior,
            predictor_shift = predictor_shift,
            relative = relative
          )
          me_prob_pos[[moderator_levels[k]]] <- me_qois_aux$prob_pos
          me_qois[[moderator_levels[k]]] <- me_qois_aux$me_qois
        }

        ## Reorder qoi
        me_prob_pos <- me_prob_pos %>% reorder_qoi()
        me_qois <- me_qois %>% reorder_qoi()
      } else {
        me_qois_tmp <- me_prob_pos_tmp <- list()
        pair_names <- c()
        for (k in seq_along(moderator_levels)) {
          me_prob_pos_tmp[[moderator_levels[k]]] <- list()
          me_qois_tmp[[moderator_levels[k]]] <- list()
          for (i in seq_along(predictor_levels)) {
            for (j in seq_along(predictor_levels)) {
              if (i != j) {
                pair_name <- paste(predictor_levels[i],
                  predictor_levels[j],
                  sep = " - "
                )
                pair_names <- c(pair_names, pair_name)
                me_qois_aux <-
                  compute_me_qois(
                    ce_est[, , i, k],
                    ce_est[, , j, k],
                    y_structure = y_structure,
                    base = base,
                    posterior_quantiles = posterior_quantiles,
                    full_posterior = full_posterior,
                    predictor_shift = 1,
                    relative = relative
                  )
                me_prob_pos_tmp[[moderator_levels[k]]][[pair_name]] <-
                  me_qois_aux$prob_pos
                me_qois_tmp[[moderator_levels[k]]][[pair_name]] <-
                  me_qois_aux$me_qois
              }
            }
          }
        }

        ## Reorder qoi
        me_qois <- list()
        for (n in unique(pair_names)) {
          me_prob_pos[[n]] <- lapply(me_prob_pos_tmp, function(x) {
            x[[n]]
          }) %>%
            reorder_qoi()
          me_qois[[n]] <- lapply(me_qois_tmp, function(x) {
            x[[n]]
          }) %>%
            reorder_qoi()
        }
      }
    }
  }

  ## ---- Return output ----
  output <- list()
  if (conditional_expectation) {
    output$conditional_expectation <- ce_qois
  } else {
    output$conditional_expectation <- NULL
  }

  if (average_marginal_effect) {
    output$average_marginal_effect <- me_qois
    output$ame_probability_positive <- me_prob_pos
  } else {
    output$average_marginal_effect <- NULL
  }

  if (!is.null(moderator)) {
    if (moderator_continuous) {
      output$moderator <- moderator_sequence
    } else {
      output$moderator <- moderator_levels
    }
  }

  if (predictor_continuous) {
    output$predictor_sequence <- predictor_sequence
  }

  return(output)
}
