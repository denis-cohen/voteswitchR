#' @title calculate_pred_error
#'
#' @description Uses an object of class \code{mavcl_estimation} and calculates
#' prediction errors of posterior means or medians.
#'
#' @param mavcl_object An object of class \code{mavcl_estimation}.
#' @param posterior \code{"mean"} or \code{"median"}, depending on whether posterior
#' means or medians should be evaluate against the observed proportions of the
#' outcome variable.
#' @param type \code{"mae"} or \code{"rmse"}, depending on whether (mean) average
#' errors or the root mean squared standard error should be reported
#' @param re_null logical, specifies whether the prediction should be calculated
#' with (\code{FALSE}) or without (\code{TRUE}) random effects
#'
#' @return Returns a list with observation-/outcome-specific absolute errors and
#' the overall mean absolute error (if \code{type == "mae"}) or the overall
#' root mean squared error (if \code{type == "rmse"})
#'
#' @export

calculate_pred_error <- function(mavcl_object,
                                 posterior = c("mean", "median"),
                                 type = c("mae", "rmse"),
                                 re_null = FALSE) {

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
  y_names <- colnames(Y)

  ## Estimates
  start <- seq(1, by = D, length = num_cat - 1L)
  if (class(mavcl_object$estimates) == "stanfit") {
    est <- rstan::extract(mavcl_object$estimates, pars = pars_all)
    if (D > 1) {
      est$nu <- lapply(start, function(i, array, D)
        array[, , i:(i + D - 1)],
        array = est$nu, D = D)
    }
  } else if (class(mavcl_object$estimates) == "list" &
             class(mavcl_object$estimates[[1]]) == "stanfit") {
    est <- list()
    for (p in pars_all) {
      est[[p]] <- do.call(abind, c(
        sapply(mavcl_object$estimates,
               rstan::extract, pars = p),
        along = 1L
      ))
    }
    if (D > 1) {
      est$nu <- lapply(start, function(i, array, D)
        array[, , i:(i + D - 1)],
        array = est$nu, D = D)
    }
  }
  num_sim <- dim(est$beta)[1]

  ## ---- Predictor matrices ----
  x_default <- t(X)
  num_replic <- num_obs

  ## ---- Posterior means/medians ----
  eta <- compute_eta(est,
                     num_cat,
                     num_catm1,
                     x_default,
                     re_null,
                     num_sim,
                     num_replic,
                     D,
                     pars_ext,
                     V)
  pi <- array(NA, c(num_sim, num_replic, num_cat))
  for (s in seq_len(num_sim)) {
    pi[s, , ] <- sapply(eta, function (c)
      c[s, ]) %>%
      softmax_vcl(which_empty)
  }

  if (posterior == "median") {
    pi <- apply(pi, 2:3, median, na.rm = TRUE)
  }

  if (posterior == "mean") {
    pi <- apply(pi, 2:3, mean, na.rm = TRUE)
  }

  ## ---- Proportional Y ----
  prop <- Y %>%
    apply(1, function(x)
      if_else(x >= 0, x / sum(x[x >= 0]), NA_real_)) %>%
    t()

  if (type == "mae") {
    ae <- abs(pi - prop)
    colnames(ae) <- y_names
    mae <- mean(ae, na.rm = TRUE)

    output <- list(
      ae = ae,
      mae = mae
    )
    return(output)
  }

  if (type == "rmse") {
    se <- pi[!(is.na(pi))] - prop[!(is.na(prop))] ^ 2
    rmse <- sqrt(mean(se, na.rm = TRUE))
    return(rmse)
  }
}
