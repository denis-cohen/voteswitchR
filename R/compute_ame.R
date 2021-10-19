#' @title compute_ame
#'
#' @description Internal function to compute average marginal effects
#'
#' @noRd

compute_ame <-
  function(eta0,
           eta1,
           predictor_shift,
           which_empty,
           s) {
    (compute_ce(eta1, which_empty, s) -
       compute_ce(eta0, which_empty, s)) / predictor_shift
  }
