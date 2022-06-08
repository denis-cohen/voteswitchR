#' @title compute_ce
#'
#' @description Internal function to compute conditional expectations
#'
#' @noRd

compute_ce <- function(eta, which_empty, s) {
  eta %>%
    vapply(function(x) {
      x[s, ]
    }) %>%
    softmax_vcl(which_empty = which_empty) %>%
    apply(2, mean, na.rm = TRUE)
}
