#' @title softmax_vcl
#'
#' @description Internal function for the inverse link
#' function of the MAVCL model; a modified softmax function.
#'
#' @noRd

softmax_vcl <- function(eta, which_empty) {
  eta[which_empty] <- NA
  if (is.vector(eta)) {
    exp(eta) / sum(exp(eta), na.rm = TRUE)
  } else if (is.matrix(eta)) {
    t(apply(eta, 1, function(x) {
      exp(x) / sum(exp(x), na.rm = TRUE)
    }))
  }
}
