## compute_ce
compute_ce <- function(eta, which_empty, s) {
  eta %>%
    sapply(function (x)
      x[s,]) %>%
    softmax_vcl(which_empty = which_empty) %>%
    apply(2, mean, na.rm = TRUE)
}