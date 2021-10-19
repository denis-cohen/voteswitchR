## compute_eta
compute_eta <- function (est,
                         num_cat,
                         num_catm1,
                         x_obj,
                         re_null,
                         num_sim,
                         num_replic,
                         D,
                         pars_ext,
                         V) {
  x_vary <- t(x_obj[1:D, ])
  eta_est <- lapply(seq_len(num_catm1),
                    function (c) {
                      eta_c <- est$beta[, c , ] %*% x_obj
                      if (not(re_null)) {
                        if (D == 1) {
                          eta_c <- eta_c + est$nu[, , c]
                        } else if (D > 1) {
                          eta_c <- eta_c + t(sapply(seq_len(num_sim), function (s)
                            (est$nu[[c]][s, ,] * x_vary) %*% rep(1, D)))
                        }
                        if (length(pars_ext) > 0) {
                          for (p in seq_along(pars_ext))
                            eta_c <-
                              eta_c + est[[pars_ext[p]]][, V[p,], c]
                        }
                      }
                      return(eta_c)
                    })
  eta_est[[num_cat]] <-
    array(0.0, dim = c(num_sim, num_replic))
  return(eta_est)
}