## reorder dyadic quantities of interest in conditional models
reorder_qoi <- function(qoi) {
  ## Auxiliary objects
  num_conditions <- length(qoi)
  dyadic_quantities <-
    paste0("dyadic_", c("losses", "gains", "balances", "volumes"))
  non_dyadic_quantities <-
    c("losses", "gains", "balance", "volume", "retention")
  dyad_names <- names(qoi[[1]][["dyadic_losses"]])
  
  ## New qoi
  reordered_qoi <- list()
  
  ## Non-dyadic quantities
  for (q in non_dyadic_quantities) {
    reordered_qoi[[q]] <- lapply(qoi, function(x)
      x[[q]]) %>%
      simplify2array()
  }
  
  ## Dyadic quantities
  for (q in dyadic_quantities) {
    reordered_qoi[[q]] <- list()
    for (n in dyad_names) {
      reordered_qoi[[q]][[n]] <-
        lapply(qoi, function(x)
          x[[q]][[n]]) %>%
        simplify2array()
    }
  }
  
  ## Return value
  return(reordered_qoi)
}