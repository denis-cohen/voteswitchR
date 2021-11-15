#' @title run_mavcl
#'
#' @description Runs the Mixed Aggregate Varying Choice Set Logit (MAVCL)
#' model via \code{rstan}.
#'
#' @return Returns an object of class \code{mavcl_estimation}: A list that
#' includes estimates and auxiliary information.
#'
#' @export

run_mavcl <- function(data,
                      Y_names,
                      null_model = FALSE,
                      main_predictor,
                      predictor_continuous = TRUE,
                      moderator = NULL,
                      moderator_continuous = TRUE,
                      other_covariates = NULL,
                      random_slopes = FALSE,
                      re_parties = FALSE,
                      re_elections = FALSE,
                      re_countries = FALSE,
                      n_iter = 2000L,
                      n_warm = 1000L,
                      n_thin = 2L,
                      n_chains = 2L,
                      n_cores = parallel::detectCores(),
                      max_treedepth = 15,
                      adapt_delta = 0.8,
                      seed,
                      parallelize = FALSE,
                      savename = "mavcl",
                      savepath = NULL,
                      outfile = TRUE) {
  `%>%` <- magrittr::`%>%`

  ## ---- Imputed data ----
  if (any(class(data) %in% c("tbl_df", "tbl", "data.frame"))) {
    is_imputed <- FALSE
    data <- list(data)
  } else if (class(data) == "list" &
             sapply(data, class) %>%
             apply(2, function(d)
               d %in% c("tbl_df", "tbl", "data.frame")) %>%
             all()) {
    is_imputed <- TRUE
  }

  ## ---- Data ----
  cat("Preparing data")
  cat("\n")

  ## Initialize data list
  dat <- list()

  for (m in seq_along(data)) {
    ## Initialize data list
    dat[[m]] <- list()

    if (null_model) {
      rhs <- "~ 1"
      dat[[m]]$D <- 1L
    } else {
      ## Main predictor
      if (!predictor_continuous) {
        data[[m]] <- data[[m]] %>%
          dplyr::mutate(!!main_predictor :=
                          droplevels(as.factor(!!as.symbol(
                            main_predictor
                          ))))
        cats_predictor <- levels(data[[m]][[main_predictor]])
      }
      rhs <- paste0("~ ", main_predictor)

      ## Moderator
      if (!is.null(moderator)) {
        if (!moderator_continuous) {
          data[[m]] <- data[[m]] %>%
            dplyr::mutate(!!moderator :=
                            droplevels(as.factor(!!as.symbol(
                              moderator
                            ))))
          cats_moderator <- levels(data[[m]][[moderator]])
        }
        rhs <- paste0(rhs,
                      " + ",
                      moderator,
                      " + ",
                      paste0(main_predictor, ":", moderator))
      }

      ## Other covariates
      if (!is.null(other_covariates)) {
        rhs <- paste0(rhs, " + ", paste(other_covariates, collapse = " + "))
      }

      ## Number of varying coefs
      if (random_slopes) {
        dat[[m]]$D <- ifelse(predictor_continuous, 2L, cats_predictor)
      } else {
        dat[[m]]$D <- 1L
      }
    }

    ## Data
    dat[[m]]$X <- model.matrix(as.formula(rhs), data = data[[m]])
    dat[[m]]$Y <- as.matrix(data[[m]][Y_names])
    dat[[m]]$ncat <- ncol(dat[[m]]$Y)
    dat[[m]]$J <- nrow(dat[[m]]$X)
    dat[[m]]$K = ncol(dat[[m]]$X)

    ## Random intercepts
    if (re_parties) {
      dat[[m]]$num_parties <- length(unique(data[[m]]$party_harmonized))
      dat[[m]]$party_id <- data[[m]]$party_harmonized
      dat[[m]]$party_id <-
        sapply(dat[[m]]$party_id, function(x)
          which(unique(dat[[m]]$party_id) == x))
    } else {
      dat[[m]]$num_parties <- NULL
      dat[[m]]$party_id <- NULL
    }

    if (re_elections) {
      dat[[m]]$num_elections <- length(unique(data[[m]]$elec_id))
      dat[[m]]$election_id <- data[[m]]$elec_id
      dat[[m]]$election_id <-
        sapply(dat[[m]]$election_id, function(x)
          which(unique(dat[[m]]$election_id) == x))
    } else {
      dat[[m]]$num_elections <- NULL
      dat[[m]]$election_id <- NULL
    }

    if (re_countries) {
      dat[[m]]$num_countries <- length(unique(data[[m]]$iso2c))
      dat[[m]]$country_id <- data[[m]]$iso2c
      dat[[m]]$country_id <-
        sapply(dat[[m]]$country_id, function(x)
          which(unique(dat[[m]]$country_id) == x))
    } else {
      dat[[m]]$num_countries <- NULL
      dat[[m]]$country_id <- NULL
    }

    ## All optinal random effects
    dat[[m]]$V <- rbind(dat[[m]]$party_id,
                        dat[[m]]$election_id,
                        dat[[m]]$country_id)
  }

  ## ---- Define Type ----
  ## Type
  type <- dplyr::case_when(
    !re_parties & !re_elections & !re_countries ~ 1L,
    re_parties & !re_elections & !re_countries ~ 2L,
    !re_parties &
      re_elections & !re_countries ~ 3L,
    !re_parties &
      !re_elections & re_countries ~ 4L,
    re_parties & re_elections & !re_countries ~ 5L,
    !re_parties &
      re_elections & re_countries ~ 6L,
    re_parties & !re_elections & re_countries ~ 7L,
    re_parties & re_elections & re_countries ~ 8L
  )

  ## Type-specific model
  model_name <- paste0("vclogit_l2_type", type, ".stan")

  ## Parameters to sample
  pars <- c("beta", "Sigma", "nu")
  re_pars <- c("Sigma", "nu")
  if (re_parties)
    pars <- c(pars, paste(re_pars, "parties", sep = "_"))
  if (re_elections)
    pars <- c(pars, paste(re_pars, "elections", sep = "_"))
  if (re_countries)
    pars <- c(pars, paste(re_pars, "countries", sep = "_"))

  ## ---- Estimation ----
  cat("Performing Full Bayesian Inference")
  cat("\n")

  if (parallelize) {
    ## Set up for parallel estimation
    if (Sys.info()["sysname"] == "Windows") {
      cl <- parallel::makeCluster(length(dat),
                        outfile = paste0("est/", savename, ".txt"),
                        type = "PSOCK")
      parallel::clusterEvalQ(cl, library(rstan))
      parallel::clusterExport(
        cl,
        c(
          "mod",
          "dat",
          "n_iter",
          "n_warm",
          "n_thin",
          "n_chains",
          "n_cores",
          "seed",
          "max_treedepth",
          "adapt_delta",
          "pars"
        ),
        envir = environment()
      )
    } else {
      cl <- parallel::makeCluster(length(dat),
                        outfile = paste0("est/", savename, ".txt"),
                        type = "FORK")
    }

    ## Sample
    est <- parallel::parLapply(cl, seq_along(dat),
                     function (m) {
                       rstan::sampling(
                         stanmodels[[model_type]],
                         data = dat[[m]],
                         pars = pars,
                         algorithm = "NUTS",
                         control = list(max_treedepth = max_treedepth,
                                        adapt_delta = adapt_delta),
                         save_warmup = FALSE,
                         sample_file = NULL,
                         init_r = .25,
                         iter = n_iter,
                         warmup = n_warm,
                         thin = n_thin,
                         chains = n_chains,
                         cores = min(n_cores, n_chains),
                         seed = seed
                       )
                     })

    ## Exit parallel computation
    parallel::stopCluster(cl)
  } else {
    est <- rstan::sampling(
      stanmodels[[model_type]],
      data = dat[[1]],
      pars = pars,
      algorithm = "NUTS",
      control = list(max_treedepth = max_treedepth,
                     adapt_delta = adapt_delta),
      save_warmup = FALSE,
      sample_file = NULL,
      init_r = .25,
      iter = n_iter,
      warmup = n_warm,
      thin = n_thin,
      chains = n_chains,
      cores = min(n_cores, n_chains),
      seed = seed
    )
  }

  ## Return output
  cat("Returning output")
  cat("\n")
  if (null_model) {
    output <- list(data = dat,
                   estimates = est,
                   is_imputed = is_imputed,
                   type = type,
                   pars = pars)
  } else {
    output <- list(
      main_predictor = main_predictor,
      predictor_continuous = predictor_continuous,
      moderator = moderator,
      moderator_continuous = ifelse(is.null(moderator),
                                    NA,
                                    moderator_continuous),
      data = dat,
      estimates = est,
      is_imputed = is_imputed,
      type = type,
      pars = pars
    )
    if (!predictor_continuous) {
      output$predictor_levels <- cats_predictor
    }
    if (!is.null(moderator) & !moderator_continuous) {
      output$moderator_levels <- cats_moderator
    }
  }

  return(output)

  ## Optionally: Save to file
  if (!is.null(savepath)) {
    save_to <- paste0(savepath, "/", svnm, ".RData")
    save(output, file = paste0(savepath, "/", svnm, ".RData"))
    cat(paste0("Output saved at ", save_to, "."))
  }
}
