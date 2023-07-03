#' @title Calculate measurement error in vote switching survey data
#'
#' @description Uses an object of class \code{mavcl_est} and calculates
#' prediction errors of posterior means or medians.
#'
#' @param switches A switching object produced by \code{build_data_file}.
#' @param raked_switches Optionally, a raked switching object produced by \code{build_data_file}.
#' @param mappings Optional: A copy of [mappings], including a
#' column [switch_factor].
#' @param switch_factor Optional: Column name of a factor variable in \code{mappings}
#' that specifies the marginal categories of the desired voter transition
#' matrix. If not supplied, the errors will be calculated on the original
#' transition matrix will nominal parties included in \code{mappings},
#' augmented with residual others and non-voters.
#' @param assign_others Optional: A character that specifies a level for
#' residual other parties for which \code{switch_factor} information is not
#' available.
#' @param  type Specifies whether election-level mean absolute errors
#' or root mean squared errors will be returned. Defaults to \code{"mae"}.
#'
#' @return Returns a list with observation-/outcome-specific absolute errors and
#' the overall mean absolute error (if \code{type == "mae"}) or the overall
#' root mean squared error (if \code{type == "rmse"})
#'
#' @return Returns a list of party-election level directional errors and
#' election-level mean absolute errors or root mean squared errors.
#'
#' @export

calculate_meas_error <- function(switches,
                                 raked_switches = NULL,
                                 mappings = NULL,
                                 switch_factor = NULL,
                                 assign_others = NULL,
                                 type = c("mae", "rmse")) {
  ## ---- Preparation ----
  ## Helper function
  get_error <- function(x, y, type) {
    if (type == "mae") {
      return(mean(abs(x - y), na.rm = TRUE))
    } else if (type == "rmse") {
      return(sqrt(mean((x - y) ^ 2, na.rm = TRUE)))
    }
  }

  ## Default arguments
  if (is.null(type)) {
    type <- "mae"
  }

  ## No summarize info
  options(dplyr.summarise.inform = FALSE)

  ## Update switches and get proportions
  switches <- switches %>%
    dplyr::group_by(elec_id) %>%
    dplyr::mutate(n = sum(weights, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prop = weights / n)

  proportions <- switches %>%
    dplyr::rename(stack = switch_to) %>%
    dplyr::group_by(elec_id, stack) %>%
    dplyr::summarize(prop_t = sum(prop, na.rm = T)) %>%
    dplyr::left_join(
      switches %>%
        dplyr::rename(stack = switch_from) %>%
        dplyr::group_by(elec_id, stack) %>%
        dplyr::summarize(prop_tm1 = sum(prop, na.rm = T)),
      by = c("elec_id", "stack")
    )

  ## ---- Conditionally, aggregate at level of switch_factor ----
  if (!is.null(switch_factor)) {
    if (is.null(mappings)) {
      stop("Please supply both 'mappings' and 'switch_factor'.")
    }

    ## Initialize and augment mappings_shares
    mappings_shares <- voteswitchR:::mappings_shares %>%
      dplyr::left_join(mappings %>%
                         dplyr::select(elec_id, stack, any_of(switch_factor)),
                       by = c("elec_id", "stack")) %>%
      dplyr::mutate(
        !!switch_factor := dplyr::case_when(
          stack == 98L ~ "oth",
          stack == 99L ~ "non",
          TRUE ~ !!as.name(switch_factor)
        )
      )

    ## switch_factor as character
    mappings_shares[[switch_factor]] <-
      as.factor(mappings_shares[[switch_factor]]) %>%
      droplevels() %>%
      as.character()

    ## assign residual parties to distinct category?
    val98 <- ifelse(is.null(assign_others), "oth", assign_others)

    ## Check for missingness in switch_factor
    if (any(is.na(mappings_shares[[switch_factor]]))) {
      warning(
        paste(
          "'switch_factor' contains NAs.",
          "These will be subsumed under 'Others' (oth).",
          sep = " "
        )
      )
      mappings_shares[[switch_factor]] <-
        ifelse(is.na(mappings_shares[[switch_factor]]),
               "oth",
               mappings_shares[[switch_factor]])
    }

    ## Augment and aggregate proportions
    proportions <- proportions %>%
      dplyr::left_join(mappings_shares,
                       by = c("elec_id", "stack")) %>%
      dplyr::group_by(elec_id,!!as.name(switch_factor)) %>%
      dplyr::summarize_at(
        .vars = dplyr::vars(
          dplyr::starts_with("prop_"),
          dplyr::starts_with("vote_share")
        ),
        .funs = ~ sum(., na.rm = TRUE)
      )

    ## Update switches
    switches <- switches %>%
      dplyr::left_join(
        mappings_shares %>%
          dplyr::rename(from = !!as.name(switch_factor)) %>%
          dplyr::select(elec_id, stack, from),
        by = c("elec_id", "switch_from" = "stack")
      ) %>%
      dplyr::left_join(
        mappings_shares %>%
          dplyr::rename(to = !!as.name(switch_factor)) %>%
          dplyr::select(elec_id, stack, to),
        by = c("elec_id", "switch_to" = "stack")
      ) %>%
      dplyr::mutate(
        from = dplyr::case_when(
          switch_from == 98 ~ val98,
          switch_from == 99 ~ "non",
          is.na(from) ~ val98,
          TRUE ~ from
        ),
        to = dplyr::case_when(
          switch_to == 98 ~ val98,
          switch_to == 99 ~ "non",
          is.na(to) ~ val98,
          TRUE ~ to
        )
      ) %>%
      dplyr::group_by(elec_id, from, to) %>%
      dplyr::summarize(prop = dplyr::if_else(all(is.na(prop)),
                                             NA_real_,
                                             sum(prop, na.rm = TRUE)))
  } else {
    ## Initialize mappings_shares
    mappings_shares <- voteswitchR:::mappings_shares

    ## Augment and aggregate proportions
    proportions <- proportions %>%
      dplyr::left_join(mappings_shares,
                       by = c("elec_id", "stack"))

    ## Conditionally, augment and aggregate raked_proportions
    if (!is.null(raked_switches)) {
      raked_proportions <- raked_proportions %>%
        dplyr::left_join(mappings_shares,
                         by = c("elec_id", "stack"))
    }

    ## Update switches
    switches <- switches %>%
      dplyr::rename(from = switch_from,
                    to = switch_to)
  }

  ## ---- Marginal errors ----
  ## Parties
  if (is.null(switch_factor)) {
    party_errors <- proportions %>%
      dplyr::group_by(elec_id) %>%
      dplyr::mutate(
        vote_share =
          ifelse(stack == 98L,
                 vote_share + (1 - sum(
                   vote_share, na.rm = TRUE
                 )),
                 vote_share),
        vote_share_lag =
          ifelse(stack == 98L,
                 vote_share_lag + (1 - sum(
                   vote_share_lag, na.rm = TRUE
                 )),
                 vote_share_lag)
      ) %>%
      dplyr::mutate(error_t = prop_t - vote_share,
                    error_tm1 = prop_tm1 - vote_share_lag)
  } else {
    party_errors <- proportions %>%
      dplyr::group_by(elec_id) %>%
      dplyr::mutate(
        vote_share =
          ifelse(
            !!as.name(switch_factor) == "oth",
            vote_share + (1 - sum(vote_share, na.rm = TRUE)),
            vote_share
          ),
        vote_share_lag =
          ifelse(
            !!as.name(switch_factor) == "oth",
            vote_share_lag + (1 - sum(vote_share_lag, na.rm = TRUE)),
            vote_share_lag
          )
      ) %>%
      dplyr::mutate(error_t = prop_t - vote_share,
                    error_tm1 = prop_tm1 - vote_share_lag)
  }

  ## Elections
  elec_errors <- party_errors %>%
    dplyr::group_by(elec_id) %>%
    dplyr::summarize(
      mean_error_t = get_error(prop_t, vote_share, type),
      mean_error_tm1 = get_error(prop_tm1, vote_share_lag, type)
    )

  ## ---- Deviance switches/raked_switches ----
  if (!is.null(raked_switches)) {
    deviance_raked_unraked <- switches %>%
      dplyr::left_join(
        raked_switches %>%
          dplyr::select(elec_id, from, to, prop) %>%
          dplyr::rename(raked_prop = prop),
        by = c("elec_id", "from", "to")
      ) %>%
      dplyr::group_by(elec_id) %>%
      dplyr::summarize(mean_deviance = get_error(prop, raked_prop, type))
  } else {
    deviance_raked_unraked = NULL
  }

  ## ---- Return value ----
  return(
    list(
      party_errors = party_errors,
      elec_errors = elec_errors
    )
  )
}
