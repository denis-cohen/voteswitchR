#' @title Recode aggregate switching patterns
#'
#' @description Recodes raw aggregate switching patterns according to a
#' user-supplied factor variable included in a (possibly user-supplied version
#' of) \code{mappings}.
#'
#' @return Returns the supplied data frame \code{mappings}, extended by columns
#' for each switching pattern detected in the data.
#'
#' @export

recode_switches <- function(switches,
                            mappings = NULL,
                            switch_factor,
                            type = c("elections", "party-elections")) {
  ## ---- Preparation ----
  ## switch_factor as character
  mappings[[switch_factor]] <-
    as.factor(mappings[[switch_factor]]) %>%
    droplevels() %>%
    as.character()

  ## Determine party-level variables in mappings
  peid_vars <- mappings %>%
    group_by(elec_id) %>%
    summarize_all(.funs = ~ length(na.omit(unique(.)))) %>%
    ungroup() %>%
    summarize_if(is.numeric, .funs = ~ max(., na.rm = T)) %>%
    as.matrix()
  peid_vars <- colnames(peid_vars)[peid_vars[1,] != 1]

  ## ---- Recoding of switches ----
  ## Augment switching
  switches <- switches %>%
    dplyr::inner_join(
      mappings %>%
        dplyr::rename(from = !!as.name(switch_factor)) %>%
        dplyr::select(elec_id, stack, from),
      by = c("elec_id", "switch_from" = "stack")
    ) %>%
    dplyr::inner_join(
      mappings %>%
        dplyr::rename(to = !!as.name(switch_factor)) %>%
        dplyr::select(elec_id, stack, to),
      by = c("elec_id", "switch_to" = "stack")
    ) %>%
    dplyr::mutate(
      from = case_when(
        switch_from == 98 ~ "oth",
        switch_from == 99 ~ "non",
        is.na(from) ~ "oth",
        TRUE ~ from
      ),
      to = case_when(
        switch_to == 98 ~ "oth",
        switch_to == 99 ~ "non",
        is.na(to) ~ "oth",
        TRUE ~ to
      )
    )

  ## ---- Aggregation at chosen level ----
  if (type == "elections") {
    ## Election-level switches
    elec_switches <- switches %>%
      dplyr::group_by(elec_id) %>%
      dplyr::mutate(switch = paste(from, to, sep = "_")) %>%
      dplyr::group_by(elec_id, switch) %>%
      dplyr::summarize(weights = if_else(all(is.na(weights)),
                                         NA_real_,
                                         sum(weights, na.rm = TRUE))) %>%
      dplyr::mutate(weights = ifelse(is.na(weights),-1.0, weights)) %>%
      tidyr::pivot_wider(names_from = "switch",
                         values_from =  weights) %>%
      dplyr::mutate_if(is.numeric,
                       .funs = ~ ifelse(is.na(.),-1.0, .))

    ## Output
    output <- mappings %>%
      dplyr::select(-all_of(peid_vars),-any_of(switch_factor)) %>%
      distinct() %>%
      left_join(elec_switches,
                by = c("elec_id"))

  } else if (type == "party-elections") {
    ## Party-election level switches
    peid_switches <- switches %>%
      dplyr::left_join(mappings %>%
                         dplyr::select(elec_id, stack),
                       by = "elec_id") %>%
      dplyr::group_by(elec_id, stack) %>%
      dplyr::mutate(
        to = ifelse(switch_to == stack, "party", to),
        from = ifelse(switch_from == stack, "party", from),
        switch = case_when(
          switch_from == stack & switch_to == stack ~ "retention",
          switch_from != stack & switch_to != stack ~ "residual",
          TRUE ~ paste(from, to, sep = "_")
        )
      ) %>%
      dplyr::group_by(elec_id, stack, switch) %>%
      dplyr::summarize(weights = if_else(all(is.na(weights)),
                                         NA_real_,
                                         sum(weights, na.rm = TRUE))) %>%
      dplyr::mutate(weights = ifelse(is.na(weights), -1.0, weights)) %>%
      tidyr::pivot_wider(names_from = "switch",
                         values_from =  weights) %>%
      dplyr::mutate_if(is.numeric,
                       .funs = ~ ifelse(is.na(.),-1.0, .))

    ## Output
    output <- mappings %>%
      left_join(peid_switches,
                by = c("elec_id", "stack"))
  }

  ## ---- Return ----
  return(output)
}
