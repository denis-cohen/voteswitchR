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
  ## No summarize info
  options(dplyr.summarise.inform = FALSE)
  
  ## switch_factor as character
  mappings[[switch_factor]] <-
    as.factor(mappings[[switch_factor]]) %>%
    droplevels() %>%
    as.character()
  
  ## Check for missingness in switch_factor
  if (any(is.na(mappings[[switch_factor]]))) {
    warning(
      paste(
        "'switch_factor' contains NAs.",
        "These will be subsumed under 'Others' (oth).",
        sep = " "
      )
    )
    mappings[[switch_factor]] <-
      ifelse(is.na(mappings[[switch_factor]]),
             "oth",
             mappings[[switch_factor]])
  }
  
  ## Determine party-level variables in mappings
  if (type == "elections") {
    peid_vars <- mappings %>%
      group_by(elec_id) %>%
      summarize_all(.funs = ~ length(na.omit(unique(.)))) %>%
      ungroup() %>%
      summarize_if(is.numeric, .funs = ~ max(., na.rm = T)) %>%
      as.matrix()
    peid_vars <- colnames(peid_vars)[peid_vars[1,] != 1]
  }
  
  ## ---- Recoding of switches ----
  ## Augment switching
  switches <- switches %>%
    filter(elec_id %in% mappings$elec_id) %>%
    dplyr::left_join(
      mappings %>%
        dplyr::rename(from = !!as.name(switch_factor)) %>%
        dplyr::select(elec_id, stack, from),
      by = c("elec_id", "switch_from" = "stack")
    ) %>%
    dplyr::left_join(
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
    ## Define y_structure and y_names
    y_structure <- switches %>%
      dplyr::select(from, to) %>%
      dplyr::distinct() %>%
      dplyr::mutate(cat = from) %>%
      tidyr::expand(cat, from, to) %>%
      dplyr::mutate(switch = paste(from, to, sep = "_")) %>%
      dplyr::mutate(
        type = dplyr::case_when(
          cat == from & cat == to ~ "retain",
          cat == from & cat != to ~ "loss",
          cat != from & cat == to ~ "gain",
          TRUE ~ "resid"
        )
      ) %>%
      dplyr::mutate(dyad = dplyr::case_when(
        type == "loss" ~ to,
        type == "gain" ~ from,
        TRUE ~ NA_character_
      )) %>%
      dplyr::select(-from, -to) %>%
      split(., .$cat) %>%
      lapply(., function (x) x %>% dplyr::mutate(pos = row_number()))
    y_names <- y_structure[[1]]$switch
    
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
    output <- list(
      data = mappings %>%
        dplyr::select(-all_of(peid_vars),-any_of(switch_factor)) %>%
        distinct() %>%
        dplyr::inner_join(elec_switches,
                          by = c("elec_id")),
      y_names = y_names,
      y_structure = y_structure
    )
    
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
      dplyr::summarize(
        from = unique(from),
        to = unique(to),
        weights = if_else(all(is.na(weights)),
                          NA_real_,
                          sum(weights, na.rm = TRUE))
      )  %>%
      dplyr::ungroup() %>%
      dplyr::mutate(weights = ifelse(is.na(weights),-1.0, weights))

    ## Define y_structure and y_names
    y_structure <- peid_switches %>%
      dplyr::select(from, to, switch) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        type = dplyr::case_when(
          from == "party" & to == "party" ~ "retain",
          from == "party" & to != "party" ~ "loss",
          from != "party" & to == "party" ~ "gain",
          TRUE ~ "resid"
        )
      ) %>%
      dplyr::mutate(dyad = dplyr::case_when(
        type == "loss" ~ to,
        type == "gain" ~ from,
        TRUE ~ NA_character_
      )) %>%
      dplyr::select(-to, -from) %>%
      dplyr::distinct() %>%
      dplyr::arrange(not(is.na(dyad)), type, dyad) %>% 
      dplyr::mutate(pos = row_number())
    y_names <- y_structure$switch
    
    peid_switches <- peid_switches %>%
      dplyr::select(-to, -from) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = "switch",
                         values_from =  weights) %>%
      dplyr::mutate_if(is.numeric,
                       .funs = ~ ifelse(is.na(.),-1.0, .))
    
    ## Output
    output <- list(
      data = mappings %>%
        dplyr::inner_join(peid_switches,
                          by = c("elec_id", "stack")),
      y_names = y_names,
      y_structure = y_structure
    )
  }
  
  ## ---- Return ----
  return(output)
}
