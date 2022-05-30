#' @title Aggregate processed data to party-election-specific switcher counts.
#'
#' @description Internal function used to aggregate switches, optionally also
#' partitioned across for subgroups
#'
#' @return Returns a tibble contained the requested aggregated switches
#'
#' @noRd

aggregate_switches <- function(
  data_file,
  context_vars = "elec_id",
  weights_var = "weights",
  switch_from = "l_vote",
  switch_to = "vote",
  subgroup = NULL
) {

  # Input vectors
  all_vars <- c(context_vars, weights_var, switch_from, switch_to, subgroup)
  switch_vars <- c(switch_from, switch_to, subgroup)

  switches <- data_file %>%
    dplyr::select(dplyr::all_of(all_vars)) %>%
    dplyr::group_by_at(context_vars) %>%
    dplyr::mutate(weights =
                    !!as.name(weights_var) / mean(!!as.name(weights_var))) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::mutate(
      cats_switch_to = list(sort(unique(!!as.name(switch_to)))),
      cats_switch_from = list(sort(unique(!!as.name(switch_from)))),
      cats_comb = list(sort(unique(
        c(!!as.name(switch_to), !!as.name(switch_from))
      )))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(c(context_vars, switch_vars), .drop = FALSE) %>%
    dplyr::summarize(
      weights = sum(weights),
      cats_comb = unique(cats_comb),
      cats_switch_to = unique(cats_switch_to),
      cats_switch_from = unique(cats_switch_from),
      n = unique(n)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      switch_to = factor(!!as.name(switch_to), levels = unlist(cats_comb)),
      switch_from = factor(!!as.name(switch_from), levels = unlist(cats_comb))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(context_vars),
                  dplyr::any_of(subgroup),
                  switch_from,
                  switch_to,
                  weights,
                  n,
                  dplyr::starts_with("cats_"))

  if (is.null(subgroup)) {
    switches_expanded <- switches %>%
      dplyr::group_by_at(context_vars) %>%
      tidyr::expand(switch_from, switch_to)%>%
      dplyr::left_join(
        switches %>%
          dplyr::select(-dplyr::starts_with("cats"), -dplyr::starts_with("n")),
        by = c(context_vars, "switch_from", "switch_to")
      )
  } else {
    switches_expanded <- switches %>%
      dplyr::group_by_at(context_vars) %>%
      tidyr::expand(switch_from, switch_to, !!as.name(subgroup))%>%
      dplyr::left_join(
        switches %>%
          dplyr::select(-dplyr::starts_with("cats"), -dplyr::starts_with("n")),
        by = c(context_vars, "switch_from", "switch_to", subgroup)
      )
  }

  switches <- switches_expanded %>%
    dplyr::left_join(
      switches %>%
        dplyr::select(dplyr::all_of(context_vars),
                      dplyr::starts_with("cats"),
                      n) %>%
        dplyr::distinct(),
      by = context_vars
    ) %>%
    dplyr::rowwise() %>%
    dplyr::filter(switch_to %in% unlist(cats_comb)) %>%
    dplyr::filter(switch_from %in% unlist(cats_comb)) %>%
    dplyr::mutate(
      weights = dplyr::case_when(
        is.na(weights) &
          switch_to %in% cats_switch_to &
          switch_from %in% cats_switch_from ~ 0.0,
        TRUE ~ weights
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::starts_with("cats")) %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(switch_from, switch_to),
      .funs = ~as.numeric(as.character(.))
    )

  ## Return
  return(switches)
}
