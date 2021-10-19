# ## aggregate_switches
# aggregate_switches <- function(data_file,
#                                context_file,
#                                out_var,
#                                agg_level = c("party", "national")) {
#   ## ---- Extract data file info ----
#   if (is.null(data_file$info_aux)) {
#     stop("Please supply a valid data file.")
#   } else {
#     map <- data_file$info_aux$map
#     map_var <- data_file$info_aux$map_var
#     impute <- data_file$info_aux$impute
#     n_imp <- data_file$info_aux$n_imp
#     seed <- data_file$info_aux$seed
#     format <- data_file$info_aux$format
#   }
#
#   ## ---- Import context file ----
#   context_file <- rio::import(context_file)
#
#   ## ---- Check arguments ----
#   if (not(map) | format != "long") {
#     stop("Your data file must be mapped and in long format.")
#   }
#
#   if (not(out_var %in% map_var)) {
#     stop("Your out_var must be included in map_var.")
#   }
#
#   ## ---- Extract data info ----
#   if (impute) {
#     jj <- names(data_file$data_imp)
#     mm <- seq_len(length(data_file$data_imp[[1]]))
#   } else {
#     jj <- names(data_file$data)
#   }
#
#   ## ---- Variable groups ----
#   party_vars <- c()
#   micro_vars <-
#
#   ## ---- Full data ----
#   if (impute) {
#     for (j in jj) {
#       for (m in 1:length(data_file$data_imp[[j]])) {
#         ## Outcome category
#         out_cats <- tapply(data_file$data_imp[[j]][[m]]$stack,
#                            data_file$data_imp[[j]][[m]][[out_var]],
#                            unique)
#         num_alts <-
#           length(unique(data_file$data_imp[[j]][[m]]$stack))
#         out_names <-
#           c(as.character(data_file$data_imp[[j]][[m]][[out_var]][1:alts]),
#             "oth",
#             "non")
#
#         ## (current/past) vote indicators
#         vt_in <- c(1:alts, 98:99) %in%
#           l_vt_in <- c(1:alts, 98:99) %in%
#           unique(data_file$data_imp[[j]][[m]]$l_vote)
#         data_file$data_imp[[j]][[m]]$out_vote <-
#           factor(data_file$data_imp[[j]][[m]]$vote, labels = pfam[vt_in])
#         data_file$data_imp[[j]][[m]]$out_l_vote <-
#           factor(data_file$data_imp[[j]][[m]]$l_vote, labels = pfam[l_vt_in])
#       }
#     }
#
#     ## Append
#     data_file_long <- list()
#     for (m in mm) {
#       data_file_long[[m]] <- lapply(data_file$data_imp, function(x)
#         x[[m]])
#       data_file_long[[m]] <-
#         do.call(bind_rows, data_file_long[[m]])
#     }
#   } else {
#     for (j in jj) {
#       ## Outcome category
#       out_cats <- tapply(data_file$data[[j]]$stack,
#                          data_file$data[[j]][[out_var]],
#                          unique)
#       num_alts <-
#         length(unique(data_file$data[[j]]$stack))
#       out_names <-
#         c(as.character(data_file$data[[j]][[out_var]][1:alts]),
#           "oth",
#           "non")
#
#       ## (current/past) vote indicators
#       vt_in <- c(1:alts, 98:99) %in%
#         l_vt_in <- c(1:alts, 98:99) %in%
#         unique(data_file$data[[j]]$l_vote)
#       data_file$data[[j]]$out_vote <-
#         factor(data_file$data[[j]]$vote, labels = pfam[vt_in])
#       data_file$data[[j]]$out_l_vote <-
#         factor(data_file$data[[j]]$l_vote, labels = pfam[l_vt_in])
#     }
#
#     ## Append
#     data_file_long <- list()
#     data_file_long <- do.call(bind_rows, data_file_long)
#   }
#
#   ## ---- Aggregation ----
#   if (agg_level == "party") {
#     if (impute) {
#       for (m in mm) {
#         data_file_long[[m]] <- data_file_long[[m]] %>%
#           dplyr::group_by(peid) %>%
#           dplyr::mutate(weights = weights / mean(weights)) %>%
#           dplyr::mutate(n = n()) %>%
#           dplyr::mutate(n_voters_t = sum(weights[vote != 99])) %>%
#           dplyr::mutate(n_voters_tm1 = sum(weights[l_vote != 99])) %>%
#           dplyr::mutate(cats_vote = list(sort(unique(out_vote[vote != stack]))),
#                         cats_l_vote = list(sort(unique(out_l_vote[l_vote != stack])))) %>%
#           dplyr::ungroup() %>%
#           dplyr::mutate(
#             out_vote = if_else(vote == stack, "p", as.character(out_vote)),
#             out_l_vote = if_else(l_vote == stack, "p",  as.character(out_l_vote)),
#           )
#       }
#     } else {
#       data_file_long <- data_file_long %>%
#         dplyr::group_by(peid) %>%
#         dplyr::mutate(weights = weights / mean(weights)) %>%
#         dplyr::mutate(n = n()) %>%
#         dplyr::mutate(n_voters_t = sum(weights[vote != 99])) %>%
#         dplyr::mutate(n_voters_tm1 = sum(weights[l_vote != 99])) %>%
#         dplyr::mutate(cats_vote = list(sort(unique(out_vote[vote != stack]))),
#                       cats_l_vote = list(sort(unique(out_l_vote[l_vote != stack])))) %>%
#         dplyr::ungroup() %>%
#         dplyr::mutate(
#           out_vote = if_else(vote == stack, "p", as.character(out_vote)),
#           out_l_vote = if_else(l_vote == stack, "p",  as.character(out_l_vote)),
#         )
#     }
#   } else if (agg_level == "national") {
#     if (impute) {
#       for (m in mm) {
#         data_file_long[[m]] <- data_file_long[[m]] %>%
#           dplyr::filter(stack == 1) %>%
#           dplyr::select(-any_of(party_vars))
#       }
#     } else {
#       data_file_long <- data_file_long %>%
#         dplyr::filter(stack == 1) %>%
#         dplyr::select(-any_of(party_vars))
#     }
#   }
# }
#
# ## ---- Data ----
# ## Vote Switching (stacked, imputed)
#
# ## Mapping info
# jj <- names(data_file$data_imp)
# mm <-  seq_len(length(data_file$data_imp[[1]]))
#
# map <- map %>%
#   mutate(parfam_harmonized = if_else(is.na(parfam_harmonized),
#                                      "",
#                                      parfam_harmonized)) %>%
#   mutate(
#     parlgov_vote_share = if_else(is.na(parlgov_vote_share),
#                                  vote_share,
#                                  parlgov_vote_share),
#     parlgov_vote_share_lag = if_else(
#       is.na(parlgov_vote_share_lag),
#       vote_share_lag,
#       parlgov_vote_share_lag
#     )
#   ) %>%
#   mutate(
#     parlgov_vote_share = if_else(cmp_pres == 1,
#                                  vote_share,
#                                  parlgov_vote_share),
#     parlgov_vote_share_lag = if_else(cmp_pres == 1,
#                                      vote_share_lag,
#                                      parlgov_vote_share_lag)
#   )
#
# for (j in jj) {
#   if (j %in% map$elec_id) {
#     for (m in mm) {
#       data_file$data_imp[[j]][[m]] <- data_file$data_imp[[j]][[m]] %>%
#         dplyr::select(-election_date) %>%
#         left_join(
#           map %>%
#             filter(elec_id == j) %>%
#             dplyr::select(
#               -iso2c,
#               -year,
#               -elec_id,
#               -starts_with("aux_"),
#               -party,
#               -map_vote,
#               -map_lr
#             ),
#           by = "party_harmonized"
#         )
#     }
#   } else {
#     data_file$data_imp[[j]] <- NULL
#   }
# }
#
# ## Long Format
# jj <- names(data_file$data_imp)
# for (j in jj) {
#   for (m in 1:length(data_file$data_imp[[j]])) {
#     ## Vote choices (party families)
#     vt.p <- tapply(data_file$data_imp[[j]][[m]]$stack,
#                    data_file$data_imp[[j]][[m]]$parfam_harmonized,
#                    unique)
#
#     ## Party-family identifiers
#     alts <- length(unique(data_file$data_imp[[j]][[m]]$stack))
#     pfam <-
#       c(as.character(data_file$data_imp[[j]][[m]]$parfam_harmonized[1:alts]),
#         "oth",
#         "non")
#     pfam <- case_when(
#       pfam == "nat" ~ "rrp",
#       pfam %in% c("",
#                   NA_character_,
#                   "agr",
#                   "div",
#                   "eth",
#                   "sip") ~ "oth",
#       TRUE ~ pfam
#     )
#
#     ## Party-family (current/past) vote indicators
#     vt.in <- c(1:alts, 98:99) %in%
#       unique(data_file$data_imp[[j]][[m]]$vote)
#     l.vt.in <- c(1:alts, 98:99) %in%
#       unique(data_file$data_imp[[j]][[m]]$l_vote)
#     data_file$data_imp[[j]][[m]]$pfam.vote <-
#       factor(data_file$data_imp[[j]][[m]]$vote, labels = pfam[vt.in])
#     data_file$data_imp[[j]][[m]]$pfam.l_vote <-
#       factor(data_file$data_imp[[j]][[m]]$l_vote, labels = pfam[l.vt.in])
#   }
# }
#
# ## ---- Append ----
# data_file_long <- list()
# for (m in mm) {
#   data_file_long[[m]] <- lapply(data_file$data_imp, function(x)
#     x[[m]])
#   data_file_long[[m]] <-  do.call(bind_rows, data_file_long[[m]])
# }
#
# rm(data_file)
# gc(reset = TRUE)
#
# for (m in mm) {
#   data_file_long[[m]] <- data_file_long[[m]] %>%
#     dplyr::mutate(
#       parfam_harmonized = case_when(
#         parfam_harmonized == "nat" ~ "rrp",
#         parfam_harmonized %in% c("",
#                                  NA_character_,
#                                  "agr",
#                                  "div",
#                                  "eth",
#                                  "sip") ~ "oth",
#         TRUE ~ parfam_harmonized
#       )
#     ) %>%
#     mutate(weights = ifelse(is.na(weights), 1.0, weights))
# }
#
# ## ---- Store backup ----
# backup <- data_file_long
# data_file_long <- backup
#
# ## ---- Switching Indicators ----
# aggregation_level <- "party"
#
# for (m in mm) {
#   ## Normalize weights within clusters, create party-election ID
#   data_file_long[[m]] <- data_file_long[[m]] %>%
#     dplyr::mutate(peid = paste0(party_name, " (", elec_id, ")")) %>%
#     dplyr::group_by(peid) %>%
#     dplyr::mutate(weights = weights / mean(weights)) %>%
#     dplyr::mutate(n = n()) %>%
#     dplyr::mutate(n_voters_t = sum(weights[vote != 99])) %>%
#     dplyr::mutate(n_voters_tm1 = sum(weights[l_vote != 99])) %>%
#     dplyr::mutate(cats.vote = list(sort(unique(pfam.vote[vote != stack]))),
#                   cats.l_vote = list(sort(unique(pfam.l_vote[l_vote != stack])))) %>%
#     dplyr::ungroup()
#
#   ## Process for party electorates
#   if (aggregation_level == "party") {
#     data_file_long[[m]] <- data_file_long[[m]] %>%
#       # dplyr::filter(vote == stack | l_vote == stack) %>%
#       dplyr::mutate(
#         pfam.vote = ifelse(vote == stack, "p", as.character(pfam.vote)),
#         pfam.l_vote = ifelse(l_vote == stack, "p",  as.character(pfam.l_vote)),
#       )
#
#     ## Selection of parties
#     data_file_long[[m]] <- data_file_long[[m]] %>%
#       dplyr::filter(parfam_harmonized == "soc") %>%
#       dplyr::filter(not(party_name %in% c("Independents' Alliance",
#                                           "DENK",
#                                           "Italian Renewal")))
#   }
# }
#
#
#
# ## ---- Aggregate (party electorates) ----
# micro_vars <- c(
#   "id",
#   "stack",
#   "weights",
#   "vote",
#   "l_vote",
#   "pid",
#   "lr_self",
#   "lr",
#   "male",
#   "age",
#   "stfdem",
#   "like",
#   "pfam.vote",
#   "pfam.l_vote"
# )
#
# data_file_agg <- list()
# for (m in mm) {
#   data_file_agg[[m]] <- data_file_long[[m]] %>%
#     dplyr::select(
#       -any_of(micro_vars)
#     ) %>%
#     distinct()
#
#   counts_long <- data_file_long[[m]] %>%
#     dplyr::select(peid,
#                   weights,
#                   pfam.vote,
#                   pfam.l_vote,
#                   cats.vote,
#                   cats.l_vote) %>%
#     dplyr::group_by(peid, pfam.l_vote, pfam.vote, .drop = FALSE) %>%
#     dplyr::summarize(
#       weights = sum(weights),
#       cats.vote = unique(cats.vote),
#       cats.l_vote = unique(cats.l_vote)
#     ) %>%
#     dplyr::ungroup()
#
#   counts_aux <- counts_long %>%
#     group_by(peid) %>%
#     dplyr::summarize(
#       contested_tm1 = sum(pfam.l_vote == "p") > 0,
#       cats.vote = unique(cats.vote),
#       cats.l_vote = unique(cats.l_vote)
#     ) %>%
#     dplyr::ungroup()
#
#   ## Note: for social democracy subset only
#   counts_long <- counts_long %>%
#     dplyr::select(-starts_with("cats")) %>%
#     mutate(pfam.vote = ifelse(pfam.vote == "soc", "oth", pfam.vote),
#            pfam.l_vote = ifelse(pfam.l_vote == "soc", "oth", pfam.l_vote)) %>%
#     group_by(peid, pfam.l_vote, pfam.vote) %>%
#     summarize_all(sum) %>%
#     dplyr::ungroup()
#
#   counts_full <- counts_long %>%
#     ungroup() %>%
#     tidyr::expand(peid, pfam.l_vote, pfam.vote) %>%
#     filter(pfam.vote == "p" | pfam.l_vote == "p") %>%
#     left_join(counts_long,
#               by = c("peid", "pfam.vote", "pfam.l_vote")) %>%
#     left_join(counts_aux,
#               by = c("peid")) %>%
#     rowwise() %>%
#     mutate(
#       weights = case_when(
#         is.na(weights) & not(contested_tm1) & pfam.l_vote == "p" ~ NA_real_,
#         is.na(weights) &
#           pfam.vote != "p" & pfam.vote %in% cats.vote ~ 0.0,
#         is.na(weights) &
#           pfam.l_vote != "p" & pfam.l_vote %in% cats.l_vote ~ 0.0,
#         is.na(weights) &
#           pfam.vote != "p" &
#           not(pfam.vote %in% cats.vote) ~ NA_real_,
#         is.na(weights) &
#           pfam.l_vote != "p" &
#           not(pfam.l_vote %in% cats.l_vote) ~ NA_real_,
#         TRUE ~ weights
#       )
#     ) %>%
#     ungroup() %>%
#     dplyr::select(-contested_tm1, -starts_with("cats")) %>%
#     pivot_wider(
#       names_from = c("pfam.l_vote", "pfam.vote"),
#       values_from =  weights,
#       names_sep = "_"
#     ) %>%
#     group_by(peid) %>%
#     mutate(ttl_switchers = rowSums(across(where(is.numeric)), na.rm = TRUE))
#
#   counts_names <- names(counts_full)
#   counts_names[substr(counts_names, 1, 2) == "p_"] <-
#     paste0("los_", substr(counts_names[substr(counts_names, 1, 2) == "p_"], 3, 5))
#   counts_names[substr(counts_names, 4, 5) == "_p"] <-
#     paste0("won_", substr(counts_names[substr(counts_names, 4, 5) == "_p"], 1, 3))
#   counts_names[counts_names == "won_los"] <- "kep"
#   names(counts_full) <- counts_names
#
#   data_file_agg[[m]] <- data_file_agg[[m]] %>%
#     left_join(counts_full,
#               by = "peid") %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(resid = n - ttl_switchers)
#
#   data_file_agg[[m]] <- data_file_agg[[m]] %>%
#     dplyr::mutate(
#       won = data_file_agg[[m]] %>%
#         dplyr::select(starts_with("won_")) %>%
#         base::rowSums(na.rm = TRUE),
#       los = data_file_agg[[m]] %>%
#         dplyr::select(starts_with("los_")) %>%
#         base::rowSums(na.rm = TRUE)
#     ) %>%
#     dplyr::mutate(
#       won = ifelse(
#         data_file_agg[[m]] %>%
#           dplyr::select(starts_with("won_")) %>%
#           mutate_all(is.na) %>%
#           base::rowSums() == 8L,
#         NA_real_,
#         won
#       ),
#       los = ifelse(
#         data_file_agg[[m]] %>%
#           dplyr::select(starts_with("los_")) %>%
#           mutate_all(is.na) %>%
#           base::rowSums() == 8L,
#         NA_real_,
#         los
#       )
#     ) %>%
#     dplyr::mutate(
#       reported_vote = sum(c(won, kep), na.rm = TRUE) / n_voters_t,
#       reported_l_vote = sum(c(los, kep), na.rm = TRUE) / n_voters_tm1,
#       reported_net = reported_vote - reported_l_vote
#     ) %>%
#     ungroup() %>%
#     dplyr::mutate_at(
#       .vars = vars(starts_with("won_"),
#                    starts_with("los_"),
#                    won,
#                    los,
#                    kep),
#       .funs = ~ case_when(is.nan(.) ~ NA_real_,
#                           is.na(.) ~ -1.0,
#                           TRUE ~ .)
#     ) %>%
#     dplyr::mutate_at(
#       .vars = vars(starts_with("reported")),
#       .funs = ~ case_when(is.nan(.) ~ NA_real_,
#                           is.na(.) ~ 0.0,
#                           TRUE ~ .)
#     ) %>%
#     dplyr::mutate(new = as.numeric(is.na(parlgov_vote_share_lag))) %>%
#     dplyr::mutate(parlgov_vote_share_lag = replace_na(parlgov_vote_share_lag, 0)) %>%
#     droplevels() %>%
#     dplyr::arrange(iso2c, party_harmonized, edate) %>%
#     dplyr::ungroup()
# }
#
# ## Filter
# for (m in mm) {
#   data_file_agg[[m]] <- data_file_agg[[m]] %>%
#     dplyr::filter(parfam_harmonized == "soc") %>%
#     dplyr::filter(not(parlgov_vote_share_lag == 0 &
#                         reported_l_vote > 0)) %>%
#     dplyr::filter(not(parlgov_vote_share_lag > 0 &
#                         reported_l_vote == 0))
# }
#
# ## Select
# for (m in mm) {
#   data_file_agg[[m]] <- data_file_agg[[m]] %>%
#     dplyr::select(-cats.vote,
#                   -cats.l_vote,
#                   -starts_with("reported_"),
#                   -starts_with("map_"))
# }
#
#
# ## ---- Save ----
# save(data_file_agg, file = "dat/nes-proc/data_file_agg_soc_dem_relative_disagg.rdata")
#
#
# ## ---- Switching Indicators ----
# data_file_long <- backup
# aggregation_level <- "national"
#
# for (m in mm) {
#   ## Normalize weights within clusters, create party-election ID
#   data_file_long[[m]] <- data_file_long[[m]] %>%
#     dplyr::mutate(peid = paste0(party_name, " (", elec_id, ")")) %>%
#     dplyr::group_by(peid) %>%
#     dplyr::mutate(weights = weights / mean(weights)) %>%
#     dplyr::mutate(n = n()) %>%
#     dplyr::mutate(n_voters_t = sum(weights[vote != 99])) %>%
#     dplyr::mutate(n_voters_tm1 = sum(weights[l_vote != 99])) %>%
#     dplyr::mutate(cats.vote = list(sort(unique(pfam.vote[vote != stack]))),
#                   cats.l_vote = list(sort(unique(pfam.l_vote[l_vote != stack])))) %>%
#     dplyr::ungroup()
#
#   ## Process for party electorates
#   if (aggregation_level == "party") {
#     data_file_long[[m]] <- data_file_long[[m]] %>%
#       # dplyr::filter(vote == stack | l_vote == stack) %>%
#       dplyr::mutate(
#         pfam.vote = ifelse(vote == stack, "p", as.character(pfam.vote)),
#         pfam.l_vote = ifelse(l_vote == stack, "p",  as.character(pfam.l_vote)),
#       )
#
#     ## Selection of parties
#     data_file_long[[m]] <- data_file_long[[m]] %>%
#       dplyr::filter(parfam_harmonized == "soc") %>%
#       dplyr::filter(not(party_name %in% c("Independents' Alliance",
#                                           "DENK",
#                                           "Italian Renewal")))
#   } else if (aggregation_level == "national") {
#     data_file_long[[m]] <- data_file_long[[m]] %>%
#       group_by(elec_id) %>%
#       dplyr::mutate(soc_pm = as.numeric("soc" %in% parfam_harmonized[outgoing_prime_minister_lag == 1])) %>%
#       ungroup() %>%
#       dplyr::filter(stack == 1)
#   }
# }
#
#
#
# ## ---- Aggregate (party electorates) ----
# micro_vars <- c(
#   "id",
#   "stack",
#   "weights",
#   "vote",
#   "l_vote",
#   "pid",
#   "lr_self",
#   "lr",
#   "male",
#   "age",
#   "stfdem",
#   "like",
#   "pfam.vote",
#   "pfam.l_vote"
# )
#
# data_file_agg <- list()
# for (m in mm) {
#   data_file_agg[[m]] <- data_file_long[[m]] %>%
#     dplyr::select(
#       -any_of(micro_vars)
#     ) %>%
#     distinct()
#
#   counts_long <- data_file_long[[m]] %>%
#     dplyr::select(elec_id,
#                   weights,
#                   pfam.vote,
#                   pfam.l_vote,
#                   cats.vote,
#                   cats.l_vote) %>%
#     dplyr::group_by(elec_id, pfam.l_vote, pfam.vote, .drop = FALSE) %>%
#     dplyr::summarize(
#       weights = sum(weights)
#     ) %>%
#     dplyr::ungroup()
#
#   counts_aux <- data_file_long[[m]] %>%
#     dplyr::group_by(elec_id) %>%
#     dplyr::summarize(cats.vote = unique(cats.vote),
#                      cats.l_vote = unique(cats.l_vote)) %>%
#     dplyr::ungroup()
#
#   counts_full <- counts_long %>%
#     dplyr::left_join(counts_aux ,
#                      by = c("elec_id")) %>%
#     dplyr::rowwise() %>%
#       dplyr::mutate(
#         weights = dplyr::case_when(
#           weights == 0.0 &
#             pfam.vote %in% cats.vote &
#             pfam.l_vote %in% cats.l_vote ~ 0.0,
#           weights == 0.0 &
#             (
#               not(pfam.vote %in% cats.vote) |
#                 not(pfam.l_vote %in% cats.l_vote)
#             ) ~ NA_real_,
#           TRUE ~ weights
#         )
#       ) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(-starts_with("cats")) %>%
#     pivot_wider(
#       names_from = c("pfam.l_vote", "pfam.vote"),
#       values_from =  weights,
#       names_sep = "_"
#     ) %>%
#     dplyr::mutate_if(is.numeric,
#                      .funs = ~ case_when(is.nan(.) ~ NA_real_,
#                                          is.na(.) ~ -1.0,
#                                          TRUE ~ .))
#
#   data_file_agg[[m]] <- data_file_agg[[m]] %>%
#     left_join(counts_full,
#               by = "elec_id")
# }
#
#
# ## Select
# for (m in mm) {
#   data_file_agg[[m]] <- data_file_agg[[m]] %>%
#     dplyr::select(-cats.vote,
#                   -cats.l_vote,
#                   -starts_with("map_"))
# }
#
#
# ## ---- Save ----
# save(data_file_agg, file = "dat/nes-proc/data_file_agg_soc_dem_absolute_disagg.rdata")
