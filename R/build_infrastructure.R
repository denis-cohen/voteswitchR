#' @title build_infrastructure
#'
#' @description Internal function called via the GUI of
#' \code{shiny/run_data_file/app.R} that performs data harmonization and,
#' upon request, data mapping, imputation, and reshaping within each
#' selected electoral context.
#'
#' @return Returns and/or stores and object \code{data_file} which contains
#' a list of data for all requested electoral contexts.
#'
#' @noRd

build_infrastructure <- function(folder_location = NULL,
                                 available_data = NULL,
                                 selected_concepts = NULL,
                                 selected_contexts = NULL,
                                 map = TRUE,
                                 impute = TRUE,
                                 n_imp = 5L,
                                 seed = 19890213L,
                                 rake = TRUE,
                                 aggregate = TRUE,
                                 format = c("long", "wide"),
                                 return_data = TRUE,
                                 return_data_imp = TRUE,
                                 return_agg_data = TRUE,
                                 return_agg_data_imp = TRUE,
                                 return_info_imp = FALSE,
                                 existing_data_file = NULL,
                                 output_file_path = NULL) {
  ## ---- Start ----
  cat("Building your data infrastructure.\n")
  cat("Supplied arguments:\n")
  args <- as.list(environment())
  print(c(args))

  ## ---- Initial checks ----
  if (impute & !(map)) {
    stop(
      paste(
        "Imputation can only be performed after mapping.",
        "Please set map = TRUE.",
        sep = " "
      )
    )
  }

  if (rake & !(map)) {
    stop(paste(
      "Raking can only be performed after mapping.",
      "Please set map = TRUE.",
      sep = " "
    ))
  }

  if (aggregate & !(map)) {
    stop(
      paste(
        "Aggregation requires mapped data.",
        'Please set format = "long" and map = TRUE.',
        sep = " "
      )
    )
  }

  if (format == "long" & !(map)) {
    stop(
      paste(
        "Long format data can only be generated after mapping.",
        "Please set map = TRUE.",
        sep = " "
      )
    )
  }

  ## ---- Input files ----
  mappings <- mappings %>%
    dplyr::select(
      elec_id,
      stack,
      peid,
      party,
      party_harmonized,
      map_vote,
      map_lr,
      vote_share,
      vote_share_lag,
      turnout,
      turnout_lag
    ) %>%
    dplyr::mutate(
      vote_share = dplyr::if_else(vote_share == 0.0,
        0.005,
        vote_share
      ),
      vote_share_lag = dplyr::if_else(vote_share_lag == 0.0,
        0.005,
        vote_share_lag
      ),
    )

  # ---- Selections ----
  ## Select concepts
  if (is.null(selected_concepts)) {
    selected_concepts <-
      concepts_df$concept[concepts_df$description %in%
        c(
          "drop",
          "region",
          "Vote choice (t)",
          "Vote choice (t - 1)"
        )]
  } else {
    selected_concepts <-
      concepts_df$concept[concepts_df$description %in%
        c(
          "drop",
          "region",
          "Vote choice (t)",
          "Vote choice (t - 1)",
          selected_concepts
        )]
  }

  ## Select contexts
  if (is.null(selected_concepts)) {
    selected_contexts <- available_data$elec_id
  }

  # ## ---- Initialize folder structure ----
  if (!(file.exists(folder_location))) {
    stop(
      paste(
        "The sub-directory",
        folder_location,
        "does not exist in your current directory.",
        sep = " "
      )
    )
  }

  ## Check folders
  needed_folders <-
    available_data$folder_name[available_data$elec_id %in% selected_contexts]
  needed_files <-
    available_data$file_name[available_data$elec_id %in% selected_contexts]
  available_folders <- list.files(folder_location)
  missing_folders <-
    needed_folders[!(needed_folders %in% available_folders)]
  missing_files <-
    needed_files[!(needed_folders %in% available_folders)]

  ## Initialize folders
  if (length(missing_folders) == 0L) {
    cat("All folders found. Proceeding.\n")
  } else {
    initialize_folders <- readline(
      prompt = paste0(
        "Folders \n",
        paste(missing_folders, collapse = "\n"),
        "\n not found. ",
        "Create folders in ",
        folder_location,
        "? [y/n]"
      )
    )

    if (initialize_folders == "y") {
      for (f in missing_folders) {
        dir.create(paste0(folder_location, f))
      }
      put_files <- readline(
        prompt = paste0(
          "Folders created.\n",
          "Please store all original data files in their ",
          "respective folders before you proceed.\n\n",
          "Make sure they are named exactly as follows:\n",
          paste(
            paste(missing_folders, missing_files, sep = "/"),
            collapse = "\n"
          ),
          "\n \nReady to proceed? [y/n]"
        )
      )

      if (!(put_files == "y")) {
        stop("Please come back when the original data files are ready.")
      }
    } else {
      stop("You need data folders for all selected contexts to proceed.")
    }
  }

  ## ---- Define auxiliary objects ----
  core_concepts <- c(
    "iso2c",
    "election_date",
    "year",
    "elec_id",
    "id",
    "weights",
    "region",
    "vote",
    "part",
    "l_vote",
    "l_part",
    "pid",
    "strength1",
    "strength2",
    "lr_self",
    "stfdem",
    "male",
    "age",
    paste0("lr_", LETTERS),
    paste0("like_", LETTERS)
  )
  survey_p_vars <- c(
    paste("lr", LETTERS, sep = "_"),
    paste("like", LETTERS, sep = "_")
  )
  mappings_p_vars <- names(mappings)[names(mappings) != "elec_id"]


  ## ---- Define sub-functions ----
  reshape_k <- function(x) {
    varying_x <-
      paste0(rep(paste0(
        c(mappings_p_vars, survey_p_vars_k_stubs), "_"
      ),
      each = n_prty
      ), 1:n_prty)
    varying_na <- varying_x[!(varying_x %in% names(x))]
    for (v in varying_na) {
      x <- within(x, assign(v, NA_real_))
    }
    varying <- tibble::tibble(
      var = varying_x,
      stubs = gsub(
        paste(paste0("_", 1:99), collapse = "|"),
        "",
        varying_x
      )
    )
    varying <- split(varying$var, varying$stubs)
    fix <- names(x)[!(names(x) %in% varying_x)]
    long_data <- stats::reshape(
      x,
      varying = varying,
      v.names = names(varying),
      idvar = "id",
      timevar = "stack",
      direction = "long"
    ) %>%
      dplyr::arrange(id) %>%
      dplyr::select(dplyr::any_of(
        c(
          "iso2c",
          "year",
          "election_date",
          "elec_id",
          "id",
          "stack",
          "weights",
          "vote",
          "l_vote",
          "pid",
          "strength1",
          "strength2",
          "lr_self",
          "lr",
          "like",
          "stfdem",
          "male",
          "age",
          mappings_p_vars
        )
      ))
    return(long_data)
  }

  ## ---- Initialize containers ----
  data_file <- list()
  data_file$info_aux <- args
  data_file$data <- list()
  if (impute) {
    data_file$data_imp <- list()
    if (return_info_imp) {
      data_file$info_imp <- list()
    } else {
      data_file$info_imp <- NULL
    }
  } else {
    data_file$data_imp <- NULL
    data_file$info_imp <- NULL
  }

  ## ---- Start loop through selected contexts ----
  counter <- 0L
  for (j in selected_contexts) {
    ## ---- Data in ----
    ## Show progress:
    counter <- counter + 1L
    paste0(
      "Recoding context ",
      counter,
      " of ",
      length(selected_contexts),
      ". Current context: ",
      j,
      ".\n"
    ) %>%
      cat()

    ## Import data
    available_data_j <- available_data %>%
      dplyr::filter(elec_id == j)
    data_path_j <- paste(
      folder_location,
      available_data_j$folder_name,
      available_data_j$file_name,
      sep = "/"
    )

    data_j <- rio::import(data_path_j) %>%
      dplyr::mutate_if(is.factor, as.character)

    ## Select recodes
    recodes_j <- recodes %>%
      dplyr::filter(elec_id == j) %>%
      dplyr::select(
        -country_name,
        -year,
        -election_date,
        -iso2c,
        -elec_id,
        -source
      )

    ## Select concepts
    names_selected_concepts_j <-
      c(
        selected_concepts[!(is.na(available_data_j[selected_concepts]))],
        recodes_j$concept[recodes_j$concept %in% selected_concepts]
      ) %>%
      unique()

    selected_concepts_j <-
      as.data.frame(available_data_j[names_selected_concepts_j])

    ## Adjust recodes_j
    recodes_j <- recodes_j %>%
      dplyr::filter(concept %in% names_selected_concepts_j)

    ## If applicable: Subset data
    ## Subset
    if ("drop2" %in% names_selected_concepts_j) {
      drop_if <- recodes_j[recodes_j$concept == "drop", "drop_if"] %>%
        gsub("drop2", selected_concepts_j$drop2, .) %>%
        gsub("drop", selected_concepts_j$drop, .)
    } else if ("drop" %in% names_selected_concepts_j) {
      drop_if <- recodes_j[recodes_j$concept == "drop", "drop_if"] %>%
        gsub("drop", selected_concepts_j$drop, .)
    }

    if ("drop" %in% names_selected_concepts_j) {
      ## Subset
      data_j <- paste0("subset(data_j, !(", drop_if, "))") %>%
        str2lang() %>%
        eval()

      ## Adjust selected concepts
      selected_concepts_j <-
        selected_concepts_j[-which(names_selected_concepts_j == "drop")]
      names_selected_concepts_j <-
        names_selected_concepts_j[-which(names_selected_concepts_j == "drop")]
    }

    ## Adjust original variable (ov) names
    names(data_j) <- paste0("ov_", names(data_j))
    selected_concepts_j[!(is.na(selected_concepts_j))] <-
      paste0("ov_", selected_concepts_j[!(is.na(selected_concepts_j))])

    ## Define general meta variables
    data_j$exp_iso2c <- available_data_j$iso2c
    data_j$exp_election_date <-
      as.Date(available_data_j$election_date)
    data_j$exp_year <- lubridate::year(data_j$exp_election_date)
    data_j$exp_elec_id <- j
    data_j$exp_id <-
      paste(j, sprintf("%04d", 1:nrow(data_j)), sep = "-")

    ## Rename selected concepts
    rename_concepts <-
      paste0(
        "dplyr::mutate(data_j, ",
        paste(
          paste(names_selected_concepts_j[!(is.na(selected_concepts_j))],
            selected_concepts_j[!(is.na(selected_concepts_j))],
            sep = " = "
          ),
          collapse = ", "
        ),
        ")"
      )
    data_j <- rename_concepts %>%
      str2lang() %>%
      eval()

    ## Container for replace by variable (after numeric recodes)
    replace_j_deriv <- data.frame()

    ## ---- Recodes (I) ----
    ## Start loop through selected concepts
    for (k in seq_along(selected_concepts_j)) {
      ## Define macros
      concept_jk <- selected_concepts_j[k]
      conname_jk <- names_selected_concepts_j[k]
      expname_jk <- paste0("exp_", conname_jk)

      ## Extract recode sequence
      recodes_jk <- recodes_j %>%
        dplyr::filter(concept == conname_jk) %>%
        dplyr::select(-concept)

      ## filter relevant operations
      recodes_jk <- recodes_jk %>%
        dplyr::select(which(!(is.na(
          dplyr::slice(recodes_jk, 1)
        ))))

      ## If applicable: extract recodes
      if (any(grepl("recode", names(recodes_jk)))) {
        recode_values_jk <- recodes_jk %>%
          dplyr::select(dplyr::starts_with("recode_"))
        recode_values_jk <-
          paste0(
            "dplyr::case_when(",
            paste(
              paste0(
                "data_j[[expname_jk]] == ",
                recode_values_jk$recode_from,
                " ~ ",
                recode_values_jk$recode_to
              ),
              collapse = ", "
            ),
            ", TRUE ~ data_j[[expname_jk]])"
          )
      }

      ## Other operations are single-row
      recodes_jk <- recodes_jk %>%
        dplyr::slice(1)

      ## start_empty; else generate copy
      if ("start_empty" %in% names(recodes_jk)) {
        data_j[[expname_jk]] <- NA_real_
      } else {
        data_j[[expname_jk]] <- data_j[[conname_jk]]
      }

      ## mvdecode
      if ("mvdecode" %in% names(recodes_jk)) {
        na_vals <- eval(str2lang(recodes_jk[["mvdecode"]]))
        for (i in seq_along(na_vals)) {
          data_j[[expname_jk]] <- dplyr::na_if(data_j[[expname_jk]], na_vals[i])
        }
      }

      ## valid_range_min, valid_range_max
      if ("valid_range_min" %in% names(recodes_jk)) {
        data_j[[expname_jk]] <-
          ifelse(data_j[[expname_jk]] < recodes_jk[["valid_range_min"]],
            NA,
            data_j[[expname_jk]]
          )
      }
      if ("valid_range_max" %in% names(recodes_jk)) {
        data_j[[expname_jk]] <-
          ifelse(data_j[[expname_jk]] > recodes_jk[["valid_range_max"]],
            NA,
            data_j[[expname_jk]]
          )
      }

      ## recode
      if (any(grepl("recode", names(recodes_jk)))) {
        data_j[[expname_jk]] <- recode_values_jk %>%
          str2lang() %>%
          eval()
      }

      ## rescale
      if ("rescale_add" %in% names(recodes_jk)) {
        data_j[[expname_jk]] <-
          data_j[[expname_jk]] + recodes_jk[["rescale_add"]]
      }
      if ("rescale_multiply" %in% names(recodes_jk)) {
        rescale_multiply <- recodes_jk[["rescale_multiply"]] %>%
          str2lang() %>%
          eval()
        data_j[[expname_jk]] <-
          data_j[[expname_jk]] * rescale_multiply
      }

      ## replace (by numeric/original variable)
      is_replace <-
        vapply(names(recodes_jk), function(x) {
          grepl("replace", x)
        })
      if (any(is_replace)) {
        replace_jk <- recodes_jk[which(is_replace)]
        replace_if <-
          t(replace_jk[startsWith(names(replace_jk), "replace_if")])
        replace_by <-
          t(replace_jk[startsWith(names(replace_jk), "replace_by")])
        replace_type <- ifelse(grepl("exp_", replace_if) |
          grepl("exp_", replace_by),
        "deriv",
        "orig"
        )
        replace_jk <- data.frame(
          concept_jk,
          conname_jk,
          expname_jk,
          replace_if = replace_if,
          replace_by = replace_by,
          replace_type = replace_type
        )

        if (any(replace_jk$replace_type == "orig")) {
          ## Operations involving replace by numeric / original variable values
          replace_jk_orig <- replace_jk %>%
            dplyr::filter(replace_type == "orig")

          for (r in seq_len(nrow(replace_jk_orig))) {
            if (replace_jk_orig$replace_if[r] == "always") {
              replace_values_jk <-
                paste0(
                  "data_j %>% dplyr::mutate(",
                  expname_jk,
                  " = ",
                  replace_jk_orig$replace_by[r],
                  ")"
                )

              data_j <- replace_values_jk %>%
                str2lang() %>%
                eval()
            } else {
              replace_values_jk <-
                paste0(
                  "with(data_j, dplyr::case_when(",
                  paste0(
                    replace_jk_orig$replace_if[r],
                    " ~ ",
                    replace_jk_orig$replace_by[r]
                  ),
                  ", TRUE ~ ",
                  expname_jk,
                  "))"
                )

              data_j[[expname_jk]] <- replace_values_jk %>%
                str2lang() %>%
                eval()
            }
          }
        }

        if (any(replace_jk$replace_type == "deriv")) {
          replace_j_deriv <- replace_j_deriv %>%
            dplyr::bind_rows(replace_jk %>%
              dplyr::filter(replace_type == "deriv"))
        }
      }
    } ## End loop through selected concepts


    ## ---- Recodes (Replace if/by derived concepts) ----
    if (nrow(replace_j_deriv > 0)) {
      replace_j_deriv <-
        split(replace_j_deriv, replace_j_deriv$expname_jk)

      for (k in names(replace_j_deriv)) {
        if (any(replace_j_deriv[[k]]$replace_if == "always")) {
          replace_values_jk <-
            paste0(
              "data_j %>% dplyr::rowwise() %>% dplyr::mutate(",
              k,
              " = ",
              replace_j_deriv[[k]]$replace_by,
              ")",
              " %>% dplyr::ungroup()"
            )

          data_j <- replace_values_jk %>%
            str2lang() %>%
            eval()
        } else {
          for (r in seq_len(nrow(replace_j_deriv[[k]]))) {
            replace_values_jk <-
              paste0(
                "with(data_j, dplyr::case_when(",
                paste0(
                  replace_j_deriv[[k]]$replace_if[r],
                  " ~ ",
                  replace_j_deriv[[k]]$replace_by[r]
                ),
                ", TRUE ~ ",
                k,
                "))"
              )

            data_j[[k]] <- replace_values_jk %>%
              str2lang() %>%
              eval()
          }
        }
      }
    }

    ## ---- Post-Recode Processing ----
    data_j <- data_j %>%
      dplyr::select(dplyr::starts_with("exp_")) %>%
      dplyr::rename_at(
        .vars = dplyr::vars(dplyr::starts_with("exp_")),
        .funs = function(x) {
          gsub("^exp_", "", x)
        }
      ) %>%
      dplyr::select_if(~ sum(!is.na(.)) > 0) %>%
      dplyr::mutate(weights = if (exists("dwght", where = .)) {
        dwght
      } else if (exists("swght", where = .)) {
        swght
      } else if (exists("pwght", where = .)) {
        pwght
      } else {
        1
      }) %>%
      dplyr::mutate(weights = ifelse(is.na(weights), 1, weights)) %>%
      dplyr::mutate(weights = ifelse(weights == 0, 1, weights)) %>%
      dplyr::mutate(weights = weights / mean(weights)) %>%
      dplyr::select(dplyr::any_of(core_concepts)) %>%
      dplyr::mutate_all(.funs = ~ ifelse(is.nan(.), NA, .))

    ## Age filter
    if (exists("age", where = data_j)) {
      data_j <- data_j %>%
        dplyr::filter(is.na(age) | (age >= 18 & age <= 130))
    }

    ## Split by region
    if (available_data_j$iso2c == "BE") {
      data_j <-
        list(
          data_j %>%
            dplyr::filter(region == 1) %>%
            dplyr::mutate(
              elec_id = gsub("BE", "BE-VL", elec_id),
              iso2c = gsub("BE", "BE-VL", iso2c),
              id = gsub("BE", "BE-VL", id)
            ),
          data_j %>%
            dplyr::filter(region == 0) %>%
            dplyr::mutate(
              elec_id = gsub("BE", "BE-WA", elec_id),
              iso2c = gsub("BE", "BE-WA", iso2c),
              id = gsub("BE", "BE-WA", id)
            )
        )
      kk <- c(
        gsub("BE", "BE-VL", j),
        gsub("BE", "BE-WA", j)
      )
    } else {
      data_j <- list(data_j)
      kk <- j
    }
    names(data_j) <- kk

    for (k in kk) {
      ## Get data
      data_k <- as.data.frame(data_j[[k]]) %>%
        labelled::remove_attributes(
          c("groups", "label", "labels", "format.stata"))

      ## ---- Mapping ----
      if (map) {
        paste0(
          "Mapping context ",
          counter,
          " of ",
          length(selected_contexts),
          ". Current context: ",
          k,
          ".\n"
        ) %>%
          cat()

        ## Party-specific survey-vars
        survey_p_vars_k <-
          survey_p_vars[survey_p_vars %in% names(data_k)]
        survey_p_vars_k_stubs <-
          unique(vapply(survey_p_vars_k, function(x) {
            gsub("(.+?)(\\_.*)", "\\1", x)
          }))

        ## Context data
        mappings_k <- mappings %>%
          dplyr::filter(elec_id == k) %>%
          dplyr::filter(map_vote %in% c(
            sort(unique(
              data_k$vote
            )),
            sort(unique(
              data_k$l_vote
            ))
          )) %>%
          dplyr::select(-elec_id) %>%
          as.data.frame()

        ## Meta information
        prty <- unique(mappings_k$party_harmonized)
        n_prty <- nrow(mappings_k)
        na_vote <- is.na(data_k$vote)
        no_l_vote <- mean(is.na(data_k$l_vote)) > .75
        no_map <- nrow(mappings_k) == 0

        ## Mapping
        if (no_map | no_l_vote) {
          ## Warnings
          if (no_map == 0) {
            warning("Context not mapped:
                    No contextual information on parties.\n") %>%
              cat()
          }
          if (is.null(no_l_vote)) {
            warning("Context not mapped:
                    No vote recall data for previous election.\n") %>%
              cat()
          }
        } else {
          ## Initialize containers
          data_k <- data_k %>%
            dplyr::mutate(
              vote_new = NA_integer_,
              l_vote_new = NA_integer_
            )

          if ("pid" %in% names(data_k)) {
            data_k <- data_k %>%
              dplyr::mutate(pid_new = NA_integer_)
          }

          ## Map
          for (p in 1:n_prty) {
            p_num <- mappings_k$stack[p]
            p_alph <- mappings_k$map_lr[p]
            data_k$vote_new <-
              ifelse(mappings_k$map_vote[p] == data_k$vote,
                p_num,
                data_k$vote_new
              )
            data_k$l_vote_new <-
              ifelse(mappings_k$map_vote[p] == data_k$l_vote,
                p_num,
                data_k$l_vote_new
              )

            if ("pid" %in% names(data_k)) {
              data_k$pid_new <-
                ifelse(mappings_k$map_vote[p] == data_k$pid,
                  p_num,
                  data_k$pid_new
                ) # new PID ID
            }

            for (v in mappings_p_vars) {
              # assign mappings party vars
              data_k <- within(
                data_k,
                assign(
                  paste(v, p_num, sep = "_"),
                  mappings_k[p, v]
                )
              )
            }
            if (nchar(p_alph) > 0 & !(is.na(p_alph))) {
              # assign survey party vars
              for (v in survey_p_vars_k_stubs) {
                if (paste(v, p_alph, sep = "_") %in% survey_p_vars_k) {
                  data_k <-
                    within(
                      data_k,
                      assign(
                        paste(v, p_num, sep = "_"),
                        data_k[, paste(v, p_alph, sep = "_")]
                      )
                    )
                }
              }
            }
          }
          ## Recode numerical format
          data_k <- data_k %>%
            dplyr::rename(
              vote_old = vote,
              l_vote_old = l_vote
            ) %>%
            dplyr::rename(
              vote = vote_new,
              l_vote = l_vote_new
            ) %>%
            dplyr::mutate(
              vote = dplyr::case_when(
                is.na(vote) & part == 0 ~ 99L,
                is.na(vote) & is.na(vote_old) ~ NA_integer_,
                is.na(vote) &
                  !(vote_old %in% mappings_k$map_vote) ~ 98L,
                TRUE ~ vote
              ),
              l_vote = dplyr::case_when(
                is.na(l_vote) & l_part == 0 ~ 99L,
                is.na(l_vote) & is.na(l_vote_old) ~ NA_integer_,
                is.na(l_vote) &
                  !(l_vote_old %in% mappings_k$map_vote) ~ 98L,
                TRUE ~ l_vote
              )
            )

          if ("pid" %in% names(data_k)) {
            data_k <- data_k %>%
              dplyr::rename(pid_old = pid) %>%
              dplyr::rename(pid = pid_new) %>%
              dplyr::mutate(pid = dplyr::case_when(
                is.na(pid) & pid_old == 0 ~ 99L,
                is.na(pid) &
                  pid_old != 0 & !(is.na(pid_old)) ~ 98L,
                TRUE ~ pid
              ))
          }

          ## Variable Selection
          ids <- c(
            "iso2c",
            "election_date",
            "year",
            "elec_id",
            "id",
            "weights",
            "region",
            paste(rep(mappings_p_vars, each = n_prty), 1:n_prty, sep = "_")
          )
          ids <- ids[ids %in% names(data_k)]

          noms <- c("vote", "l_vote", "pid", "male", "strength1", "strength2")
          noms <- noms[noms %in% names(data_k)]
          imp_vars <- c(
            noms,
            "lr_self",
            "age",
            "stfdem",
            paste(rep(c(
              "lr", "like"
            ), each = n_prty), 1:n_prty, sep = "_")
          )
          imp_vars <- imp_vars[imp_vars %in% names(data_k)]
          vars <- c(ids, imp_vars)

          ## Remove all-missing rows
          all_na <-
            which(rowSums(
              is.na(data_k[, imp_vars])) == ncol(data_k[, imp_vars]))

          if (length(all_na) >= 1) {
            warning(
              paste0(
                "Observations with IDs ",
                paste(data_k$id[all_na], collapse = " "),
                " are NA on all variables and will thus be dropped.\n"
              )
            ) %>%
              cat()
            data_k <- data_k %>%
              dplyr::slice(-all_na)
          }

          ## Drop old/auxiliary variables
          data_k <- data_k %>%
            dplyr::select(-dplyr::any_of(
              c(
                "vote_old",
                "part",
                "l_vote_old",
                "l_part",
                "pid_old",
                survey_p_vars_k
              )
            ))

          ## Save to data file
          data_file$data[[k]] <- data_k
        }
      } else {
        data_file$data[[k]] <- data_k
      }

      ## ---- Imputation ----
      set.seed(seed)
      if (impute & !(no_map) & !(no_l_vote)) {
        paste0(
          "Imputing context ",
          counter,
          " of ",
          length(selected_contexts),
          ". Current context: ",
          k,
          ".\n"
        ) %>%
          cat()

        ## Maximum number of categories in discrete variables
        max_cats <- data_k[noms] %>%
          vapply(function(var) {
            length(unique(var))
          }) %>%
          max()

        data_k_imp <- hot.deck::hot.deck(
          data_k[, vars],
          method = "p.draw",
          m = n_imp,
          impContinuous = "mice",
          IDvars = ids,
          cutoff = max_cats,
          optimizeSD = TRUE,
          seed = seed
        )

        ## Store to data_file
        if (return_info_imp) {
          data_file$info_imp[[k]] <- data_k_imp
          data_file$info_imp[[k]]$data <- NULL
        }
        data_file$data_imp[[k]] <- data_k_imp$data
      }

      ## ---- Raking ----
      if (rake) {
        paste0(
          "Raking vote switching weights for context ",
          counter,
          " of ",
          length(selected_contexts),
          ". Current context: ",
          k,
          ".\n"
        ) %>%
          cat()

        ## True marginals
        levels_vote <- sort(na.omit(unique(data_k$vote)))
        levels_l_vote <- sort(na.omit(unique(data_k$l_vote)))
        mappings_k_vote <- mappings_k %>%
          dplyr::select(stack, vote_share, turnout) %>%
          dplyr::filter(stack %in% levels_vote) %>%
          dplyr::mutate(
            vote_share =
              ifelse(is.na(vote_share), 0.005, vote_share)
          ) %>%
          na.omit()
        mappings_k_l_vote <- mappings_k %>%
          dplyr::select(stack, vote_share_lag, turnout_lag) %>%
          dplyr::filter(stack %in% levels_l_vote) %>%
          dplyr::mutate(
            vote_share_lag =
              ifelse(is.na(vote_share_lag), 0.005, vote_share_lag)
          ) %>%
          na.omit()
        vote <- mappings_k_vote$vote_share
        turnout <- unique(mappings_k_vote$turnout)
        turnout_na <- is.na(turnout)
        l_vote <- mappings_k_l_vote$vote_share_lag
        l_turnout <- unique(mappings_k_l_vote$turnout_lag)
        l_turnout_na <- is.na(l_turnout)

        # Determine if others and/or non-voters are in the data
        oth_exist <- any(!(levels_vote %in% c(mappings_k$stack, 99L)))
        l_oth_exist <- any(!(levels_l_vote %in% c(mappings_k$stack, 99L)))
        non_exist <- any(levels_vote == 99L)
        l_non_exist <- any(levels_l_vote == 99L)

        if (turnout_na | l_turnout_na) {
          warning(
            paste0(
              "Turnout information incomplete. Raking skipped. ",
              "Returning unraked weights for this context.\n"
            )
          ) %>%
            cat()
        } else {
          ## Check vote shares
          sum_vote <- sum(vote, na.rm = TRUE)
          sum_l_vote <- sum(l_vote, na.rm = TRUE)

          if (sum_vote >= 1 & oth_exist) {
            if (sum_vote >= 1.005) {
              warning("Combined vote shares exceed 1. Please check.")
            }
            oth_vote <- 0.005
          } else if (!oth_exist) {
            oth_vote <- NULL
          } else {
            oth_vote <- 1 - sum_vote
          }

          if (sum_l_vote >= 1 & l_oth_exist) {
            if (sum_l_vote >= 1.005) {
              warning("Combined vote shares exceed 1. Please check.")
            }
            oth_l_vote <- 0.005
          } else if (!l_oth_exist) {
            oth_l_vote <- NULL
          } else {
            oth_l_vote <- 1 - sum_l_vote
          }

          ## Continue true marginals
          if (oth_exist) {
            oth_val <- 98L
            vote <- c(vote, oth_vote)
          } else {
            oth_val <- NULL
          }

          if (l_oth_exist) {
            l_oth_val <- 98L
            l_vote <- c(l_vote, oth_l_vote)
          } else {
            l_oth_val <- NULL
          }

          if (non_exist) {
            non_val <- 99L
            vote <- na.omit(c(vote * turnout, 1 - turnout))
          } else {
            non_val <- NULL
          }

          if (l_non_exist) {
            l_non_val <- 99L
            l_vote <- na.omit(c(l_vote * l_turnout, 1 - l_turnout))
          } else {
            l_non_val <- NULL
          }

          # Name true marginals
          names(vote) <-
            as.character(c(mappings_k_vote$stack, oth_val, non_val))
          names(l_vote) <-
            as.character(c(mappings_k_l_vote$stack, l_oth_val, l_non_val))

          ## Raking
          # imputed data
          if (impute) {
            for (m in seq_len(n_imp)) {
              data_k_imp$data[[m]]$raked_weights <- anesrake::anesrake(
                inputter = list(
                  vote = vote,
                  l_vote = l_vote
                ),
                dataframe = data_k_imp$data[[m]] %>%
                  dplyr::mutate_at(
                    .vars = dplyr::vars(vote, l_vote),
                    .funs = as.factor
                  ),
                caseid = data_k_imp$data[[m]]$id,
                weightvec = data_k_imp$data[[m]]$weights,
                pctlim = 0.005
              )$weightvec
            }
            data_file$data_imp[[k]] <- data_k_imp$data
          }

          # original data
          data_k$raked_weights <- anesrake::anesrake(
            inputter = list(
              vote = vote,
              l_vote = l_vote
            ),
            dataframe = data_k %>%
              dplyr::mutate_at(
                .vars = dplyr::vars(vote, l_vote),
                .funs = as.factor
              ),
            caseid = seq_along(data_k$id),
            weightvec = data_k$weights,
            pctlim = 0.005
          )$weightvec
          data_file$data[[k]] <- data_k
        }
      }

      ## ---- Reshaping ----
      if (format == "long" & !(no_map) & !(no_l_vote)) {
        paste0(
          "Reshaping context ",
          counter,
          " of ",
          length(selected_contexts),
          ". Current context: ",
          j,
          ".\n"
        ) %>%
          cat()

        data_k <- reshape_k(data_k)

        ## Store to_data_file
        if (impute) {
          data_file$data_imp[[k]] <- lapply(data_k_imp$data, reshape_k)
        }

        ## ---- Output ----
        if (!(no_map) & !(no_l_vote)) {
          data_file$data[[k]] <- data_k
        }
      }
    }
  } ## End loop through selected contexts

  ## ---- Append data ----
  cat("Integrating context-specific data.\n")
  data_file$data <- dplyr::bind_rows(data_file$data) %>%
    dplyr::ungroup()
  if (impute) {
    data_file$data_imp <-
      lapply(seq_len(n_imp), function(m) {
        lapply(data_file$data_imp, function(dat) {
          dat[[m]]
        })
      })
    for (m in seq_len(n_imp)) {
      data_file$data_imp[[m]] <-
        dplyr::bind_rows(data_file$data_imp[[m]]) %>%
        dplyr::ungroup()
    }
  }

  ## ---- Aggregation ----
  cat("Aggreagting vote switching data.\n")
  if (aggregate) {
    ## Raw data
    if (format == "wide") {
      data_file$switches <-
        aggregate_switches(
          data_file$data,
          context_vars = "elec_id",
          weights_var = "weights",
          switch_from = "l_vote",
          switch_to = "vote"
        )
      if (rake) {
        data_file$raked_switches <-
          aggregate_switches(
            data_file$data,
            context_vars = "elec_id",
            weights_var = "raked_weights",
            switch_from = "l_vote",
            switch_to = "vote"
          )
      }
    } else if (format == "long") {
      data_file$switches <-
        aggregate_switches(
          data_file$data %>%
            dplyr::filter(stack == 1),
          context_vars = "elec_id",
          weights_var = "weights",
          switch_from = "l_vote",
          switch_to = "vote"
        )
      if (rake) {
        data_file$raked_switches <-
          aggregate_switches(
            data_file$data %>%
              dplyr::filter(stack == 1),
            context_vars = "elec_id",
            weights_var = "raked_weights",
            switch_from = "l_vote",
            switch_to = "vote"
          )
      }
    }

    ## Imputed
    if (impute) {
      data_file$switches_imp <- list()
      if (format == "wide") {
        for (m in seq_len(n_imp)) {
          data_file$switches_imp[[m]] <-
            aggregate_switches(
              data_file$data_imp[[m]],
              context_vars = "elec_id",
              weights_var = "weights",
              switch_from = "l_vote",
              switch_to = "vote"
            )
        }
        if (rake) {
          data_file$raked_switches_imp <- list()
          for (m in seq_len(n_imp)) {
            data_file$raked_switches_imp[[m]] <-
              aggregate_switches(
                data_file$data_imp[[m]],
                context_vars = "elec_id",
                weights_var = "raked_weights",
                switch_from = "l_vote",
                switch_to = "vote"
              )
          }
        }
      } else if (format == "long") {
        for (m in seq_len(n_imp)) {
          data_file$switches_imp[[m]] <-
            aggregate_switches(
              data_file$data_imp[[m]] %>%
                dplyr::filter(stack == 1),
              context_vars = "elec_id",
              weights_var = "weights",
              switch_from = "l_vote",
              switch_to = "vote"
            )
        }
        if (rake) {
          data_file$raked_switches_imp <- list()
          for (m in seq_len(n_imp)) {
            data_file$raked_switches_imp[[m]] <-
              aggregate_switches(
                data_file$data_imp[[m]] %>%
                  dplyr::filter(stack == 1),
                context_vars = "elec_id",
                weights_var = "raked_weights",
                switch_from = "l_vote",
                switch_to = "vote"
              )
          }
        }
      }
    }
  }

  ## ---- Select return values ----
  if (!(return_data)) {
    data_file$data <- NULL
  }
  if (!(return_data_imp)) {
    data_file$data_imp <- NULL
  }
  if (aggregate & !(return_agg_data)) {
    data_file$switches <- NULL
  }
  if (aggregate & !(return_agg_data_imp)) {
    data_file$switches_imp <- NULL
  }

  ## Define class
  class(data_file) <- "voteswitchR_data_file"

  ## ---- Value ----
  cat("Data processing completed. Storing data file(s).\n")
  if (is.null(output_file_path)) {
    list2env(list("data_file" = data_file), envir = .GlobalEnv)
    cat("An object 'data_file' has been stored in the global environment.\n")
  } else {
    save(data_file, file = output_file_path)
    cat(paste0("An object 'data_file' has been saved at ", output_file_path))
  }
}
