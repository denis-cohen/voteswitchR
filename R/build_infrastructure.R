#' @title build_infrastructure
#'
#' @description Internal function called via the GUI of
#' \code{shiny/run_data_file/app.R} that performs data recoding and,
#' upon request, data mapping, imputation, and reshaping within each
#' selected electoral context.
#'
#' @return Returns and/or stores and object called data_file which contains
#' a list of data for all requested electoral contexts.
#'
#' @noRd

build_infrastructure <- function(folder_location = NULL,
                                 selected_concepts = NULL,
                                 selected_contexts = NULL,
                                 map = TRUE,
                                 map_vars = NULL,
                                 impute = TRUE,
                                 include_info_imp = FALSE,
                                 n_imp = 5L,
                                 seed = 19890213L,
                                 format = c("long", "wide"),
                                 existing_data_file = NULL,
                                 output_file_path = NULL) {

  ## ---- Checks ----
  cat("Building your data infrastructure.\n")
  cat("Supplied arguments:\n")
  print(c(as.list(environment())))

  ## ---- Dependencies ----
  ## Save package names as a vector of strings
  pkgs <-
    c(
      "countrycode",
      "dplyr",
      "magrittr",
      "lubridate",
      "rio",
      "tidyr",
      "hot.deck",
      "labelled"
    )

  ## Install uninstalled packages
  lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

  ## Load all packages to library and adjust options
  lapply(pkgs, library, character.only = TRUE)


  ## ---- Initial checks ----
  if (impute & not(map)) {
    stop(
      paste(
        "Imputation can only be performed after mapping.",
        "Please set map = TRUE.",
        sep = " "
      )
    )
  }

  if (format == "long" & not(map)) {
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
    dplyr::select(elec_id, party, party_harmonized, map_vote, map_lr,
                  any_of(map_vars))

  # ---- Selections ----
  ## Select concepts
  if (is.null(selected_concepts)) {
    selected_concepts <-
      concepts_df$concept[concepts$description %in%
                            c("drop",
                              "region",
                              "Vote choice (t)",
                              "Vote choice (t - 1)")]
  } else {
    selected_concepts <-
      concepts_df$concept[concepts$description %in%
                            c("drop",
                              "region",
                              "Vote choice (t)",
                              "Vote choice (t - 1)",
                              selected_concepts)]
  }

  ## Select contexts
  if (is.null(selected_concepts)) {
    selected_contexts <- available_data$elec_id
  }

  # ## ---- Initialize folder structure ----
  if (not(file.exists(folder_location))) {
    stop(
      paste("The sub-directory",
            folder_location,
            "does not exist in your current directory.",
            sep = " ")
    )
  }

  ## Check folders
  needed_folders <-
    available_data$folder_name[available_data$elec_id %in% selected_contexts]
  needed_files <-
    available_data$file_name[available_data$elec_id %in% selected_contexts]
  available_folders <- list.files(folder_location)
  missing_folders <- needed_folders[not(needed_folders %in% available_folders)]
  missing_files <- needed_files[not(needed_folders %in% available_folders)]

  ## Initialize folders
  if (length(missing_folders) == 0L) {
    cat("All folders found. Proceeding.\n")
  } else {
    initialize_folders <- readline(
      prompt = paste0("Folders \n",
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
        prompt = paste0("Folders created.\n",
                        "Please store all original data files in their ",
                        "respective folders before you proceed.\n\n",
                        "Make sure they are named exactly as follows:\n",
                        paste(paste(missing_folders, missing_files, sep = "/"),
                              collapse ="\n"),
                        "\n \nReady to proceed? [y/n]"
        )
      )

      if (not(put_files == "y")) {
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
    "lr_self",
    "stfdem",
    "male",
    "age",
    paste0("lr_", LETTERS),
    paste0("like_", LETTERS)
  )
  survey_p_vars <- c(paste("lr", LETTERS, sep = "_"),
                     paste("like", LETTERS, sep = "_"))
  mappings_p_vars <- names(mappings)[names(mappings) != "elec_id"]


  ## ---- Define sub-functions ----
  reshape_k <- function (x) {
    varying_x <-
      paste0(rep(paste0(
        c(mappings_p_vars, survey_p_vars_k_stubs), "_"
      ),
      each = n_prty), 1:n_prty)
    varying_na <- varying_x[not(varying_x %in% names(x))]
    for (v in varying_na) {
      x <- within(x, assign(v, NA_real_))
    }
    varying <- tibble(var = varying_x,
                      stubs = gsub(paste(paste0("_", 1:99), collapse = "|"),
                                   "",
                                   varying_x))
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
      arrange(id) %>%
      dplyr::select(any_of(
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
  if (is.null(existing_data_file)) {
    data_file <- list()
    data_file$data <- list()
    if (impute) {
      data_file$data_imp <- list()
      if (include_info_imp) {
        data_file$info_imp <- list()
      } else {
        data_file$info_imp <- NULL
      }
    } else {
      data_file$data_imp <- NULL
      data_file$info_imp <- NULL
    }
    data_file$info_aux <- list(
      map = map,
      impute = impute,
      n_imp = n_imp,
      seed = seed,
      format = format
    )
  } else {
    warning(
      paste(
        "You are updating 'selected_contexts' in an existing data file.",
        "Your arguments 'map', 'impute', 'n_imp', 'seed', and 'format'",
        "will be overwritten with those stored in the existing data file.\n",
        sep = " "
      ) %>%
        cat()
    )

    ## Load and attach existing file
    data_file <- import(existing_data_file)

    ## Overwrite arguments
    map <- data_file$info_aux$map
    impute <- data_file$info_aux$impute
    n_imp <- data_file$info_aux$n_imp
    seed <- data_file$info_aux$seed
    format <- data_file$info_aux$format
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
      filter(elec_id == j)
    data_path_j <- paste(folder_location,
                         available_data_j$folder_name,
                         available_data_j$file_name,
                         sep = "/")

    data_j <- import(data_path_j) %>%
      dplyr::mutate_if(is.factor, as.character)

    ## Select recodes
    recodes_j <- recodes %>%
      dplyr::filter(elec_id == j) %>%
      dplyr::select(-country_name,
             -year,
             -election_date,
             -iso2c,
             -elec_id,
             -source)

    ## Select concepts
    names_selected_concepts_j <- c(
      selected_concepts[not(is.na(available_data_j[selected_concepts]))],
      recodes_j$concept[recodes_j$concept %in% selected_concepts]
    ) %>%
      unique()

    selected_concepts_j <-
      as.vector(available_data_j[names_selected_concepts_j])

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
      ##Subset
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
    selected_concepts_j[not(is.na(selected_concepts_j))] <-
      paste0("ov_", selected_concepts_j[not(is.na(selected_concepts_j))])

    ## Define general meta variables
    data_j$exp_iso2c <- available_data_j$iso2c
    data_j$exp_election_date <-
      as.Date(available_data_j$election_date)
    data_j$exp_year <- year(data_j$exp_election_date)
    data_j$exp_elec_id <- j
    data_j$exp_id <-
      paste(j, sprintf("%04d", 1:nrow(data_j)), sep = "-")

    ## Rename selected concepts
    rename_concepts <-
      paste0("dplyr::mutate(data_j, ",
             paste(
               paste(names_selected_concepts_j[not(is.na(selected_concepts_j))],
                     selected_concepts_j[not(is.na(selected_concepts_j))],
                     sep = " = "),
               collapse = ", "
             ),
             ")")
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
        filter(concept == conname_jk) %>%
        select(-concept)

      ## Filter relevant operations
      recodes_jk <- recodes_jk %>%
        select(which(not(is.na(
          slice(recodes_jk, 1)
        ))))

      ## If applicable: extract recodes
      if (any(grepl("recode", names(recodes_jk)))) {
        recode_values_jk <- recodes_jk %>%
          select(starts_with("recode_"))
        recode_values_jk <-
          paste0("case_when(",
                 paste(
                   paste0(
                     "data_j[[expname_jk]] == ",
                     recode_values_jk$recode_from,
                     " ~ ",
                     recode_values_jk$recode_to
                   ),
                   collapse = ", "
                 ),
                 ", TRUE ~ data_j[[expname_jk]])")
      }

      ## Other operations are single-row
      recodes_jk <- recodes_jk %>%
        slice(1)

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
          data_j[[expname_jk]] <- na_if(data_j[[expname_jk]], na_vals[i])
        }
      }

      ## valid_range_min, valid_range_max
      if ("valid_range_min" %in% names(recodes_jk)) {
        data_j[[expname_jk]] <-
          ifelse(data_j[[expname_jk]] < recodes_jk[["valid_range_min"]],
                 NA,
                 data_j[[expname_jk]])
      }
      if ("valid_range_max" %in% names(recodes_jk)) {
        data_j[[expname_jk]] <-
          ifelse(data_j[[expname_jk]] > recodes_jk[["valid_range_max"]],
                 NA,
                 data_j[[expname_jk]])
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
        sapply(names(recodes_jk), function(x)
          grepl("replace", x))
      if (any(is_replace)) {
        replace_jk <- recodes_jk[which(is_replace)]
        replace_if <-
          t(replace_jk[startsWith(names(replace_jk), "replace_if")])
        replace_by <-
          t(replace_jk[startsWith(names(replace_jk), "replace_by")])
        replace_type <- ifelse(grepl("exp_", replace_if) |
                                 grepl("exp_", replace_by),
                               "deriv",
                               "orig")
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
            filter(replace_type == "orig")

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
                  "with(data_j, case_when(",
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
            bind_rows(replace_jk %>%
                        filter(replace_type == "deriv"))
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
              "data_j %>% rowwise() %>% dplyr::mutate(",
              k,
              " = ",
              replace_j_deriv[[k]]$replace_by,
              ")",
              " %>% ungroup()"
            )

          data_j <- replace_values_jk %>%
            str2lang() %>%
            eval()
        } else {
          for (r in seq_len(nrow(replace_j_deriv[[k]]))) {
            replace_values_jk <-
              paste0(
                "with(data_j, case_when(",
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
      select(starts_with("exp_")) %>%
      rename_at(
        .vars = vars(starts_with("exp_")),
        .funs = function(x)
          gsub("^exp_", "", x)
      ) %>%
      select_if(~ sum(!is.na(.)) > 0) %>%
      dplyr::mutate(weights = if (exists('dwght', where = .)) {
        dwght
      } else if (exists('swght', where = .)) {
        swght
      } else if (exists('pwght', where = .)) {
        pwght
      } else {
        1
      }) %>%
      dplyr::mutate(weights = ifelse(is.na(weights), 1, weights)) %>%
      dplyr::mutate(weights = ifelse(weights == 0, 1, weights)) %>%
      dplyr::mutate(weights = weights / mean(weights)) %>%
      dplyr::select(any_of(core_concepts)) %>%
      dplyr::mutate_all(.funs = ~ ifelse(is.nan(.), NA, .))

    ## Age filter
    if (exists('age', where = data_j)) {
      data_j <- data_j %>%
        filter(is.na(age) | (age >= 18 & age <= 130))
    }

    ## Split by region
    if (available_data_j$iso2c == "BE") {
      data_j <-
        list(
          data_j %>%
            filter(region == 1) %>%
            dplyr::mutate(
              elec_id = gsub("BE", "BE-VL", elec_id),
              iso2c = gsub("BE", "BE-VL", iso2c),
              id = gsub("BE", "BE-VL", id)
            ),
          data_j %>%
            filter(region == 0) %>%
            dplyr::mutate(
              elec_id = gsub("BE", "BE-WA", elec_id),
              iso2c = gsub("BE", "BE-WA", iso2c),
              id = gsub("BE", "BE-WA", id)
            )
        )
      kk <- c(gsub("BE", "BE-VL", j),
              gsub("BE", "BE-WA", j))
    } else {
      data_j <- list(data_j)
      kk <- j
    }
    names(data_j) <- kk

    for (k in kk) {
      ## Get data
      data_k <- as.data.frame(data_j[[k]]) %>%
        labelled::remove_attributes(c("groups", "label", "labels", "format.stata"))

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
          unique(sapply(survey_p_vars_k, function (x)
            gsub("(.+?)(\\_.*)", "\\1", x)))

        ## Context data
        mappings_k <- mappings %>%
          filter(elec_id == k) %>%
          filter(map_vote %in% c(sort(unique(
            data_k$vote
          )),
          sort(unique(
            data_k$l_vote
          )))) %>%
          dplyr::select(-elec_id)

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
            warning("Context not mapped: No contextual information on parties.\n") %>%
              cat()
          }
          if (is.null(no_l_vote)) {
            warning("Context not mapped: No vote recall data for previous election.\n") %>%
              cat()
          }
        } else {
          ## Initialize containers
          data_k <- data_k %>%
            dplyr::mutate(vote_new = NA_integer_,
                   l_vote_new = NA_integer_)

          if ("pid" %in% names(data_k)) {
            data_k <- data_k %>%
              dplyr::mutate(pid_new = NA_integer_)
          }

          ## Map
          for (p in 1:n_prty) {
            p_alph <- mappings_k$map_lr[p]
            data_k$vote_new <-
              ifelse(mappings_k$map_vote[p] == data_k$vote,
                     p,
                     data_k$vote_new)
            data_k$l_vote_new <-
              ifelse(mappings_k$map_vote[p] == data_k$l_vote,
                     p,
                     data_k$l_vote_new)
            if ("pid" %in% names(data_k)) {
              data_k$pid_new <-
                ifelse(mappings_k$map_vote[p] == data_k$pid,
                       p,
                       data_k$pid_new) # new PID ID
            }

            for (v in mappings_p_vars) {
              # assign mappings party vars
              data_k <- within(data_k,
                               assign(paste(v, p , sep = "_"),
                                      mappings_k[p, v]))
            }
            if (nchar(p_alph) > 0 & not(is.na(p_alph))) {
              # assign survey party vars
              for (v in survey_p_vars_k_stubs) {
                if (paste(v, p_alph, sep = "_") %in% survey_p_vars_k) {
                  data_k <-
                    within(data_k,
                           assign(paste(v, p , sep = "_"),
                                  data_k[, paste(v, p_alph, sep = "_")]))
                }
              }
            }
          }

          ## Recode numerical format
          data_k <- data_k %>%
            dplyr::rename(vote_old = vote,
                          l_vote_old = l_vote) %>%
            dplyr::rename(vote = vote_new,
                          l_vote = l_vote_new) %>%
            dplyr::mutate(
              vote = case_when(
                is.na(vote) & part == 0 ~ 99L,
                is.na(vote) & is.na(vote_old) ~ NA_integer_,
                is.na(vote) &
                  not(vote_old %in% mappings_k$map_vote) ~ 98L,
                TRUE ~ vote
              ),
              l_vote = case_when(
                is.na(l_vote) & l_part == 0 ~ 99L,
                is.na(l_vote) & is.na(l_vote_old) ~ NA_integer_,
                is.na(l_vote) &
                  not(l_vote_old %in% mappings_k$map_vote) ~ 98L,
                TRUE ~ l_vote
              )
            )

          if ("pid" %in% names(data_k)) {
            data_k <- data_k %>%
              dplyr::rename(pid_old = pid) %>%
              dplyr::rename(pid = pid_new) %>%
              dplyr::mutate(pid = case_when(
                is.na(pid) & pid_old == 0 ~ 99L,
                is.na(pid) &
                  pid_old != 0 & not(is.na(pid_old)) ~ 98L,
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

          noms <- c("vote", "l_vote", "pid", "male")
          noms <- noms[noms %in% names(data_k)]
          imp_vars <- c(noms,
                        "lr_self",
                        "age",
                        "stfdem",
                        paste(rep(c(
                          "lr", "like"
                        ), each = n_prty), 1:n_prty, sep = "_"))
          imp_vars <- imp_vars[imp_vars %in% names(data_k)]
          vars <- c(ids, imp_vars)

          ## Remove all-missing rows
          all_na <-
            which(rowSums(is.na(data_k[, imp_vars])) == ncol(data_k[, imp_vars]))

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
              slice(-all_na)
          }

          ## Drop old/auxiliary variables
          data_k <- data_k %>%
            dplyr::select(-any_of(
              c(
                "vote_old",
                "part",
                "l_vote_old",
                "l_part",
                survey_p_vars_k
              )
            ))
        }
      }

      ## ---- Imputation ----
      if (impute & not(no_map) & not(no_l_vote)) {
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
          sapply(function(var)
            length(unique(var))) %>%
          max()

        data_k_imp <- hot.deck(
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
        if (include_info_imp) {
          data_file$info_imp[[k]] <- data_k_imp
          data_file$info_imp[[k]]$data <- NULL
        }
        data_file$data_imp[[k]] <- data_k_imp$data
      }

      ## ---- Reshaping ----
      if (format == "long" & not(no_map) & not(no_l_vote)) {
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
        if (not(no_map) & not(no_l_vote)) {
          data_file$data[[k]] <- data_k
        }
      }
    }
  } ## End loop through selected contexts

  ## Define class
  class(data_file) <- "voteswitchr_data_file"

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