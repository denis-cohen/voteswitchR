`%>%` <- magrittr::`%>%`

#### UI PART ####
NUM_PAGES <- 3
ui <- shiny::fluidPage(
  shinyalert::useShinyalert(),
  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$style(HTML("
      .error {
        border: 5px solid red;
      }
      .warning {
        border: 5px solid orange;
      }
      .success {
        border: 5px solid green;
      }"))
  ),
  shiny::titlePanel("Vote Switching Data"),
  shinyjs::hidden(lapply(seq(NUM_PAGES), function(i) {
    # Landing Page (Concepts+Contexts Selection)
    if (i == 1) {
      htmltools::div(
        class = "page",
        id = paste0("step", i),
        h4('Concepts:'),
        shiny::actionLink("selectall", "Select All"),
        shiny::fluidRow(column(
          width = 4, uiOutput("variables_concepts")
        )),
        h4('Contexts:'),
        shiny::actionLink("resetall", "Reset Context Selection"),
        DT::dataTableOutput('countries_year')
      )
      # Data Download/Selection
    } else if (i == 2) {
      htmltools::div(
        class = "page",
        id = paste0("step", i),
        h4('Preparation:'),
        h5(
          "In case you haven't already done, please download the selected data manually using the provided download links and put the files into the respective folders."
        ),
        shiny::textInput("dir", "Input directory", placeholder = "e.g. ./data"),
        shiny::actionButton("find_files", "Find files"),
        shiny::actionButton("structure_creation", "Create initial folder structure"),
        h4('Selected Contexts:'),
        h5("Please Note: File format must be either .sav, .dta or .rdata."),
        shiny::actionLink("refresh_table", "Reset Context Table"),
        DT::dataTableOutput('data_selected')
      )
      # Parameter Setting for infrastructure function
    } else if (i == 3) {
      htmltools::div(
        class = "page",
        id = paste0("step", i),
        h4('Execution Specification:'),
        shiny::checkboxInput("map_par", "map", value = TRUE),
        shiny::checkboxInput("impute_par", "impute", value = TRUE),
        shiny::textInput(
          "n_imp_par",
          "n_imp",
          value = "5",
          placeholder = "e.g. 5"
        ),
        shiny::textInput(
          "seed_par",
          "seed",
          value = "20210910",
          placeholder = "e.g. 20210910"
        ),
        shiny::checkboxInput("rake_par", "rake", value = TRUE),
        shiny::checkboxInput("aggregate_par", "aggregate", value = TRUE),
        shiny::radioButtons(
          "format_par",
          selected = "long",
          "format",
          c("long" = "long",
            "wide" = "wide")
        ),
        shiny::checkboxInput("return_data_par", "return_data", value = TRUE),
        shiny::checkboxInput("return_data_imp_par", "return_data_imp", value = TRUE),
        shiny::checkboxInput("return_agg_data_par", "return_agg_data", value = TRUE),
        shiny::checkboxInput("return_agg_data_imp_par", "return_agg_data_imp", value = TRUE),
        shiny::checkboxInput("return_info_imp_par", "return_info_imp", value = TRUE),
        shiny::textInput(
          "output_file_path_par",
          "output_file_path (If empty, the output file is stored in the R Environment)",
          placeholder = "e.g. path/to/data/data_file.RData"
        )
      )
    }
  })),
  htmltools::br(),
  shiny::actionButton("prevBtn", "< Previous"),
  shiny::actionButton("nextBtn", "Next >")
)

#### BACKEND PART ####
server <- function(input, output, session) {
  # Set up pages (+ navigation)
  rv <- shiny::reactiveValues(page = 1)
  assign("data_filtered_global", data.frame(), envir = .GlobalEnv)
  shiny::observe({
    shinyjs::toggleState(id = "prevBtn", condition = rv$page > 1)
    if (rv$page == 3) {
      shiny::updateActionButton(session, "nextBtn", label = "Execute")
    } else {
      shiny::updateActionButton(session, "nextBtn", label = "Next >")
    }
    shinyjs::hide(selector = ".page")
    shinyjs::show(paste0("step", rv$page))
  })
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  shiny::observeEvent(input$prevBtn, navPage(-1))
  shiny::observeEvent(input$nextBtn, {
    if (rv$page < 3) {
      assign("data_filtered_global", update_selected_table(data_filtered_global, FALSE), envir = .GlobalEnv)
      if (rv$page > 1) {
        if (has_missing_file_names()) {
          shinyalert::shinyalert(
            "Please download the selected data manually using the provided download links and put the files into the respective folders.",
            size = "l",
            animation = "slide-from-top",
            type = "error"
          )
          return()
        } else {
          assign("data_filtered_global", store_selected_file_names(data_filtered_global), envir = .GlobalEnv)
        }
      }
      navPage(1)
    } else {
      # Call process function from external source
      voteswitchR:::build_infrastructure(
        folder_location = input$dir,
        available_data = data_filtered_global,
        selected_concepts = input$concepts,
        selected_contexts = data_filtered_global$elec_id,
        map = input$map_par,
        impute = input$impute_par,
        n_imp = as.numeric(input$n_imp_par),
        seed = as.numeric(input$seed_par),
        rake = input$rake_par,
        aggregate = input$aggregate_par,
        format = input$format_par,
        return_data = input$return_data_par,
        return_data_imp = input$return_data_imp_par,
        return_agg_data = input$return_agg_data_par,
        return_agg_data_imp = input$return_agg_data_imp_par,
        return_info_imp = input$return_info_imp_par,
        output_file_path = if (input$output_file_path_par != "")
          input$output_file_path_par
        else
          NULL
      )
      shiny::stopApp()
    }
  })

  ##### Page 1: Concepts + Context List Output #####
  # select concepts from input data
  concepts <-
    unique(voteswitchR:::concepts_df[!(voteswitchR:::concepts_df$base_concept %in%
                                         c("drop", "region", "vote", "l_vote")), ]$description)
  output$variables_concepts <-
    renderUI({
      tags$div(align = 'left',
               shiny::checkboxGroupInput("concepts",
                                         label = "",
                                         choices = concepts))

    })

  shiny::observe({
    if (input$selectall == 0)
      return(NULL)
    else if (input$selectall %% 2 == 0)
    {
      shiny::updateCheckboxGroupInput(session,
                                      "concepts",
                                      "",
                                      choices = concepts)
    }
    else
    {
      shiny::updateCheckboxGroupInput(session,
                                      "concepts",
                                      "",
                                      choices = concepts,
                                      selected = concepts)
    }
  })

  # # Change year to unique value if there are more
  # # than one elections in one year for one country
  # voteswitchR:::available_data <- voteswitchR:::available_data %>%
  #   dplyr::group_by(country_name, year) %>%
  #   dplyr::mutate(n = dplyr::n()) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(year = ifelse(n > 1, stringr::str_replace(elec_id, paste0(iso2c, "-"), ""), as.character(year))) %>%
  #   dplyr::group_by(country_name, year) %>%
  #   dplyr::mutate(n = dplyr::n()) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(year = ifelse(n > 1, paste0(year, stringr::str_sub(iso2c, start=-3)), as.character(year))) %>%
  #   dplyr::select(-n)

  # Build country/year matrix
  data_country_year <-
    data.frame(matrix(ncol = nrow(unique(
      voteswitchR:::available_data["year"]
    )),
    nrow = nrow(unique(
      voteswitchR:::available_data["country_name"]
    ))))
  # set unique years as colnames
  colnames(data_country_year) <-
    as.vector(unique(as.character(sort(
      voteswitchR:::available_data[["year"]]
    ))))
  # Write unique country values to new column
  data_country_year["Country"] <-
    unique(voteswitchR:::available_data["country_name"])
  data_country_year <- data_country_year %>%
    dplyr::select("Country", everything())
  # select Country as first column
  tibble::column_to_rownames(data_country_year, var = "Country")

  # functon to set available (TRUE) combinations of country/year
  set_values_year <- function(row) {
    current_years <-
      list(as.character(voteswitchR:::available_data[voteswitchR:::available_data$country_name == row["Country"], ]$year))
    for (year in current_years) {
      row[year] <- paste(year, "_", row["Country"], sep = "")
    }
    return(row)
  }

  # iterate over rows of transposed matrix and call set_values_year function
  data_country_year <-
    as.data.frame(t(apply(data_country_year, 1, set_values_year)))
  options(DT.options = list(pageLength = 5))

  # include checkbox matrix into data table
  counter <<- 1

  data_country_year_datatable <- {
    data_country_year <- rbind("Select all", data_country_year)
    data_country_year <- data_country_year %>%
      tibble::add_column(" " = data_country_year$Country, .before = "Country")
    data_country_year[1, 2] <-
      glue::glue(paste('<input type="checkbox" id=select_all>'))
    data_country_year[,-2] <-
      apply(data_country_year[,-2], c(1, 2), function(x) {
        # fill cell with NA if no context/row/column selection
        if (is.na(x)) {
          return(NA)
          # fill cell with checkbox if column selection
        } else if (x == "Select all") {
          counter <<- counter + 1
          return(glue::glue(
            paste(
              '<input type="checkbox" class=select_column id=',
              counter,
              '>'
            )
          ))
        } else {
          return(glue::glue(
            paste(
              '<input type="checkbox" class=contexts id=',
              toString(gsub(" ",  "-", x, fixed = TRUE)),
              '>'
            )
          ))
        }
      })

    # start at 1000 to prevent duplicate IDs of HTML elements
    counter <<- 1000
    data_country_year[, 1] <-
      apply(data_country_year[" "], 1, function(x) {
        if (counter == 1000) {
          counter <<- counter + 1
          return(NA)
        }
        counter <<- counter + 1
        return(glue::glue(
          paste(
            '<input type="checkbox" class=select_row id=',
            counter,
            '>'
          )
        ))
      })

    # finally, configure data table
    DT::datatable(
      data_country_year,
      extensions = "FixedColumns",
      escape = F,
      rownames = F,
      class = 'cell-border compact',
      options = list(
        ordering = T,
        autowidth = F,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 2),
        scrollY = "400px",
        pageLength = nrow(data_country_year),
        columnDefs = list(
          list(
            className = 'dt-center',
            targets = "_all",
            orderable = FALSE
          )
        )
      ),
      selection = "none"
    )
  }

  # Render Data Table with preprocessed data
  output$countries_year = DT::renderDT(data_country_year_datatable)

  # add JS handlers to checkbox clicks
  shinyjs::runjs(
    "$('body').on('click', '#select_all',
                function() {
                  $('[class*=contexts]').attr('checked', event.target.checked)
                  $('[class=select_column]').attr('checked', event.target.checked)
                  $('[class=select_row]').attr('checked', event.target.checked)

                  let checkboxes = [...document.querySelectorAll('.contexts:checked')].map(item => item.id);
                  Shiny.setInputValue('checkboxes',checkboxes);
                }
    );"
  )
  shinyjs::runjs(
    "$('body').on('click', '.select_column',
                function(event) {
                  let firstCells = document.querySelectorAll('td:nth-child(' + event.target.id + ')');
                  firstCells.forEach(function(singleCell) {
                    if(singleCell.hasChildNodes() && !singleCell.childNodes[0].classList.contains('select_column')) {
                      $('#' + singleCell.childNodes[0].id).attr('checked', event.target.checked)
                    }
                  });

                  let checkboxes = [...document.querySelectorAll('.contexts:checked')].map(item => item.id);
                  Shiny.setInputValue('checkboxes',checkboxes);
                }
    );"
  )

  shinyjs::runjs(
    "$('body').on('click', '.select_row',
                function(event) {
                  let nextSibling = document.getElementById(event.target.id).parentElement.nextElementSibling;
                  while(nextSibling) {
                    if(nextSibling.hasChildNodes()) {
                      $('#' + nextSibling.childNodes[0].id).attr('checked', event.target.checked)
                    }
                    nextSibling = nextSibling.nextElementSibling;
                  }

                  let checkboxes = [...document.querySelectorAll('.contexts:checked')].map(item => item.id);
                  Shiny.setInputValue('checkboxes',checkboxes);
                }
    );"
  )

  shinyjs::runjs(
    "$('body').on('click', '.contexts',
            function() {
              let checkboxes = [...document.querySelectorAll('.contexts:checked')].map(item => item.id);
              Shiny.setInputValue('checkboxes',checkboxes);
            }
          );"
  )

  # Event to reset context selections
  shiny::observeEvent(input$resetall, {
    output$countries_year = DT::renderDT(data_country_year_datatable)
  })

  ##### Page 2: Choose Directory #####
  # Fill out directory
  shiny::observeEvent(input$dir, {
    shinyjs::toggleState("structure_creation", input$dir != "")
    shinyjs::toggleState("find_files", input$dir != "")
  })

  # Event for creation of initial folder structure (button)
  shiny::observeEvent(input$structure_creation, {
    input_dir <- input$dir

    checkbox_names = gsub("-",  " ", input$checkboxes, fixed = TRUE)
    data <- voteswitchR:::available_data %>%
      dplyr::filter(year %in% unlist(sapply(stringr::str_split(checkbox_names, "_"), `[`, 1))) %>%
      dplyr::filter(country_name %in% unlist(sapply(stringr::str_split(checkbox_names, "_"), `[`)))

    for (context in 1:nrow(data)) {
      dir.create(file.path(paste0(input_dir, "/"), paste0(data[context, "folder_name"], "/")), showWarnings = FALSE)
    }
  })

  shiny::observeEvent(input$find_files, {
    assign("data_filtered_global",update_selected_table(data_filtered_global, TRUE), envir = .GlobalEnv)
  })

  # Create Data Table with selected contexts
  update_selected_table <- function(data_filtered, forceUpdate = FALSE) {
    previous_data_filtered <- data_filtered
    data_filtered <- voteswitchR:::available_data

    # Map numbers for data access to labels
    data_filtered <- data_filtered %>%
      dplyr::mutate(data_access = dplyr::case_when(
        data_access == 1 ~ "Accept terms of use",
        data_access == 2 ~ "Register and accept terms of use",
        data_access == 3 ~ "Specific research proposal required",
        data_access == 4 ~ "Specific research proposal and signed user agreement required",
        data_access == 5 ~ "Signed user agreement and payment of provision fee required",
      ))

    checkbox_names <- gsub("-",  " ", input$checkboxes, fixed = TRUE)
    data_filtered <-
      data_filtered %>% dplyr::filter(year %in% unlist(sapply(
        stringr::str_split(checkbox_names, "_"), `[`, 1
      )))
    data_filtered <-
      data_filtered %>% dplyr::filter(country_name %in% unlist(sapply(
        stringr::str_split(checkbox_names, "_"), `[`
      )))
    data_filtered <- data_filtered %>%
      dplyr::mutate(file_name, "") %>%
      store_selected_file_names()

    if (!forceUpdate & (nrow(previous_data_filtered) > 0)) {
      if (isTRUE(dplyr::all_equal(dplyr::select(previous_data_filtered, elec_id), dplyr::select(data_filtered, elec_id)))) {
        return(previous_data_filtered)
      }
    }

    for (i in 1:nrow(data_filtered)) {
      file_path <- paste0(input$dir, "/", data_filtered$folder_name[i])
      files <- list.files(path = file_path,
                          pattern = "\\.(dta|sav|rdata|DTA|SAV|RDATA|Rdata|RData)$",
                          full.names = FALSE)

      if (length(files) > 1) {
        files <- c("", files)
      }

      random_id <- sample(1:10000, 1)
      data_filtered[i, "random_id"] <- random_id
      input_id <- paste0("sel", i, "_", random_id)
      data_filtered[i, "file_name_options"] <-
        shiny::selectInput(input_id, "", choices = files, width = "100px") %>%
        as.character

      data_filtered[i, "file_name_options"] <- dplyr::case_when(
        length(files) == 1 ~ gsub("<select ", "<select class='success'", data_filtered[i, "file_name_options"]),
        length(files) > 1 ~ gsub("<select ", "<select class='warning'", data_filtered[i, "file_name_options"]),
        length(files) == 0 ~ gsub("<select ", "<select class='error'", data_filtered[i, "file_name_options"])
      )

      shinyjs::runjs(
        paste0("
          $('body').on('change', '#", input_id, "',
                  function(event) {
                    let select_element = document.getElementById(event.target.id);
                    select_element.classList.remove('error', 'warning', 'success');

                    if (select_element.value == '') {
                      if (select_element.length > 0) {
                        select_element.classList.add('warning');
                      } else {
                        select_element.classList.add('error');
                      }
                    } else {
                      select_element.classList.add('success');
                    }
                  }
          );"))
    }

    output$data_selected <- DT::renderDataTable(
      data_filtered %>%
        dplyr::select(elec_id,
                      source,
                      version_dataset,
                      download_link,
                      data_access,
                      folder_name,
                      file_name_options),
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      server = FALSE,
      options = list(
        ordering = TRUE,
        autowidth = FALSE,
        scrollX = TRUE,
        pageLength = nrow(data_filtered),
        columnDefs = list(list(
          className = 'dt-center', targets = "_all"
        ))
      ),
      callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
            var $this = $(this.node());
            $this.attr('id', this.data()[0]);
            $this.addClass('shiny-input-container');
          });
          Shiny.unbindAll(table.table().node());
          Shiny.bindAll(table.table().node());")
    )

    return(data_filtered)
  }

  shiny::observeEvent(input$refresh_table, {
    assign("data_filtered_global", update_selected_table(data_filtered_global, TRUE), envir = .GlobalEnv)
  })

  has_missing_file_names <- function() {
    data_filtered <- get('data_filtered_global', envir = .GlobalEnv)
    if (nrow(data_filtered) > 0) {
      for (i in 1:nrow(data_filtered)) {
        file_name <- input[[paste0("sel", i, "_", data_filtered[i, "random_id"])]]
        if ((is.null(file_name)) || (file_name == "")) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  }

  store_selected_file_names <- function(data_filtered) {
    if (nrow(data_filtered) > 0) {
      for (i in 1:nrow(data_filtered)) {
        selected_value <- input[[paste0("sel", i, "_", data_filtered[i, "random_id"])]]
        data_filtered[i, "file_name"] <- ifelse(is.null(selected_value), "", selected_value)
      }
    }
    return(data_filtered)
  }

  # Event for updating the selected concepts table on change on UI
  shiny::observe({
    if (!input$map_par) {
      shiny::updateCheckboxInput(session, "impute_par", value = FALSE)
      shinyjs::disable("impute_par")
      shiny::updateCheckboxInput(session, "rake_par", value = FALSE)
      shinyjs::disable("rake_par")
      shiny::updateCheckboxInput(session, "aggregate_par", value = FALSE)
      shinyjs::disable("aggregate_par")
      updateRadioButtons(session, "format_par", selected = "wide")
      shinyjs::disable("format_par")
      shiny::updateCheckboxInput(session, "return_data_imp_par", value = FALSE)
      shinyjs::disable("return_data_imp_par")
      shiny::updateCheckboxInput(session, "return_agg_data_par", value = FALSE)
      shinyjs::disable("return_agg_data_par")
      shiny::updateCheckboxInput(session, "return_agg_data_imp_par", value = FALSE)
      shinyjs::disable("return_agg_data_imp_par")
      shiny::updateCheckboxInput(session, "return_info_imp_par", value = FALSE)
      shinyjs::disable("return_info_imp_par")
    } else {
      shinyjs::enable("impute_par")
      shinyjs::enable("rake_par")
      shinyjs::enable("aggregate_par")
      shinyjs::enable("format_par")
      shinyjs::enable("return_data_imp_par")
      shinyjs::enable("return_agg_data_par")
      shinyjs::enable("return_agg_data_imp_par")
      shinyjs::enable("return_info_imp_par")
    }

    if (!input$impute_par) {
      shiny::updateCheckboxInput(session, "return_data_imp_par", value = FALSE)
      shinyjs::disable("return_data_imp_par")
      shiny::updateCheckboxInput(session, "return_agg_data_imp_par", value = FALSE)
      shinyjs::disable("return_agg_data_imp_par")
      shiny::updateCheckboxInput(session, "return_info_imp_par", value = FALSE)
      shinyjs::disable("return_info_imp_par")
    } else {
      shinyjs::enable("return_data_imp_par")
      shinyjs::enable("return_agg_data_imp_par")
      shinyjs::enable("return_info_imp_par")
    }
  })
}

shiny::shinyApp(ui, server)
