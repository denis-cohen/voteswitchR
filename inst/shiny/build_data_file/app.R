library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(shinyFiles)
library(tibble)
library(stringr)
library(shinyalert)

#### UI PART ####
NUM_PAGES <- 3
ui <- fluidPage(
  useShinyalert(),
  tags$head(tags$style(
    HTML(
      ".multicol .shiny-options-group{

                              -webkit-column-count: 4; /* Chrome, Safari, Opera */
                              -moz-column-count: 4;    /* Firefox */
                              column-count: 4;
                              -moz-column-fill: balanced;
                              -column-fill: balanced;
                              }
                              .checkbox{
                              margin-top: 0px !important;
                              -webkit-margin-after: 0px !important;
                              }
                              "
    )
  )),
  useShinyjs(),
  titlePanel("Vote Switching Data"),
  hidden(lapply(seq(NUM_PAGES), function(i) {
    if (i == 1) {
      div(
        class = "page",
        id = paste0("step", i),
        h4('Concepts:'),
        actionLink("selectall", "Select All"),
        fluidRow(column(
          width = 4, uiOutput("variables_concepts")
        )),
        h4('Contexts:'),
        actionLink("resetall", "Reset Context Selection"),
        DTOutput('countries_year')
      )
    } else if (i == 2) {
      div(
        class = "page",
        id = paste0("step", i),
        h4('Preparation:'),
        h5(
          "In case you haven't already done, please download the selected data manually using the provided download links and put the files into the respective folders."
        ),
        textInput("dir", "Input directory", placeholder = "e.g. ./data"),
        actionButton("structure_creation", "Create initial folder structure"),
        h4('Selected Contexts:'),
        h5("Please Note: File format must be either .sav, .dta or .rdata."),
        actionLink("refresh_table", "Generate/Update Context Table"),
        DTOutput('data_selected')
      )
    } else if (i == 3) {
      div(
        class = "page",
        id = paste0("step", i),
        h4('Execution Specification:'),
        checkboxInput("map_par", "map", value = TRUE),
        textInput("map_vars_par", "map_vars", placeholder = "e.g. var1,var2,var3"),
        checkboxInput("impute_par", "impute", value = TRUE),
        checkboxInput("include_info_imp_par", "include_info_imp"),
        textInput(
          "n_imp_par",
          "n_imp",
          value = "5",
          placeholder = "e.g. 5"
        ),
        textInput(
          "seed_par",
          "seed",
          value = "20210910",
          placeholder = "e.g. 20210910"
        ),
        radioButtons(
          "format_par",
          selected = "long",
          "format",
          c("long" = "long",
            "wide" = "wide")
        ),
        textInput(
          "existing_data_file_par",
          "existing_data_file",
          placeholder = "e.g. path/to/data/existing_data_file.csv"
        ),
        textInput(
          "output_file_path_par",
          "output_file_path (If empty, the output file is stored in the R Environment)",
          placeholder = "e.g. path/to/data/data_file.RData"
        )
      )
    }
  })),
  br(),
  actionButton("prevBtn", "< Previous"),
  actionButton("nextBtn", "Next >")
)

#### BACKEND PART ####
server <- function(input, output, session) {
  # Load & update the packages we need
  # set options
  options(stringsAsFactors = F)
  p_needed <- c("dplyr")
  packages <- rownames(installed.packages())
  p_to_install <- p_needed[!(p_needed %in% packages)]
  if (length(p_to_install) > 0) {
    install.packages(p_to_install)
  }
  lapply(p_needed, require, character.only = TRUE)
  
  # Set up pages (+ navigation)
  rv <- reactiveValues(page = 1)
  data_filtered <<- NULL
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    if (rv$page == 3) {
      updateActionButton(session, "nextBtn", label = "Execute")
    } else {
      updateActionButton(session, "nextBtn", label = "Next >")
    }
    hide(selector = ".page")
    show(paste0("step", rv$page))
  })
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, {
    if (rv$page < 3) {
      update_selected_table()
      if (rv$page > 1 && has_missing_file_names()) {
        shinyalert(
          "Please download the selected data manually using the provided download links and put the files into the respective folders.",
          size = "l",
          animation = "slide-from-top",
          type = "error"
        )
        return()
      }
      navPage(1)
    } else {
      # Call process function from external source
      source("build_infrastructure.R", echo = TRUE)
      build_infrastructure(
        folder_location = input$dir,
        available_data = data_filtered,
        recodes_path = NULL,
        mappings_path = NULL,
        selected_concepts = input$concepts, 
        selected_contexts = data_filtered$elec_id,
        map = input$map_par,
        map_vars = unlist(strsplit(input$map_vars_par, split = ",")),
        impute = input$impute_par,
        include_info_imp = input$include_info_imp_par,
        n_imp = as.numeric(input$n_imp_par),
        seed = as.numeric(input$seed_par),
        format = input$format_par,
        existing_data_file = if (input$existing_data_file_par != "")
          input$existing_data_file_par,
        output_file_path = if (input$output_file_path_par != "")
          input$output_file_path_par
        else
          NULL
      )
      stopApp()
    }
  })
  
  # read configuration data
  data <-
    read.csv("data/available_data.csv",
             header = TRUE,
             sep = ",")
  
  # select concepts from input data
  concepts_df <- read.csv("data/concepts.csv",
                          header = TRUE)
  concepts <-
    unique(concepts_df[!(concepts_df$base_concept %in% 
                           c("drop", "region", "vote", "l_vote")), ]$description)
  
  ##### Page 1: Concepts + Context List Output #####
  output$variables_concepts <-
    renderUI({
      tags$div(align = 'left',
               checkboxGroupInput("concepts",
                                  label = "",
                                  choices = concepts
                                  ))

    })
  
  observe({
    if (input$selectall == 0)
      return(NULL)
    else if (input$selectall %% 2 == 0)
    {
      updateCheckboxGroupInput(session,
                               "concepts",
                               "",
                               choices = concepts)
    }
    else
    {
      updateCheckboxGroupInput(session,
                               "concepts",
                               "",
                               choices = concepts,
                               selected = concepts)
    }
  })
  
  # Build country/year matrix
  data_country_year <-
    data.frame(matrix(ncol = nrow(unique(data["year"])),
                      nrow = nrow(unique(data["country_name"]))))
  # set unique years as colnames
  colnames(data_country_year) <-
    as.vector(unique(as.character(sort(data[["year"]]))))
  # Write unique country values to new column
  data_country_year["Country"] <- unique(data["country_name"])
  data_country_year <- data_country_year %>%
    select("Country", everything())
  # select Country as first column
  column_to_rownames(data_country_year, var = "Country")
  
  # functon to set available (TRUE) combinations of country/year
  set_values_year <- function(row) {
    current_years <-
      list(as.character(data[data$country_name == row["Country"], "year"]))
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
    data_country_year <-
      data_country_year %>% add_column(' ' = data_country_year$Country, .before =
                                         "Country")
    data_country_year[1, 2] <-
      glue::glue(paste('<input type="checkbox" id=select_all>'))
    data_country_year[, -2] <-
      apply(data_country_year[, -2], c(1, 2), function(x) {
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
    datatable(
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
  
  output$countries_year = renderDT(data_country_year_datatable)
  
  # add handlers to checkbox clicks
  runjs(
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
  runjs(
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
  
  runjs(
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
  
  runjs(
    "$('body').on('click', '.contexts',
            function() {
              let checkboxes = [...document.querySelectorAll('.contexts:checked')].map(item => item.id);
              Shiny.setInputValue('checkboxes',checkboxes);
            }
          );"
  )
  
  observeEvent(input$resetall, {
    output$countries_year = renderDT(data_country_year_datatable)
  })
  
  
  ##### Page 2: Choose Directory #####
  # popup to choose directory
  observeEvent(input$dir, {
    toggleState("structure_creation", input$dir != "")
  })
  
  observeEvent(input$structure_creation, {
    input_dir <- input$dir
    
    checkbox_names = gsub("-",  " ", input$checkboxes, fixed = TRUE)
    data <-
      data %>% filter(year %in% unlist(sapply(
        str_split(checkbox_names, "_"), `[`, 1
      )))
    data <-
      data %>% filter(country_name %in% unlist(sapply(str_split(
        checkbox_names, "_"
      ), `[`)))
    
    for (context in 1:nrow(data)) {
      dir.create(file.path(paste0(input_dir, "/"), paste0(data[context, "folder_name"], "/")), showWarnings = FALSE)
    }
  })
  
  # create data table with selected concepts
  several_files_found_msg <- "Multiple files found! Please check."
  
  update_selected_table <- function() {
    data_filtered <<- data
    selected_context_table <<- {
      checkbox_names = gsub("-",  " ", input$checkboxes, fixed = TRUE)
      data_filtered$file_name <<-
        apply(data_filtered, 1, function(row) {
          file_path <- paste0(input$dir, "/", row["folder_name"])
          files <-
            list.files(path = file_path,
                       pattern = "\\.(dta|sav|rdata)$",
                       full.names = FALSE)
          if (length(files) == 1) {
            return(files[1])
          } else if (length(files) > 1) {
            return(several_files_found_msg)
          } else {
            return("")
          }
        })
      
      data_filtered$data_access <<-
        ifelse(data_filtered$data_access == 1,
               "non-restrictive",
               data_filtered$data_access)
      data_filtered$data_access <<-
        ifelse(data_filtered$data_access == 2,
               "less restrictive",
               data_filtered$data_access)
      data_filtered$data_access <<-
        ifelse(data_filtered$data_access == 3,
               "very restrictive",
               data_filtered$data_access)
      
      data_filtered <<-
        data_filtered %>% filter(year %in% unlist(sapply(
          str_split(checkbox_names, "_"), `[`, 1
        )))
      data_filtered <<-
        data_filtered %>% filter(country_name %in% unlist(sapply(
          str_split(checkbox_names, "_"), `[`
        )))
      
      datatable(
        select(
          data_filtered,
          "elec_id",
          "source",
          "version_dataset",
          "download_link",
          "data_access",
          "file_name"
        ),
        escape = F,
        rownames = F,
        class = 'cell-border compact',
        options = list(
          ordering = T,
          autowidth = F,
          scrollX = TRUE,
          pageLength = nrow(data_country_year),
          columnDefs = list(list(
            className = 'dt-center', targets = "_all"
          ))
        ),
        selection = "none"
      )
    }
    
    output$data_selected <- renderDT(
      selected_context_table %>% formatStyle(
        "file_name",
        target = 'cell',
        backgroundColor = styleEqual(c("", several_files_found_msg), c('red', 'yellow'))
      ))
  }
  
  observeEvent(input$refresh_table, {
    update_selected_table()
  })
  
  observe({
    update_selected_table()
  })
  
  has_missing_file_names <- function() {
    if (!is.null(data_filtered)) {
      return(nrow(
        data_filtered %>% filter(file_name == "" ||
                                   file_name == several_files_found_msg)
      ) > 0)
    }
    return(TRUE)
  }
}

shinyApp(ui, server)