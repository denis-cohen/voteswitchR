#' @title build_data_file
#'
#' @description Runs a ShinyApp that guides users through the steps
#' required for data download, recoding, mapping, and imputation.
#'
#' @return Returns and/or stores and object called data_file which contains
#' a list of data for all requested electoral contexts.
#'
#' @export

build_data_file <- function() {
  appDir <-
    system.file("shiny", "build_data_file", package = "voteswitchR")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `voteswitchR`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
