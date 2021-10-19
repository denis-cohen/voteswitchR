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
