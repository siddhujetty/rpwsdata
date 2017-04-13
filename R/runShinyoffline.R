#' runShiny function to start shiny app in offline mode.
#' 
#' @description A function to start shiny app for offline use
#' @name runShinyoffline-function
#' @export
#' @import shiny
runShinyoffline <- function() {
  appDir <- system.file("shinyappoffline", "Shinyoffline.R", package = "rpwsdata")
  if (appDir == "") {
    stop("Could not find Shiny directory. Try re-installing `rpwsdata`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}