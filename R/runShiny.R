#' runShiny function to start shiny app in online mode.
#' 
#' @description A function to start shiny app
#' @name runShiny-function
#' @export
#' @import shiny
runShiny <- function() {
  appDir <- system.file("shinyapp", "Shiny.R", package = "rpwsdata")
  if (appDir == "") {
    stop("Could not find Shiny directory. Try re-installing `rpwsdata`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}