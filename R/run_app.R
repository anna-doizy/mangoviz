#' Run APPNAME application
#'
#' Warning: beware of launching the app in a web browser and not in the RStudio viewer
#'
#' @param lang interface langage : english (en) or french (fr)
#'
#' @export
run_app <- function(lang="fr") {
  if(!lang %in% c("en","fr")) {
    message(lang," is not a valid language (en, fr). Reverting to French!")
    lang <- "fr"
  }
  
  i8n_launch.browser <- function(url) {
    utils::browseURL(paste0(url, "?lang=", lang))
  }
  
  shiny::runApp(
    system.file("app", package = "mangoviz"), 
    launch.browser = i8n_launch.browser
    )
}
