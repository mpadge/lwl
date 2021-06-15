#' launches the LWL app
#'
# wrapper for shiny::shinyApp()
#' @return shiny application object
#'
#' @import shiny
#' @export
runLWL = function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
