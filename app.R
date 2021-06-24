
library (shiny)
library (magrittr)
source ("R/server.R")
source ("R/ui.R")

shinyApp(ui = ShinyAppUI, server = shinyAppServer)
