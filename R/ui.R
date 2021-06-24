#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
#' @import leaflet
#' @export
ShinyAppUI <- fluidPage(

   sidebarLayout(
     sidebarPanel(
       tabsetPanel(
         id = "side_tabs",
         type = "tabs",
         tabPanel("Origins",
                      checkboxInput ("allorigins", "Select All/None", value = TRUE),
                      checkboxGroupInput("origin", "Point of origin",
                                         originnames (),
                                         selected = originnames ())),
         tabPanel("Destinations",
                      checkboxInput ("alldestinations", "Select All/None", value = TRUE),
                      checkboxGroupInput("destination", "Point of destination",
                                         museumnames (),
                                         selected = museumnames ())),
         tabPanel("Objects",
                      checkboxInput ("allobjects", "Select All/None", value = TRUE),
                      checkboxGroupInput("object", "Type of object",
                                         objectnames (),
                                         selected = objectnames ())),
         tabPanel("Information", shiny::htmlOutput("app_info"))
       )
     ),
     mainPanel(
           mapdeck::mapdeckOutput ("map",
                                   height = "800px")
     )
   )
)
