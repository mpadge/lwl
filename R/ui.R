#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
#' @import leaflet
#' @export
ShinyAppUI <- fluidPage(

  title = "LWL Museums",

   sidebarLayout(
     sidebarPanel(
                  strong ("LWL Museum Collections"),
                  helpText ("Visualisation of data collected in the 1980s from ",
                            "34 museums in North-Rhine Westfalia, Germany. ",
                            "Lines connect origins of extra-European items now ",
                            "held in these museums."),
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
