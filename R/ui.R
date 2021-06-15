#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
#' @import leaflet
#' @export
shinyAppUI <- navbarPage("LWL Destinations", # nolint
    column(12, shiny::htmlOutput("app_info")),
    column(width = 3,
           checkboxInput ("alldestinations", "Select All/None", value = TRUE),
           checkboxGroupInput("destination", "Point of destination",
                              museumnames (),
                              selected = museumnames ())
    ),
    column(width = 9,
           mapdeck::mapdeckOutput ("map_destinations",
                                   height = "800px")
    ),
    p()
)
