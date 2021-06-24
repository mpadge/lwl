
get_data <- function (sort_col = "name") {

  data_file <- list.files ("data",
                           pattern = "*.tsv$",
                           full.names = TRUE)
  dat <- suppressWarnings (
            readr::read_delim (data_file, delim = "\t",
                               col_types = list (readr::col_character (),
                                                 readr::col_character (),
                                                 readr::col_double (),
                                                 readr::col_double (),
                                                 readr::col_character (),
                                                 readr::col_character (),
                                                 readr::col_character (),
                                                 readr::col_double (),
                                                 readr::col_double ()))
            )
  dat$name <- paste0 (dat$Museumsname, " ", dat$Standort)
  dat$name <- gsub ("\\s+", " ", gsub ("\\\"", " ", dat$name))
  nms <- unique (dat$name)
  index <- which (duplicated (tolower (nms)))
  for (i in index) {
      index_i <- which (tolower (nms) == tolower (nms) [i])
      index_j <- which (dat$name %in% nms [index_i])
      dat$name [index_j] <- nms [index_i [1]]
  }

  dat <- dat [which (!is.na (dat$Lat_origin) & !is.na (dat$Long_origin)), ]

  categories <- c ("domestic", "smoking", "furniture", "decorative", "tea", "weaponry",
                   "music", "printed", "clothing", "money", "jewellery", "playtoys")
  if (sort_col == "object")
      dat <- dat [which (dat$object %in% categories), ]

  names (dat) [grep ("^Land", names (dat))] <- "origin"
  dat <- dat [order (dat [[sort_col]]), ]

  return (dat)
}

museumnames <- function (sort_col = "name") {
    dat <- get_data (sort_col)
    return (unique (dat$name))
}

originnames <- function (sort_col = "origin") {
    dat <- get_data (sort_col)
    return (unique (dat$origin [which (!is.na (dat$origin))]))
}

objectnames <- function (sort_col = "object") {
    dat <- get_data (sort_col)
    return (unique (dat$object [which (!is.na (dat$object))]))
}

#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session The shiny session

#' @export
shinyAppServer = function(input, output, session) { # nolint

    #mapdeck_token <- mapdeck::mapdeck_tokens()[[1]][[1]]
    mapdeck_token <- "pk.eyJ1Ijoic21leHVzIiwiYSI6ImNrYjd1ZjZ0ZzA5ZDcyd3FoanluaXVibXgifQ.lFbKbqMwDt4oImHg2a-DJQ"
    Sys.setenv ("MAPBOX_TOKEN" = mapdeck_token)

    if (is.null(mapdeck_token)) {
        message ("No mapdeck token found on system. ",
                 "Trying environment variable MAPBOX")
        mapdeck::set_token(token = Sys.getenv("MAPBOX"))
    }

    #repo_sha <- system("git rev-parse --short HEAD", intern = TRUE)
    repo_sha <- "001"
    output$app_info <- renderText(
        paste(
              "Warning: this is not a stable version. ",
              "Please do not distribute. Version",
              a(
                repo_sha,
                href = paste0("https://github.com/fjensz/LWL-origins/tree/", repo_sha),
                target = "_blank"
                ),
              "released under the",
              a("GNU Affero GPL",
                href = "https://www.gnu.org/licenses/agpl-3.0.en.html",
                target = "_blank")
        )
    )
    dat <<- get_data ()

    output$map <- mapdeck::renderMapdeck({
        mapdeck::mapdeck(style = mapdeck::mapdeck_style ("light"))
    })

    p <- reactive ({
        switch (input$side_tabs,
                "Origins" = plot_map (dat, "origin", input$origins, update_view = FALSE),
                "Destinations" = plot_map (dat, "name", input$names, update_view = FALSE),
                "Objects" = plot_map (dat, "object", input$objects, update_view = FALSE))
    })

    observeEvent(input$side_tabs,
                 {
                     p()
    })

    observeEvent({
        input$destination
    }, {
        plot_map(dat, "name", input$destination, update_view = FALSE)
    }
    )
    observeEvent({
        input$alldestinations
    }, {
        updateCheckboxGroupInput(
            session = getDefaultReactiveDomain (),
            inputId = "destination", # nolint
            choices = museumnames (),
            selected = if (input$alldestinations) museumnames () else NULL
        )
        plot_map (dat, "name", input$destination, update_view = FALSE)
    })

    observeEvent({
        input$origin
    }, {
        plot_map(dat, "origin", input$origin, update_view = FALSE)
    }
    )
    observeEvent({
        input$allorigins
    }, {
        updateCheckboxGroupInput(
            session = getDefaultReactiveDomain (),
            inputId = "origin", # nolint
            choices = originnames (),
            selected = if (input$allorigins) originnames () else NULL
        )
        plot_map (dat, "origin", input$origin, update_view = FALSE)
    })

    observeEvent({
        input$object
    }, {
        plot_map(dat, "object", input$object, update_view = FALSE)
    }
    )
    observeEvent({
        input$allobjects
    }, {
        updateCheckboxGroupInput(
            session = getDefaultReactiveDomain (),
            inputId = "object", # nolint
            choices = objectnames (),
            selected = if (input$allobjects) objectnames () else NULL
        )
        plot_map (dat, "object", input$object, update_view = FALSE)
    })

}

plot_map <- function(dat, layer_name, layers, update_view = FALSE) {

  cols <- grDevices::rgb (colourvalues::get_palette("inferno"),
                          maxColorValue = 255)

  map_id <- "map"
  dat_filt <- dat [dat [[layer_name]] %in% layers, ]
  dat_filt <- dat_filt [which (!is.na (dat_filt [[layer_name]])), ]

  if (nrow (dat_filt) == 0) {

      mapdeck::mapdeck_update(map_id = map_id)

  } else {

      # palettes in colourvalues::color_palettes()
      mapdeck::mapdeck_update(map_id = map_id) %>%
          mapdeck::add_arc (
                            data = dat_filt,
                            origin = c ("Long", "Lat"),
                            destination = c ("Long_origin", "Lat_origin"),
                            palette = "inferno",
                            stroke_from = layer_name,
                            stroke_to = layer_name,
                            update_view = update_view,
                            layer_id = "mylayer"
          )
  }
}
