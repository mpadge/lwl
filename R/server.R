
get_data <- function () {

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

  index <- which (!is.na (dat$Lat_origin) & !is.na (dat$Long_origin))

  return (dat [index, ])
}

museumnames <- function () {
    dat <- get_data ()
    return (unique (dat$name))
}

originnames <- function () {
    dat <- get_data ()
    return (unique (dat$origin [which (!is.na (dat$origin))]))
}

#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session The shiny session

#' @export
# Define server logic required to draw a histogram
shinyAppServer = function(input, output, session) { # nolint

  mapdeck_token <- mapdeck::mapdeck_tokens()[[1]][[1]]

  if (is.null(mapdeck_token)) {
    message ("No mapdeck token found on system. ",
             "Trying environment variable MAPBOX")
    mapdeck::set_token(token = Sys.getenv("MAPBOX"))
  }

  repo_sha <- system("git rev-parse --short HEAD", intern = TRUE)
  output$app_info <- renderText(
    paste(
      "Warning: this is not a stable version. ",
      "Please do not distribute. Version",
      a(
        repo_sha,
        href = paste0("https://github.com/fjensz/LWL/tree/", repo_sha),
        target = "_blank"
      ),
      "released under the",
      a("GNU Affero GPL",
        href = "https://www.gnu.org/licenses/agpl-3.0.en.html",
        target = "_blank")
    )
  )


  dat <<- get_data ()

  ggplot2::theme_set(ggplot2::theme_minimal())

  output$map_destinations <- mapdeck::renderMapdeck({
    mapdeck::mapdeck(style = mapdeck::mapdeck_style ("light"))
  })

  observeEvent({
      input$destination
    }, {
      plot_map(dat, input$destination, update_view = FALSE)
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
        plot_map (dat, input$destination, update_view = FALSE)
    }
  )

  x <- reactive({
    g <- plot_chart(city = input$city)
    return(g)
  })
  output$plot <- renderPlot({
    print(x())
  })

}

plot_map <- function(dat, layers, update_view = FALSE) {

  cols <- grDevices::rgb (colourvalues::get_palette("inferno"),
                          maxColorValue = 255)

  dat_filt <- dat [dat$name %in% layers, ]

  if (nrow (dat_filt) == 0) {

      mapdeck::mapdeck_update(map_id = "map_destinations")

  } else {

      # palettes in colourvalues::color_palettes()
      mapdeck::mapdeck_update(map_id = "map_destinations") %>%
          mapdeck::add_arc (
                            data = dat_filt,
                            origin = c ("Long", "Lat"),
                            destination = c ("Long_origin", "Lat_origin"),
                            palette = "inferno",
                            stroke_from = "name",
                            stroke_to = "name",
                            update_view = update_view,
                            layer_id = "mylayer"
          )
  }
}

plot_chart <- function(city) {

    x <- y <- NULL

    x <- data.frame (x = 1:10,
                     y = stats::runif (10))

    ggplot2::ggplot(x, ggplot2::aes(x = x,
                                    y = y)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_smooth(method = "lm") +
        ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 90))
}
