#' Inititialise
#'
#' @export
aCharts <- R6::R6Class(
  "aScatter",
  public = list(
    initialize = function(width = "100%", height = "400px", colors = NULL){
      private$width <- width
      private$height <- height

      if(!is.null(colors))
        private$colors <- colors
    },
    scatter = function(data, x, y, z, size = NULL, color = NULL){

      if(missing(data) || missing(x) || missing(y) || missing(z))
        stop("must pass data, x, y, z", call. = FALSE)

      x <- rlang::enquo(x)
      y <- rlang::enquo(y)
      z <- rlang::enquo(z)
      size <- rlang::enquo(size)
      color <- rlang::enquo(color)

      cols <- list(
        x = x,
        y = y,
        z = z
      )

      if(!rlang::quo_is_null(size))
        cols <- append(cols, list(size = size))

      if(!rlang::quo_is_null(color))
        cols <- append(cols, list(color = color))

      data <- dplyr::select(data, !!!cols)

      sc <- private$graph
      # plot
      for(i in 1:nrow(data)){
        point <- aframer::a_sphere(
          position = glue::glue("{data$x[i]} {data$y[i]} {data$z[i]}")
        )

        if(length(data$size))
          point <- htmltools::tagAppendAttributes(point, radius = data$size[i])

        if(length(data$color))
          point <- htmltools::tagAppendAttributes(point, color = data$color[i])

        sc <- htmltools::tagAppendChild(sc, point)
      }

      private$graph <- sc
      invisible(self)
    },
    plot = function(...){
      aframer::a_scene(..., private$graph)
    },
    get = function(){
      private$graph
    }
  ),
  private = list(
    graph = htmltools::tagList(),
    db = NULL,
    width = "100%",
    height = "400px",
    colors = c("#FF005C", "#02C6FF", "#008976", "#dddc37")
  )
)
