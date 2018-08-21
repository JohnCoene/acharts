#' acharts R6 Class
#'
#' Create virtual reality charts.
#'
#' @section Methods:
#' \itemize{
#'   \item{\code{scatter} scatter plot.}
#'   \item{\code{bar} bar chart.}
#'   \item{\code{plot} Plot chart}
#'   \item{\code{get} Get chart}
#'   \item{\code{browse} Browse chart}
#'   \item{\code{embed} Embed chart}
#'   \item{\code{insert} Insert \code{aframer} components in the \code{a_scene}.}
#' }
#'
#' @examples
#' aCharts$
#'   new()$
#'   scatter(
#'     data = mtcars,
#'     x = mpg,
#'     y = drat,
#'     z = wt,
#'     color = qsec
#'   )$
#'   plot()$
#'   browse()
#'
#' @export
aCharts <- R6::R6Class(
  "aScatter",
  public = list(
    initialize = function(colors = NULL){
      if(!is.null(colors))
        private$colors <- colors
    },
    scatter = function(data, x, y, z, size = NULL, color = NULL){

      if(missing(data) || missing(x) || missing(y) || missing(z))
        stop("must pass data, x, y, or z", call. = FALSE)

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

      if("color" %in% names(data))
        data$color <- colorRampPalette(private$colors)(nrow(data))

      sc <- private$graph
      # plot
      for(i in 1:nrow(data)){
        point <- aframer::a_sphere(
          position = glue::glue("{data$x[i]} {data$y[i]} {data$z[i]}")
        )

        if(length(data$size))
          point <- htmltools::tagAppendAttributes(point, radius = data$size[i])
        else
          point <- htmltools::tagAppendAttributes(point, radius = .5)

        if(length(data$color))
          point <- htmltools::tagAppendAttributes(point, color = data$color[i])

        sc <- htmltools::tagAppendChild(sc, point)
      }

      private$graph <- sc
      invisible(self)
    },
    bar = function(data, x, y, z, height, color = NULL, width = 1){

      if(missing(data) || missing(x) || missing(y) || missing(z) || missing(height))
        stop("must pass data, x, y, z, or height", call. = FALSE)

      x <- rlang::enquo(x)
      y <- rlang::enquo(y)
      z <- rlang::enquo(z)
      color <- rlang::enquo(color)
      height <- rlang::enquo(height)

      cols <- list(
        x = x,
        y = y,
        z = z,
        height = height
      )

      if(!rlang::quo_is_null(color))
        cols <- append(cols, list(color = color))

      data <- dplyr::select(data, !!!cols)

      if("color" %in% names(data))
        data$color <- colorRampPalette(private$colors)(nrow(data))

      sc <- private$graph
      # plot
      for(i in 1:nrow(data)){
        bar <- aframer::a_box(
          height = data$height[i],
          wdith = 1,
          position = glue::glue("{data$x[i]} {data$y[i]} {data$z[i]}")
        )

        if(length(data$color))
          bar <- htmltools::tagAppendAttributes(bar, color = data$color[i])

        sc <- htmltools::tagAppendChild(sc, bar)
      }

      private$graph <- sc
      invisible(self)

    },
    plot = function(...){
      private$scene <- aframer::a_scene(..., private$to_insert, private$graph)
      invisible(self)
    },
    get = function(){
      private$scene
    },
    browse = function(){
      aframer::browse_aframe(private$scene)
    },
    embed = function(width = "100%", height = "400px"){
      aframer::embed_aframe(private$scene, width, height)
    },
    insert = function(...){
      private$to_insert <- list(...)
      invisible(self)
    }
  ),
  private = list(
    to_insert = htmltools::tagList(),
    scene = NULL,
    graph = htmltools::tagList(),
    db = NULL,
    width = "100%",
    height = "400px",
    colors = c("#FF005C", "#02C6FF", "#008976", "#dddc37")
  )
)
