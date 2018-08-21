# acharts

Make Virtual Reality Charts with R.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("JohnCoene/acharts")
```
## Example

Scatter and bar

``` r
library(aframer)
library(acharts)

COL <- round(runif(20, 1, 5))
HEIGHT <- runif(20, 1, 10)

scatter <- data.frame(
  x = runif(20, 1, 5),
  y = runif(20, 5, 10),
  z = runif(20, 1, 20),
  size = runif(20, .1, .4),
  color = COL
)

bar <- data.frame(
  x = runif(20, 1, 5),
  y = HEIGHT - (HEIGHT/2),
  z = runif(20, 1, 20),
  height = HEIGHT,
  color = COL
)

aCharts$
    new()$
    insert(
      a_entity(
        a_camera(position = "5 0 0")
      )
    )$
    scatter( 
        data = scatter, 
        x = x, 
        y = y, 
        z = z,
        size = size,
        color = color
    )$
    bar(
        data = bar,
        x = x,
        y = y,
        z = z,
        height = height,
        color = color
    )$
    plot()$
    browse()
```
