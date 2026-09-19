# Use single legend for grid of plots

from
https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html

## Usage

``` r
grid_arrange_shared_legend(
  ...,
  ncol = length(list(...)),
  nrow = 1,
  position = c("bottom", "right", "top")
)
```

## Arguments

- ...:

  ggplot objects

- ncol:

  number of columns

- nrow:

  number of rows

- position:

  position of the legend
