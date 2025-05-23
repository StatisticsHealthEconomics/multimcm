
#' Use single legend for grid of plots
#'
#' from https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
#'
#' @import ggplot2
#' @import grid
#' @import gridExtra
#'
grid_arrange_shared_legend <- function(...,
                                       ncol = length(list(...)),
                                       nrow = 1,
                                       position = c("bottom", "right", "top")) {

  plots <- list(...)
  position <- match.arg(position)

  g <-
    ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs

  legend <-
    g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]

  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)

  gl <-
    lapply(plots, function(x)
      x + theme(legend.position = "none"))

  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(
    position,
    "bottom" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight)
    ),
    "top" = arrangeGrob(
      legend,
      do.call(arrangeGrob, gl),
      ncol = 1,
      heights = unit.c(lheight, unit(1, "npc") - lheight)
    ),
    "right" = arrangeGrob(
      do.call(arrangeGrob, gl),
      legend,
      ncol = 2,
      widths = unit.c(unit(1, "npc") - lwidth, lwidth)
    )
  )

  grid.newpage()
  grid.draw(combined)

  invisible(combined)
}

