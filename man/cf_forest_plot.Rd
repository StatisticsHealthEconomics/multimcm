% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cf_forest_plot.R
\name{cf_forest_plot}
\alias{cf_forest_plot}
\title{Cure fraction forest plot using Stan output}
\usage{
cf_forest_plot(folder = "data/")
}
\arguments{
\item{folder}{String of location directory}
}
\value{
ggplot object
}
\description{
Using all treatment Stan model.
}
\examples{
\dontrun{
 # independent model
 fp_sep <- cf_forest_plot("data/separate/")
 #ggsave(fp_sep, filename = "plots/forest_plot_sep_multimcm.png", dpi = 640, width = 12, height = 8)

 # hierarchical model
 fp_hier <- cf_forest_plot("data/")
 #ggsave(fp_hier, filename = "plots/forest_plot_multimcm.png", dpi = 640, width = 12, height = 8)
}
}
