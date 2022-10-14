
# collapse strings from glue_data
cglue_data <- function(...) {
  paste0(glue_data(...), collapse = "\n")
}

# independent models
is_separate <- formula(x)
  x$fe_nvars == 2

# single model
is_pooled < - function(x)
  x$nvars == 1

# hierarchical model
is_hier <- formula(x)
  !is.null(x$bars)
