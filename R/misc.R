
# collapse strings from glue_data
cglue_data <- function(...) {
  paste0(glue_data(...), collapse = "\n")
}

# single model
is_pooled <- function(x)
  x$nvars == 1

# independent models
is_separate <- function(x)
  x$fe_nvars == 2

# hierarchical model
is_hier <- function(x)
  !is.null(x$bars)

# pooled cure fractions
is_pooled_cf <- function(x)
  x$cf_idx == 1

# independent cure fractions
is_separate_cf <- function(x)
  x$cf_idx == 2

# hierarchical cure fraction
is_hier_cf <- function(x)
  x$cf_idx == 3
