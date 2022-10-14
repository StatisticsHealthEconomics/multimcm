
# collapse strings from glue_data
cglue_data <- function(...) {
  paste0(glue_data(...), collapse = "\n")
}
