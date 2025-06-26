
#' Prepare background data
#'
#' Background hazard point values.
#'
#' @param input_data
#' @param bg_varname
#' @param formula_cure
#' @param event_type
#' @param suffix
#'
prep_bg_data <- function(input_data,
                         bg_varname,
                         formula_cure,
                         event_type,
                         suffix = TRUE) {
  dat <- dplyr::filter(
    input_data, !!sym(formula_cure$group_var) == event_type)

  # append unique id
  if (suffix && !identical(event_type, ""))
    nm <- paste("h_bg", event_type, sep = "_")

  setNames(list(dat[[bg_varname]]), nm)
}

