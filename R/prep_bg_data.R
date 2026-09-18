
#' Prepare background data
#'
#' Background hazard point values.
#'
#' @param input_data Data frame
#' @param bg_varname String for background variable name
#' @param formula_cure Parsed formula list
#' @param event_type String specifying event type
#' @param suffix Logical indicating whether to append suffix
#' @return A list with background data
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

