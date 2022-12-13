
#' Prep background data
#'
#' background hazard point values
#'
prep_bg_data <- function(input_data,
                         bg_varname,
                         formula_cure,
                         event_type,
                         suffix = TRUE) {
  dat <- filter(
    input_data, !!sym(formula_cure$group_var) == event_type)

  # append unique id
  if (suffix && !identical(event_type, ""))
    nm <- paste("h_bg", event_type, sep = "_")

  setNames(list(dat[[bg_varname]]), nm)
}

