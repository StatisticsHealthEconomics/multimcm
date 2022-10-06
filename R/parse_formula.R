
#' parse formula
#' @importFrom lme4 nobars findbars
#' @importFrom rstan nlist
#'
parse_formula <- function(formula, data) {

  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula.")
  }

  formula <- as.formula(formula)

  # all variables of entire formula
  allvars <- all.vars(formula)
  allvars_form <- reformulate(allvars)

  # LHS of entire formula
  lhs       <- lhs(formula)         # LHS as expression
  lhs_form  <- reformulate_lhs(lhs) # LHS as formula

  # RHS of entire formula
  rhs       <- rhs(formula)         # RHS as expression
  rhs_form  <- reformulate_rhs(rhs) # RHS as formula

  # just fixed-effect part of formula
  fe_form   <- lme4::nobars(tf_form)

  # just random-effect part of formula
  bars      <- lme4::findbars(tf_form)
  re_parts  <- lapply(bars, split_at_bars)
  re_forms  <- pluck(re_parts, "re_form")  # ?

  nlist(formula,
        data,
        allvars,
        allvars_form,
        lhs,
        lhs_form,
        rhs,
        rhs_form,
        fe_form,
        bars,
        re_parts,
        re_forms)
}


###################
# from rstanarm

# Extract LHS of a formula
#
# @param x A formula object.
# @return An expression.
lhs <- function(x, as_formula = FALSE) {
  len <- length(x)
  if (len == 3L) {
    out <- x[[2L]]
  } else {
    out <- NULL
  }
  out
}

# Extract RHS of a formula
#
# @param x A formula object.
# @return An expression.
rhs <- function(x, as_formula = FALSE) {
  len <- length(x)
  if (len == 3L) {
    out <- x[[3L]]
  } else {
    out <- x[[2L]]
  }
  out
}

# Reformulate as LHS of a formula
#
# @param x A character string or expression.
# @return A formula.
reformulate_lhs <- function(x) {
  formula(substitute(LHS ~ 1, list(LHS = x)))
}

# Reformulate as RHS of a formula
#
# @param x A character string or expression.
# @return A formula.
reformulate_rhs <- function(x) {
  formula(substitute(~ RHS, list(RHS = x)))
}

