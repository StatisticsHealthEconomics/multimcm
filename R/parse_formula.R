
#' parse formula
#' @importFrom lme4 nobars findbars
#' @importFrom rstan nlist
#'
parse_formula <- function(formula, data, family = NA) {

  # if (!inherits(formula, "formula")) {
  #   stop("'formula' must be a formula.")
  # }

  formula <- as.formula(formula)

  # all variables of entire formula
  allvars <- all.vars(formula)
  allvars_form <- reformulate(allvars)

  nvars <- length(allvars)

  # LHS of entire formula
  lhs       <- lhs(formula)         # LHS as expression
  lhs_form  <- reformulate_lhs(lhs) # LHS as formula

  # RHS of entire formula
  rhs       <- rhs(formula)         # RHS as expression
  rhs_form  <- reformulate_rhs(rhs) # RHS as formula

  # just fixed-effect part of formula
  fe_form   <- lme4::nobars(formula)

  # just random-effect part of formula
  bars      <- lme4::findbars(formula)[[1]]
  re_parts  <- split_at_bars(bars)

  mf <- model.frame(lme4::subbars(formula), data = data)

  # names of variable without event type
  fe_vars <- attr(terms(fe_form), "term.labels")
  fe_nvars <- length(fe_vars)

  n_groups <- length(unique(mf[, re_parts$group_var]))

  mf[, re_parts$group_var] <- as.factor(mf[, re_parts$group_var])

  fe_nlevels <- nlevels(mf[, fe_vars])

  c(nlist(
    formula,
    mf,
    family,
    allvars,
    allvars_form,
    nvars,
    lhs,
    lhs_form,
    rhs,
    rhs_form,
    fe_form,
    fe_vars,
    fe_nvars,
    fe_nlevels,
    n_groups,
    bars),
    re_parts)
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

# Split the random effects part of a model formula into
#   - the formula part (ie. the formula on the LHS of "|"), and
#   - the name of the grouping factor (ie. the variable on the RHS of "|")
#
# @param x Random effects part of a model formula, as returned by lme4::findbars
# @return A named list with the following elements:
#   re_form: a formula specifying the random effects structure
#   group_var: the name of the grouping factor
split_at_bars <- function(x) {

  terms <- strsplit(deparse(x, 500), "\\s\\|\\s")[[1L]]

  if (!length(terms) == 2L)
    return(NULL)

  re_form <- formula(paste("~", terms[[1L]]))
  group_var <- terms[[2L]]
  nlist(re_form, group_var)
}

