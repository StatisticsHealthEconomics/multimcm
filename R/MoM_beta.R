
#' MoM_beta
#'
#' Method of moments for the Beta distribution.
#'
#' @param xbar Mean
#' @param vbar Variance
#'
#' @return Beta distribution parameters a, b in a list.
#' @export
#'
#' @examples
#'
MoM_beta <- function(xbar,
                     vbar) {
  if (vbar == 0) {
    stop("zero variance not allowed")
  } else if (xbar * (1 - xbar) < vbar) {
    stop("mean or var inappropriate")
  } else{
    a <- xbar * (((xbar * (1 - xbar)) / vbar) - 1)
    b <- (1 - xbar) * (((xbar * (1 - xbar)) / vbar) - 1)
  }
  list(a = a, b = b)
}
