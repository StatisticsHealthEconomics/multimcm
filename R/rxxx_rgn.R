# sample survival distributions with covariates ---------------------------

#' rexp_rgn
#'
#' @param n
#' @param mu
#' @param X
#'
rexp_rgn <- function(n, mu, X) {

  linpred <- mu[1] + as.matrix(X) %*% mu[-1]
  rates <- exp(linpred)

  rexp(n, rate = rates)
}


#' rweibull_rgn
#'
#' @param n
#' @param alpha
#' @param mu
#' @param X
#'
rweibull_rgn <- function(n, alpha, mu, X) {

  linpred <- mu[1] + as.matrix(X) %*% mu[-1]
  rates <- exp(linpred)

  rweibull(n, shape = alpha, scale = rates)
}


#' rbiweibull_rgn
#'
#' @param n
#' @param alpha
#' @param mu_exp
#' @param mu_w
#' @param X
#'
rbiweibull_rgn <- function(n, alpha, mu_exp, mu_w, X) {

  lp_exp <- mu_exp[1] + as.matrix(X) %*% mu_exp[-1]
  lp_w <- mu_w[1] + as.matrix(X) %*% mu_w[-1]

  rates_exp <- exp(lp_exp)
  rates_w <- exp(lp_w)

  t_exp <- rexp(n, rate = rates_exp)
  t_w <- rweibull(n, shape = alpha, scale = rates_w)

  pmin(t_exp, t_w)
}

