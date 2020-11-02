# sample survival distributions with covariates ---------------------------

#'
rexp_rgn <- function(n, mu, X) {
  
  linpred <- mu[1] + mu[-1] %*% X
  rates <- exp(linpred)
  
  rexp(n, rate = rates)
}

#'
rweibull_rgn <- function(n, alpha, mu, X) {
  
  linpred <- mu[1] + mu[-1] %*% X
  rates <- exp(linpred)
  
  rweibull(n, shape = alpha, scale = rates)
}

#'
rbiweibull_rgn <- function(n, alpha, mu_exp, mu_w, X) {
  
  lp_exp <- mu_exp[1] + mu_exp[-1] %*% X
  lp_w <- mu_w[1] + mu_w[-1] %*% X
  
  rates_exp <- exp(lp_exp)
  rates_w <- exp(lp_w)
  
  t_exp <- rexp(n, rate = rates_exp)
  t_w <- rweibull(n, shape = alpha, scale = rates_w)
  
  pmin(t_exp, t_w)
}

