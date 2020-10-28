


#' Generate time to event sample with censoring
#'
#' @examples
#'
#' rsurv(n = 100,
#'       distn = "exp",
#'       prop_cens = 0.1,
#'       params = list(mu = c(-5, 0.005)),
#'       X = rep(c(10, 25, 50, 100), each = 25))
#'
rsurv <- function(n = 100,
                  distn = "exp",
                  prop_cens = 0.1,
                  params = list(mu = c(-5, 0.005)),
                  X = rep(c(10, 25, 50, 100), each = 25)) {

  times <-
    if (distn == "exp") {
      rexp_rgn(n, params$mu, X)
    } else if (distn == "weibull") {
      rweibull_rgn(n, params$alpha, params$mu, X)      #for mu: it is params$mu? 
    } else if (distn == "biweibull") {
      do.call(rbiweibull_rgn, c(n = n, X = X, params))
    }

  # uniform sample then censor selected
  cens_idx <- sample(1:n,
                     size = n*prop_cens,
                     replace = FALSE)

  t_cens <- times
  t_cens[cens_idx] <-
    map_dbl(t_cens[cens_idx], function(x) runif(1, 0, x))

  status <- as.numeric(!(1:n) %in% cens_idx)

  list(times = times,
       t_cens = t_cens,
       status = status,
       X = X)
}

# rsurv_mix(cf = 0.2,
#           n = 200,
#           distn = c("exp", "exp"),
#           prop_cens = 0.1,
#           params =
#             list(
#               list(
#                 mu = c(2.5, 0.005)),
#               list(
#                 mu = c(-8, 0.005))),
#           X = rep(c(10, 25, 50, 100), each = 50))

#' @importFrom purrr transpose
#'
rsurv_mix <- function(cf = 0.2,
                      n = 200,
                      distn = c("exp", "exp"),
                      prop_cens = 0.1,
                      params =
                        list(
                          list(
                            mu = c(2.5, 0.005)),
                          list(
                            mu = c(-8, 0.005))),
                      X = rep(c(10, 25, 50, 100), each = 50)) {

  n_distns <- length(distn)

  if (length(params) != n_distns)
    stop("Number of parameter sets and distributions don't match",
         call. = FALSE)

  z <- rbinom(n, 1, cf) + 1
  s <- c(sum(z == 1), sum(z == 2))
  m <- list(X[z == 1], X[z == 2])

  prop_cens <-
    if (length(prop_cens) < n_distns) {
      rep(prop_cens[1], n_distns)
    } else {
      prop_cens}


  res <- list()

  for (i in seq_along(distn)) {

    res[[i]] <-
      rsurv(n = s[i],
            X = m[[i]],
            params[[i]],
            distn = distn[i],
            prop_cens = prop_cens[i])
  }

  out <- purrr::transpose(res)

  list(
    times = unlist(out$times),
    t_cens = unlist(out$t_cens),
    status = unlist(out$status)
  )
}

