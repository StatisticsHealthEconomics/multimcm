
#' partial match on start of word
#'
validate_distns <- function(model) {

  model_names <- c("exp",
                   "weibull",
                   "gompertz",
                   "loglogistic",
                   "lognormal",
                   "gengamma")
  res <-
    unname(sapply(model, FUN =
                    \(x) model_names[startsWith(x, model_names)]))

  if (any(!res %in% model_names))
    stop("distribution not available.", call. = FALSE)

 res
}
