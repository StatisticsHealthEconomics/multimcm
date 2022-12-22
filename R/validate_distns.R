
#' partial match on start of word
#'
validate_distns <- function(model) {

  model_names <- c("exp",
                   "weibull",
                   "gompertz",
                   "loglogistic",
                   "lognormal",
                   "gengamma")

  model <- model_names[startsWith(model, model_names)]

  if (any(!model %in% model_names))
    stop("distribution not available.", call. = FALSE)

 model
}
