
#'
validate_distns <- function(model) {

  model_names <- c("exponential",
                   "weibull",
                   "gompertz",
                   "loglogistic",
                   "lognormal")

  model <- match.arg(model, choices = model_names)

  if (!model %in% model_names)
    stop("distribution not available.", call. = FALSE)

 model
}
