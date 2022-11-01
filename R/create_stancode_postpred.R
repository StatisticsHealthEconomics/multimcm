
#'
create_stancode_postpred <- function(models = c("exp",
                                                "weibull",
                                                "gompertz",
                                                "loglogistic",
                                                "lognormal")) {
  models <- match.arg(models)

  scode <- list()

  scode$functions <- paste0(
    "functions {\n",
    ppv_functions_block(models),
    "\n}\n\n"
  )

  scode$data <- paste0(
    "data {\n",
    ppv_data_block(models),
    "\n}\n\n"
  )

  scode$parameters <- "parameters {\n}\n\n"

  scode$generated_quantities <- paste0(
    "generated quantities {\n",
    ppv_gen_quants_block(models),
    "\n}\n\n"
  )

  paste0(
    scode$functions,
    scode$data,
    scode$parameters,
    scode$generated_quantities)
}

