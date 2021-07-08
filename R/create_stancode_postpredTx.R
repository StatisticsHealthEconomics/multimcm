
#'
create_stancode_postpredTx <- function(os_model = c("exponential",
                                                    "weibull",
                                                    "gompertz",
                                                    "loglogistic",
                                                    "lognormal"),
                                       pfs_model = c("exponential",
                                                     "weibull",
                                                     "gompertz",
                                                     "loglogistic",
                                                     "lognormal")) {
  os_model <- match.arg(os_model)
  pfs_model <- match.arg(pfs_model)

  scode <- list()

  scode$functions <- paste0(
    "functions {\n",
    ppv_functions_block(os_model, pfs_model),
    "\n}\n\n"
  )

  scode$data <- paste0(
    "data {\n",
    ppv_data_block(os_model, pfs_model),
    "\n}\n\n"
  )

  scode$parameters <- "parameters {\n}\n\n"

  scode$generated_quantities <- paste0(
    "generated quantities {\n",
    ppv_gen_quants_block(os_model, pfs_model),
    "\n}\n\n"
  )

  paste0(
    scode$functions,
    scode$data,
    scode$parameters,
    scode$generated_quantities)
}

