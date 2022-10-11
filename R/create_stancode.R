
#' Create Stan code
#'
#' check with
#' writeLines(create_stancode(c("exponential", "exponential"), "temp.stan")
#'
#' @param models Vector survival model names
#'
#' @return
#' @export
#' @importFrom glue glue
#'
#' @examples
#' cat(create_stancode(c("exponential", "exponential"))
#'
create_stancode <- function(models) {

  # validate_data()
  # validate_vars()

  stancode <- create_code_skeleton()

  for (i in seq_along(models)) {
    latent_model_code <- create_latent_model_code(models[i])
  }

  cf_code <- create_cf_code(cf_model)
  loglik_code <- make_loglik(models)
  priorpred_code <- make_priorpred(models)
  postpred_code <- make_postpred(models)
  loo_code <- make_loo(models)

  scode <- list()

  scode$functions <-
    c("functions {\n#include /include/distributions.stan\n}\n\n")

  # generate data block
  scode$data <- paste0(
    "data {\n",
    stancode$data$def,
    cf_code$data$def,
    latent_model_code$data,
    stancode$data$main,
    cf_code$data$main,
    "\n}\n\n"
  )

  # generate parameters block
  scode$parameters <- paste0(
    "parameters {\n",
    latent_model_code$parameters,
    stancode$parameters,
    cf_code$parameters,
    "\n}\n\n"
  )

  # generate transformed parameters block
  scode$trans_params <- paste0(
    "transformed parameters {\n",
    stancode$trans_params$def,
    latent_model_code$trans_params$def,
    cf_code$trans_params$def,
    stancode$trans_params$main,
    latent_model_code$trans_params$main,
    cf_code$trans_params$main,
    "\n}\n\n"
  )

  # combine likelihood with prior part
  scode$model <- paste0(
    "model {\n",
    stancode$model,
    latent_model_code$model,
    cf_code$model,
    loglik_code,
    "\n}\n\n"
  )

  # generate generated quantities block
  scode$generated_quantities <- paste0(
    "generated quantities {\n",
    stancode$generated_quantities$def,
    latent_model_code$generated_quantities$def,
    latent_model_code$generated_quantities$main,
    stancode$generated_quantities$main,
    cf_code$generated_quantities,
    postpred_code,
    priorpred_code,
    loo_code,
    "}\n"
  )

  # combine all elements into a complete Stan model
  paste0(
    scode$functions,
    scode$data,
    scode$parameters,
    scode$trans_params,
    scode$model,
    scode$generated_quantities
  )
}

