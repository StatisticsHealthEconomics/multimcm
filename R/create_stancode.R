
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
#' @importFrom purrr map2
#'
#' @examples
#' cat(create_stancode(c("exponential", "exponential"))
#'
create_stancode <- function(models) {

  n_grps <- length(models)
  seq_grps <- 1:n_grps

  stancode <- create_code_skeleton(n_grps)
  cf_code <- create_cf_code(n_grps)

  # generate separate blocks of Stan code
  latent_model_code <- map2(models, seq_grps, make_latent_model_code)
  priorpred_code <- map2(models, seq_grps, make_priorpred)
  postpred_code <- map2(models, seq_grps, make_postpred)
  loo_code <- map2(models, seq_grps, make_loo)
  loglik_code <- map2(models, seq_grps, make_loglik)

  latent_model_code <- rearrange_all_grp_in_a_block(latent_model_code)
  priorpred_code <- rearrange_all_grp_in_a_block(priorpred_code)
  postpred_code <- rearrange_all_grp_in_a_block(postpred_code)
  loo_code <- rearrange_all_grp_in_a_block(loo_code)
  loglik_code <- rearrange_all_grp_in_a_block(loglik_code)

  scode <- list()

  scode$functions <-
    c("functions {\n#include /include/distributions.stan\n}\n\n")

  # generate data block
  scode$data <- paste0(
    "data {\n",
    stancode$data_def,
    cf_code$data_def,
    latent_model_code$data_def,
    stancode$data_main,
    cf_code$data_main,
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
    stancode$trans_params_def,
    latent_model_code$trans_params_def,
    cf_code$trans_params_def,
    stancode$trans_params_main,
    latent_model_code$trans_params_main,
    cf_code$trans_params_main,
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
    stancode$generated_quantities_def,
    paste(latent_model_code$generated_quantities_def, collapse = "\n "),
    paste(latent_model_code$generated_quantities_main, collapse = "\n "),
    stancode$generated_quantities_main,
    cf_code$generated_quantities_main,
    do.call(paste, postpred_code),
    do.call(paste, priorpred_code),
    do.call(paste, loo_code),
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


#
rearrange_all_grp_in_a_block <- function(x) {
  simply_x <- simplify_all(transpose(x))
  lapply(simply_x, paste, collapse = "\n ")
}

