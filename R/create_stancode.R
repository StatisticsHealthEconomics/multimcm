
#' Create Stan code
#'
#' check with
#' writeLines(create_stancode(c("exp", "exp", "gengamma")), "temp.stan")
#'
#' @param models Vector survival model names
#'
#' @return string
#' @export
#' @importFrom glue glue
#'
#' @examples
#' cat(create_stancode(c("exp", "exp")))
#'
create_stancode <- function(models) {
  n_grps <- length(models)

  # generate separate blocks of Stan code

  stancode <- create_code_skeleton(n_grps)
  cf_code <- create_cf_code(n_grps)

  latent_model_code <- priorpred_code <- postpred_code <- list()
  loo_code <- loglik_code <- list()

  for (i in seq_along(models)) {
    latent_model_code[[i]] <-  make_latent_model_code(models[i], id = i)
    priorpred_code[[i]] <- make_priorpred(models[i], id = i)
    postpred_code[[i]] <- make_postpred(models[i], id = i)
    loglik_code[[i]] <- make_loglik(models[i], id = i)
    loo_code[[i]] <- make_loo(models[i], id = i)
  }

  # interleave all model lists
  latent_model_code <- rearrange_blocks(latent_model_code)
  priorpred_code <- rearrange_blocks(priorpred_code)
  postpred_code <- rearrange_blocks(postpred_code)
  loglik_code <- rearrange_blocks(loglik_code)
  loo_code <- rearrange_blocks(loo_code)

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
    latent_model_code$generated_quantities_def,
    latent_model_code$generated_quantities_main,
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


#' @importFrom purrr simplify_all transpose
#'
rearrange_blocks <- function(x) {
  simple_x <- simplify_all(transpose(x))
  lapply(simple_x, paste, collapse = "\n ")
}

