
#' create_stancode
#'
#' @param os_mode
#' @param pfs_model
#' @param cf_model
#' @param joint_model
#'
#' @return
#' @export
#' @importFrom glue glue
#'
#' @examples
#' create_stancode("exp", "lognormal", 3, 1)
#'
create_stancode <- function(os_model,
                            pfs_model,
                            cf_model,
                            joint_model) {

  # validate_data()
  # validate_vars()

  stancode <- create_code_skeleton()
  pfs_code <- create_pfs_code(pfs_model)
  os_code <- create_os_code(os_model)
  cf_code <- create_cf_code(cf_model)
  loglik_code <- make_loglik(os_model, pfs_model)
  priorpred_code <- make_priorpred(os_model, pfs_model)
  postpred_code <- make_postpred(os_model, pfs_model)
  loo_code <- make_loo(os_model, pfs_model)

  scode <- list()

  scode$functions <-
    c("functions {\n#include /include/distributions.stan\n}\n\n")

  # generate data block
  scode$data <- paste0(
    "data {\n",
    pfs_code$data,
    os_code$data,
    stancode$data,
    cf_code$data,
    "\n}\n\n"
  )

  # generate parameters block
  scode$parameters <- paste0(
    "parameters {\n",
    pfs_code$parameters,
    os_code$parameters,
    stancode$parameters,
    cf_code$parameters,
    "\n}\n\n"
  )

  # generate transformed parameters block
  scode$trans_params <- paste0(
    "transformed parameters {\n",
    stancode$trans_params$def,
    os_code$trans_params$def,
    pfs_code$trans_params$def,
    cf_code$trans_params$def,
    stancode$trans_params$main,
    os_code$trans_params$main,
    pfs_code$trans_params$main,
    cf_code$trans_params$main,
    "\n}\n\n"
  )


  # combine likelihood with prior part
  scode$model <- paste0(
    "model {\n",
    pfs_code$model,
    os_code$model,
    stancode$model,
    cf_code$model,
    loglik_code,
    "\n}\n\n"
  )

  # generate generated quantities block
  scode$generated_quantities <- paste0(
    "generated quantities {\n",
    stancode$generated_quantities$def,
    pfs_code$generated_quantities$def,
    os_code$generated_quantities$def,
    pfs_code$generated_quantities$main,
    os_code$generated_quantities$main,
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

