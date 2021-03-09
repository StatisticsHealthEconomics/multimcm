
#' create_stancode
#'
#' @param os_mode
#' @param pfs_model
#' @param cf_model
#' @param joint_model
#'
#' @return
#' @export
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
    stancode$trans_params$declarations,
    os_code$trans_params$declarations,
    pfs_code$trans_params$declarations,
    cf_code$trans_params$declarations,
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

  # # generate generated quantities block
  # scode$generated_quantities <- paste0(
  #   "generated quantities {\n",
  #   stancode$generated_quantities,
  #   pfs_code$generated_quantities,
  #   os_code$generated_quantities,
  #   cf_code$generated_quantities,
  #   "}\n"
  # )

  # combine all elements into a complete Stan model
  paste0(
    scode$functions,
    scode$data,
    scode$parameters,
    scode$trans_params,
    scode$model#,
    # scode$generated_quantities ##TODO:
    )
  }

