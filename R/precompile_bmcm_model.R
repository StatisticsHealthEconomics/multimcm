
#' Precompile bmcm model
#'
#' @param input_data
#' @param family_latent
#' @param cureformula
#' @param model_name
#' @param use_cmdstanr Logical. If TRUE, use cmdstanr to compile the model. Default is FALSE.
#'
#' @return
#' @export
#'
precompile_bmcm_model <- function(input_data,
                                  family_latent = "exponential",
                                  cureformula = ~ 1,
                                  model_name = NA,
                                  use_cmdstanr = FALSE) {
  rtn_wd <- getwd()
  new_wd <- system.file("stan", package = "multimcm")
  setwd(new_wd)
  on.exit(setwd(rtn_wd))

  distns <- validate_distns(family_latent)

  formula_cure <- parse_formula(cureformula, input_data)

  if (length(distns) == 1) {
    if (is_hier(formula_cure)) {
      distns <- rep(distns, formula_cure$re_nlevels[1])
    } else if (is_separate(formula_cure)) {
      distns <- rep(distns, formula_cure$fe_nlevels[2])
    }}

  model_code <- create_stancode(distns)

  if (is.na(model_name)) {
    model_name <-
      paste0("bmcm_stan_", glue::glue_collapse(distns, sep = "_"))
  }

  if (use_cmdstanr) {
    model_path <- cmdstanr::write_stan_file(model_code, dir = ".", basename = model_name)
    precompiled_model <- cmdstanr::cmdstan_model(stan_file = model_path, compile = TRUE)
  } else {
    precompiled_model <- rstan::stan_model(model_code = model_code,
                                           model_name = model_name)
    saveRDS(precompiled_model, file = glue::glue("{model_name}.RDS"))
  }

  invisible(precompiled_model)
}
