
#' @examples
#' lapply(create_pfs_code(1), cat)
#'
create_pfs_code <- function(pfs_model) {

  scode <- list()

  # common to all
  scode$data <-
    c("\tint<lower=0> n_pfs;
      int<lower=0> H_pfs;
      vector[n_pfs] t_pfs;
      vector[n_pfs] d_pfs;
      matrix[n_pfs, H_pfs] X_pfs;
      vector[H_pfs] mu_0_pfs;
      vector<lower=0> [H_pfs] sigma_0_pfs;\n")

  scode$trans_params <- list(
    declarations =
    c("\tvector[n_pfs] lp_pfs;
      vector[n_os] lp_pfs_bg;
      vector[n_os] lambda_pfs_bg;\n"))

  if (pfs_model == "exp") {
    scode$trans_params$declations <-
      paste0(scode$trans_params$declarations,
      c("// rate parameters
        vector[n_pfs] lambda_pfs;"))

    scode$trans_params$main <-
        c("lambda_pfs = exp(lp_pfs);\n")
  }

  if (pfs_model %in% c("gompertz", "loglogistic", "weibull")) {
    scode$data <-
      paste0(scode$data,
             "\treal<lower=0> a_shape_pfs;
              real<lower=0> b_shape_pfs;\n")

    scode$parameters <-
      c("\treal<lower=0> shape_pfs;\n")

    scode$trans_params$declations <-
      paste0(scode$trans_params$declarations,
             c("// rate parameters
        vector[n_pfs] lambda_pfs;"))

    scode$trans_params$main <-
      c("lambda_pfs = exp(lp_pfs);\n")

    scode$model <-
      c("\tshape_pfs ~ gamma(a_shape_pfs, b_shape_pfs);\n")
  }

  if (pfs_model == "lognormal") {
    scode$data <-
      paste0(scode$data,
             "\treal<lower=0> a_sd_pfs;    // gamma hyper-parameters
              real<lower=0> b_sd_pfs;\n")

    scode$parameters <-
      c("\treal<lower=0> sd_pfs;\n")

    scode$trans_params$declations <-
      paste0(scode$trans_params$declarations,
             c("// rate parameters
        vector[n_pfs] mean_pfs;"))

    scode$trans_params$main <-
      c("mean_pfs = lp_pfs;\n")

    scode$model <-
      c("\tsd_pfs ~ gamma(a_sd_pfs, b_sd_pfs);\n")
  }

  scode
}


create_os_code <- function(os_model) {

  scode <- list()

  # common to all
  scode$data <-
    c("\tint<lower=0> n_os;         // number of observations
      int<lower=0> H_os;             // number of covariates
      vector[n_os] t_os;             // observation times
      vector[n_os] d_os;             // censoring indicator (1 = observed, 0 = censored)
      matrix[n_os, H_os] X_os;       // matrix of covariates (with n rows and H columns)
      vector[H_os] mu_0_os;
      vector<lower=0> [H_os] sigma_0_os;\n")

  scode$trans_params <-
    c("\tvector[n_os] lp_os;
      vector[n_os] lp_os_bg;
      vector[n_os] lambda_os_bg;\n")

  if (os_model == "exp") {
    scode$trans_params$declations <-
      paste0(scode$trans_params$declarations,
             c("// rate parameters
               vector[n_os] lambda_os;"))

    scode$trans_params$main <-
      c("lambda_os = exp(lp_os);\n")
  }

  if (os_model %in% c("gompertz", "loglogistic", "weibull")) {
    scode$data <-
      paste0(scode$data,
             "\treal<lower=0> a_shape_os;
               real<lower=0> b_shape_os;\n")

    scode$parameters <-
      c("\treal<lower=0> shape_os;\n")

    scode$trans_params$declations <-
      paste0(scode$trans_params$declarations,
             c("// rate parameters
              vector[n_os] lambda_os;"))

    scode$trans_params$main <-
      c("lambda_os = exp(lp_os);\n")

    scode$model <-
      c("\tshape_os ~ gamma(a_shape_os, b_shape_os);\n")
  }

  if (os_model == "lognormal") {
    scode$data <-
      c(scode$data,
        "\treal<lower=0> a_sd_os;    // gamma hyper-parameters
        real<lower=0> b_sd_os;\n")

    scode$parameters <-
      c("\treal<lower=0> sd_os;\n")

    scode$trans_params$declations <-
      paste0(scode$trans_params$declarations,
             c("// rate parameters
              vector[n_os] mean_os;"))

    scode$trans_params$main <-
      c("mean_os = lp_os;\n")

    scode$model <-
      c("\tsd_os ~ gamma(a_sd_os, b_sd_os);\n")
  }


  scode
}


create_cf_code <- function(cf_model) {

  scode <- list()
  scode$data <-
    c("\tint<lower=1, upper=3> cf_model;         // cure fraction
      real mu_cf_gl[cf_model == 3 ? 1 : 0];   // 1- shared; 2- separate; 3- hierarchical
      real mu_cf_os[cf_model == 2 ? 1 : 0];
      real mu_cf_pfs[cf_model == 2 ? 1 : 0];
      real<lower=0> sigma_cf_gl[cf_model == 3 ? 1 : 0];
      real<lower=0> sd_cf_os[cf_model != 1 ? 1 : 0];
      real<lower=0> sd_cf_pfs[cf_model != 1 ? 1 : 0];
      real a_cf[cf_model == 1 ? 1 : 0];
      real b_cf[cf_model == 1 ? 1 : 0];")

  scode$parameters <-
    c("\treal<lower=0, upper=1> cf_pooled[cf_model == 1 ? 1 : 0];
      real lp_cf_global[cf_model == 3 ? 1 : 0];
      real lp_cf_os[cf_model != 1 ? 1 : 0];
      real lp_cf_pfs[cf_model != 1 ? 1 : 0];\n")

  scode$trans_params <- list(
    declarations =
      c("\treal<lower=0, upper=1> cf_global[cf_model == 3 ? 1 : 0];
      real<lower=0, upper=1> cf_os;
      real<lower=0, upper=1> cf_pfs;\n"),
    main =
      c("if (cf_model == 3) {
        cf_global = inv_logit(lp_cf_global);
      }\n
      if (cf_model != 1) {
        cf_os = inv_logit(lp_cf_os[1]);
        cf_pfs = inv_logit(lp_cf_pfs[1]);
      } else {
        cf_os = cf_pooled[1];
        cf_pfs = cf_pooled[1];
      }\n"))

  scode$model <-
    c("\t// cure fraction
      if (cf_model == 3) {
        lp_cf_global ~ normal(mu_cf_gl, sigma_cf_gl);
        lp_cf_os ~ normal(lp_cf_global, sd_cf_os);
        lp_cf_pfs ~ normal(lp_cf_global, sd_cf_pfs);
      } else if (cf_model == 2) {
        lp_cf_os ~ normal(mu_cf_os, sd_cf_os);
        lp_cf_pfs ~ normal(mu_cf_pfs, sd_cf_pfs);
      } else {
        cf_pooled ~ beta(a_cf, b_cf);
      }\n")

  scode
}


create_code_skeleton <- function() {

  scode <- list()

  scode$data <-
    c("\tint<lower=1, upper=2> bg_model;
      vector[bg_model == 1 ? H_os : 0] mu_bg;
      vector<lower=0>[bg_model == 1 ? H_os : 0] sigma_bg;
      vector[bg_model == 2 ? n_os : 0] h_bg_os;
      vector[bg_model == 2 ? n_pfs : 0] h_bg_pfs;
      int<lower=0, upper=1> joint_model;
      real mu_joint[joint_model];
      real<lower=0> sigma_joint[joint_model];
      int<lower=0> t_max;\n")

  scode$parameters <-
    c("\tvector[H_os] beta_os;       // coefficients in linear predictor (including intercept)
      vector[H_pfs] beta_pfs;
      vector[bg_model == 1 ? H_os : 0] beta_bg;
      real beta_joint[joint_model];\n")

  scode$trans_params <- list(
    declarations =
      c("\t  vector[n_os] lp_os;
      vector[n_pfs] lp_pfs;
      vector[n_os] lp_os_bg;
      vector[n_os] lp_pfs_bg;

      vector[n_os] lambda_os_bg;
      vector[n_os] lambda_pfs_bg;\n"),
    main =
      c("# correlated event times
      if (joint_model) {
        lp_os = X_os*beta_os + beta_joint[1]*(t_pfs - 1/exp(beta_pfs[1]));
      } else {
        lp_os = X_os*beta_os;
      }\n
      lp_pfs = X_pfs*beta_pfs;\n
      if (bg_model == 1) {         // background survival with uncertainty
        lp_os_bg = X_os*beta_bg;
        lp_pfs_bg = X_pfs*beta_bg;
      } else {
        lp_os_bg = log(h_bg_os);
        lp_pfs_bg = log(h_bg_pfs);
      }\n
      lambda_os_bg = exp(lp_os_bg);
      lambda_pfs_bg = exp(lp_pfs_bg);\n"))

  scode$model <-
    c("\tbeta_os ~ normal(mu_0_os, sigma_0_os);
      beta_pfs ~ normal(mu_0_pfs, sigma_0_pfs);\n
      if (bg_model == 1) {
      }\n
      beta_bg ~ normal(mu_bg, sigma_bg);
      if (joint_model) {
        beta_joint ~ normal(mu_joint, sigma_joint);
      }\n")

  scode
}


#' @importFrom glue glue
#'
make_loglik <- function(os_model, pfs_model) {

  distn_params <-
    list(exp = "lambda",
         weibull = c("shape", "lambda"),
         gompertz = c("shape", "lambda"),
         loglogistic = c("shape", "lambda"),
         lognormal = c("mean", "sd"),
         gengamma = "")

  os_params <- paste0(distn_params[[os_model]], "_os")
  pfs_params <- paste0(distn_params[[pfs_model]], "_pfs")

  os_params[grep("mean", os_params)] <-
    paste0(os_params[grep("mean", os_params)], "[i]")

  os_params[grep("lambda", os_params)] <-
    paste0(os_params[grep("lambda", os_params)], "[i]")

  pfs_params[grep("mean", pfs_params)] <-
    paste0(os_params[grep("mean", pfs_params)], "[i]")

  pfs_params[grep("lambda", pfs_params)] <-
    paste0(pfs_params[grep("lambda", pfs_params)], "[i]")

  pfs_params <- paste(pfs_params, collapse = ", ")
  os_params <- paste(os_params, collapse = ", ")

  surv_lpdf_os <- glue("joint_exp_{os_model}_lpdf")
  surv_lpdf_pfs <- glue("joint_exp_{pfs_model}_lpdf")

  scode <-
    glue("
    // likelihood
    for (i in 1:n_os) {{
       {tp()}log_sum_exp(
        log(cf_os) +
          surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
        log1m(cf_os) +
          {surv_lpdf_os}(t_os[i] | d_os[i], {os_params}, lambda_os_bg[i])) +
        log_sum_exp(
          log(cf_pfs) +
            surv_exp_lpdf(t_pfs[i] | d_pfs[i], lambda_pfs_bg[i]),
          log1m(cf_pfs) +
            {surv_lpdf_pfs}(t_pfs[i] | d_pfs[i], {pfs_params}, lambda_pfs_bg[i]));
         }\n")

  scode
}

# from brms::
tp <- function(wsp = 2) {
  wsp <- glue_collapse(rep(" ", wsp))
  paste0(wsp, "target += ")
}

