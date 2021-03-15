
#' @examples
#' lapply(create_pfs_code("lognormal"), cat)
#'
create_pfs_code <- function(pfs_model) {

  scode <- list()

  scode$data <-
    common_code_event_data("pfs")

  scode$trans_params <- list(
    def =
      c("\tvector[n_pfs] lp_pfs;\n"))

  if (pfs_model == "exp") {
    scode$trans_params$def <-
      paste0(scode$trans_params$def,
             c("// rate parameters
             vector[n_pfs] lambda_pfs;"))

    scode$trans_params$main <-
      c("lambda_pfs = exp(lp_pfs);\n")

    scode$generated_quantities$main <-
      c("\tmean_pfs = exp(beta_pfs[1]);\n")
  }

  if (pfs_model %in% c("gompertz", "loglogistic", "weibull")) {
    scode$data <-
      paste0(scode$data,
             "\treal<lower=0> a_shape_pfs;
              real<lower=0> b_shape_pfs;\n")

    scode$parameters <-
      c("real<lower=0> shape_pfs;\n")

    scode$trans_params$def <-
      paste0(scode$trans_params$def,
             c("// rate parameters
               vector[n_pfs] lambda_pfs;\n"))

    scode$trans_params$main <-
      c("lambda_pfs = exp(lp_pfs);\n")

    scode$model <-
      c("shape_pfs ~ gamma(a_shape_pfs, b_shape_pfs);\n")

    scode$generated_quantities$def <-
      c("\treal pshape_pfs = gamma_rng(a_shape_pfs, b_shape_pfs);\n")
    scode$generated_quantities$main <-
      c("\tmean_pfs = exp(beta_pfs[1]);\n")
  }

  if (pfs_model == "lognormal") {
    scode$data <-
      paste0(scode$data,
             "\treal<lower=0> a_sd_pfs;    // gamma hyper-parameters
              real<lower=0> b_sd_pfs;\n")

    scode$parameters <-
      c("\treal<lower=0> sd_pfs;\n")

    scode$trans_params$def <-
      paste0(scode$trans_params$def,
             c("// rate parameters
              vector[n_pfs] mu_pfs;\n"))

    scode$trans_params$main <-
      c("mu_pfs = lp_pfs;\n")

    scode$model <-
      c("\tsd_pfs ~ gamma(a_sd_pfs, b_sd_pfs);\n")

    scode$generated_quantities$def <-
      c("\treal psd_pfs = gamma_rng(a_sd_pfs, b_sd_pfs);\n")
    scode$generated_quantities$main <-
      c("\tmean_pfs = beta_pfs[1];\n")
  }

  scode
}


#
create_os_code <- function(os_model) {

  scode <- list()

  scode$data <-
    common_code_event_data("os")

  scode$trans_params <-
    list(
      def =
        c("\tvector[n_os] lp_os;\n"))

  if (os_model == "exp") {
    scode$trans_params$def <-
      paste0(scode$trans_params$def,
             c("// rate parameters
               vector[n_os] lambda_os;\n"))

    scode$trans_params$main <-
      c("lambda_os = exp(lp_os);\n")

    scode$generated_quantities$main <-
      c("\tmean_os = exp(beta_os[1]);\n")
  }

  if (os_model %in% c("gompertz", "loglogistic", "weibull")) {
    scode$data <-
      paste0(scode$data,
             "\treal<lower=0> a_shape_os;
               real<lower=0> b_shape_os;\n")

    scode$parameters <-
      c("real<lower=0> shape_os;\n")

    scode$trans_params$def <-
      paste0(scode$trans_params$def,
             c("// rate parameters
              vector[n_os] lambda_os;\n"))

    scode$trans_params$main <-
      c("lambda_os = exp(lp_os);\n")

    scode$model <-
      c("shape_os ~ gamma(a_shape_os, b_shape_os);\n")

    scode$generated_quantities$def <-
      c("\treal pshape_os = gamma_rng(a_shape_os, b_shape_os);\n")
    scode$generated_quantities$main <-
      c("\tmean_os = exp(beta_os[1]);\n")
  }

  if (os_model == "lognormal") {
    scode$data <-
      paste0(scode$data,
             c("\treal<lower=0> a_sd_os;    // gamma hyper-parameters
              real<lower=0> b_sd_os;\n"))

    scode$parameters <-
      c("\treal<lower=0> sd_os;\n")

    scode$trans_params$def <-
      paste0(scode$trans_params$def,
             c("// rate parameters
              vector[n_os] mu_os;\n"))

    scode$trans_params$main <-
      c("mu_os = lp_os;\n")

    scode$model <-
      c("\tsd_os ~ gamma(a_sd_os, b_sd_os);\n")

    scode$generated_quantities$def <-
      c("\treal psd_os = gamma_rng(a_sd_os, b_sd_os);\n")
    scode$generated_quantities$main <-
      c("\tmean_os = beta_os[1];\n")
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
    def =
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

  scode$generated_quantities <-
    c("// cure fraction prior
      if (cf_model == 3) {
        //TODO: include extra sd_cf_os, sd_cf_pfs variation?
        real pcurefrac = normal_rng(mu_cf_gl[1], sigma_cf_gl[1]);
        pmean_cf_os = inv_logit(pcurefrac);
        pmean_cf_pfs = inv_logit(pcurefrac);
      } else if (cf_model == 2) {
        real pcf_os = normal_rng(mu_cf_os[1], sd_cf_os[1]);
        real pcf_pfs = normal_rng(mu_cf_pfs[1], sd_cf_pfs[1]);
        pmean_cf_os = inv_logit(pcf_os);
        pmean_cf_pfs = inv_logit(pcf_pfs);
      } else {
        real pcurefrac = beta_rng(a_cf[1], b_cf[1]);
        pmean_cf_os = pcurefrac;
        pmean_cf_pfs = pcurefrac;
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
    def =
      c("\tvector[n_os] lp_os_bg;
      vector[n_os] lp_pfs_bg;

      vector[n_os] lambda_os_bg;
      vector[n_os] lambda_pfs_bg;\n"),
    main =
      c("// correlated event times
      if (joint_model) {
        lp_os = X_os*beta_os + beta_joint[1]*(t_pfs - 1/exp(beta_pfs[1]));
      } else {
        lp_os = X_os*beta_os;
      }\n
      lp_pfs = X_pfs*beta_pfs;\n
      // background survival with uncertainty\n
      if (bg_model == 1) {
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
        beta_bg ~ normal(mu_bg, sigma_bg);
      }\n
      if (joint_model) {
        beta_joint ~ normal(mu_joint, sigma_joint);
      }\n")

  scode$generated_quantities$def <-
    c("\treal mean_os;
      real mean_pfs;
      real mean_bg;

      vector[t_max] S_bg;
      vector[t_max] S_os;
      vector[t_max] S_pfs;
      vector[t_max] S_os_pred;
      vector[t_max] S_pfs_pred;

      // prior pred
      real pmean_os;
      real pmean_pfs;
      real pmean_bg;
      real pmean_cf_os;
      real pmean_cf_pfs;

      vector[t_max] pS_bg;
      vector[t_max] pS_os;
      vector[t_max] pS_pfs;
      vector[t_max] S_os_prior;
      vector[t_max] S_pfs_prior;

      vector[n_os] log_lik;

      real pbeta_os = normal_rng(mu_0_os[1], sigma_0_os[1]);
      real pbeta_pfs = normal_rng(mu_0_pfs[1], sigma_0_pfs[1]);

      real pbeta_bg;\n\n")

  scode$generated_quantities$main <-
    c("// background rate
      if (bg_model == 1) {
      pbeta_bg = normal_rng(mu_bg[1], sigma_bg[1]);
      } else {
      //pbeta_bg = log(mean(h_bg_os));
      pbeta_bg = log(0.001);
      }\n\n
      if (bg_model == 1) {
        mean_bg = exp(beta_bg[1]);
      } else {
        //mean_bg = mean(h_bg_os);
        mean_bg = 0.001;
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
         lognormal = c("mu", "sd"),
         gengamma = "")

  os_params <- paste0(distn_params[[os_model]], "_os")
  pfs_params <- paste0(distn_params[[pfs_model]], "_pfs")

  os_params[grep("mu", os_params)] <-
    paste0(os_params[grep("mu", os_params)], "[i]")

  os_params[grep("lambda", os_params)] <-
    paste0(os_params[grep("lambda", os_params)], "[i]")

  pfs_params[grep("mu", pfs_params)] <-
    paste0(pfs_params[grep("mu", pfs_params)], "[i]")

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


common_code_event_data <- function(e) {
  glue(
    " int<lower=0> n_{e};\n",
    " int<lower=0> H_{e};\n",
    " vector[n_{e}] t_{e};\n",
    " vector[n_{e}] d_{e};\n",
    " matrix[n_{e}, H_{e}] X_{e};\n",
    " vector[H_{e}] mu_0_{e};\n",
    " vector<lower=0>[H_{e}] sigma_0_{e};\n\n")
}


make_postpred <- function(os_model, pfs_model) {

  distn_params <-
    list(exp = "mean",
         weibull = c("shape", "mean"),
         gompertz = c("shape", "mean"),
         loglogistic = c("shape", "mean"),
         lognormal = c("mean", "sd"),
         gengamma = "")

  os_params <- paste0(distn_params[[os_model]], "_os")
  pfs_params <- paste0(distn_params[[pfs_model]], "_pfs")

  pfs_params <- paste(pfs_params, collapse = ", ")
  os_params <- paste(os_params, collapse = ", ")

  glue(
    "// posterior mean checks
    for (i in 1:t_max) {{
    S_bg[i] = exp_Surv(i, mean_bg);
    S_os[i] = exp_{os_model}_Surv(i, {os_params}, mean_bg);
    S_pfs[i] = exp_{pfs_model}_Surv(i, {pfs_params}, mean_bg);
    S_os_pred[i] = cf_os*S_bg[i] + (1 - cf_os)*S_os[i];
    S_pfs_pred[i] = cf_pfs*S_bg[i] + (1 - cf_pfs)*S_pfs[i];
    }\n\n")
}


make_priorpred <- function(os_model, pfs_model) {

  distn_params <-
    list(exp = "pmean",
         weibull = c("pshape", "pmean"),
         gompertz = c("pshape", "pmean"),
         loglogistic = c("pshape", "pmean"),
         lognormal = c("pmean", "psd"),
         gengamma = "")

  os_params <- paste0(distn_params[[os_model]], "_os")
  pfs_params <- paste0(distn_params[[pfs_model]], "_pfs")

  pfs_params <- paste(pfs_params, collapse = ", ")
  os_params <- paste(os_params, collapse = ", ")

  glue(
    "// prior mean checks
    pmean_os = exp(pbeta_os);
    pmean_pfs = exp(pbeta_pfs);
    pmean_bg = exp(pbeta_bg);

    for (i in 1:t_max) {{
     pS_bg[i] = exp_Surv(i, pmean_bg);
     pS_os[i] = exp_{os_model}_Surv(i, {os_params}, pmean_bg);
     pS_pfs[i] = exp_{pfs_model}_Surv(i, {pfs_params}, pmean_bg);
     S_os_prior[i] = pmean_cf_os*pS_bg[i] + (1 - pmean_cf_os)*pS_os[i];
     S_pfs_prior[i] = pmean_cf_pfs*pS_bg[i] + (1 - pmean_cf_pfs)*pS_pfs[i];
   }\n\n")
}


##TODO: duplication with make_loglik()
make_loo <- function(os_model, pfs_model) {

  distn_params <-
    list(exp = "lambda",
         weibull = c("shape", "lambda"),
         gompertz = c("shape", "lambda"),
         loglogistic = c("shape", "lambda"),
         lognormal = c("mu", "sd"),
         gengamma = "")

  os_params <- paste0(distn_params[[os_model]], "_os")
  pfs_params <- paste0(distn_params[[pfs_model]], "_pfs")

  os_params[grep("mu", os_params)] <-
    paste0(os_params[grep("mu", os_params)], "[i]")

  os_params[grep("lambda", os_params)] <-
    paste0(os_params[grep("lambda", os_params)], "[i]")

  pfs_params[grep("mu", pfs_params)] <-
    paste0(pfs_params[grep("mu", pfs_params)], "[i]")

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
       log_lik[i] = log_sum_exp(
        log(cf_os) +
          surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
        log1m(cf_os) +
          {surv_lpdf_os}(t_os[i] | d_os[i], {os_params}, lambda_os_bg[i])) +
        log_sum_exp(
          log(cf_pfs) +
            surv_exp_lpdf(t_pfs[i] | d_pfs[i], lambda_pfs_bg[i]),
          log1m(cf_pfs) +
            {surv_lpdf_pfs}(t_pfs[i] | d_pfs[i], {pfs_params}, lambda_pfs_bg[i]));
         }\n\n")

  scode
}

