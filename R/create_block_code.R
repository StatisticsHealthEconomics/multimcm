# Stan code blocks for create_stancode() ----------------------------------
# all treatments in single model


#' @examples
#' lapply(create_pfs_code("lognormal"), cat)
#'
create_pfs_code <- function(pfs_model) {

  scode <- list()

  scode$data <-
    common_code_event_dataTx("pfs")

  scode$trans_params <- list(
    def =
      c("\tvector[N_pfs] lp_pfs;\n"))

  if (pfs_model == "exp") {
    scode$trans_params$def <-
      paste0(scode$trans_params$def,
             c("// rate parameters
             vector<lower=0>[N_pfs] lambda_pfs;\n"))

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
               vector[N_pfs] lambda_pfs;\n"))

    scode$trans_params$main <-
      c("\tlambda_pfs = exp(lp_pfs);\n")

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
              vector[N_pfs] mu_pfs;\n"))

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
    common_code_event_dataTx("os")

  scode$trans_params <-
    list(
      def =
        c("\tvector[N_os] lp_os;\n"))

  if (os_model == "exp") {
    scode$trans_params$def <-
      paste0(scode$trans_params$def,
             c("// rate parameters
               vector<lower=0>[N_os] lambda_os;\n"))

    scode$trans_params$main <-
      c("\tlambda_os = exp(lp_os);\n")

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
              vector[N_os] lambda_os;\n"))

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
              vector[N_os] mu_os;\n"))

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

#
create_cf_code <- function(cf_model) {

  scode <- list()

  scode$data <- list(
    def =
      c("\tint<lower=1, upper=3> cf_model;         // cure fraction\n"),
    main =
      c("real<lower=0> a_cf[cf_model == 1 ? 1 : 0];
      real<lower=0> b_cf[cf_model == 1 ? 1 : 0];
      vector[cf_model == 3 ? nTx : 0] mu_sd_cf;
      vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_sd_cf;\n"))

  scode$parameters <-
    c("\tvector<lower=0, upper=1>[cf_model == 1 ? nTx : 0] cf_pooled;
      vector[cf_model == 3 ? nTx : 0] lp_cf_os;
      vector[cf_model == 3 ? nTx : 0] lp_cf_pfs;
      vector<lower=0>[cf_model == 3 ? nTx : 0] sd_cf;\n")

  scode$trans_params <- list(
    def =
      c("\tvector<lower=0, upper=1>[cf_model == 3 ? nTx : 0] cf_global;
        vector<lower=0, upper=1>[nTx] cf_os;
        vector<lower=0, upper=1>[nTx] cf_pfs;
        vector[cf_model == 3 ? nTx : 0] lp_cf_global;
        vector[cf_model == 2 ? nTx : 0] tx_cf_os;
        vector[cf_model == 2 ? nTx : 0] tx_cf_pfs;\n"),
    main =
      c("if (cf_model == 3) {
        lp_cf_global = Tx_dmat*alpha;
        cf_global = inv_logit(lp_cf_global);
        cf_os = inv_logit(lp_cf_os);
        cf_pfs = inv_logit(lp_cf_pfs);
      }\n
      if (cf_model == 2) {
        tx_cf_os = Tx_dmat*alpha_os;
        tx_cf_pfs = Tx_dmat*alpha_pfs;
        cf_os = inv_logit(tx_cf_os);
        cf_pfs = inv_logit(tx_cf_pfs);
      }\n
      if (cf_model == 1) {
        cf_os = cf_pooled;
        cf_pfs = cf_pooled;
      }\n"))

  scode$model <-
    c("\t// cure fraction
      if (cf_model == 3) {
        alpha ~ normal(mu_alpha, sigma_alpha);
        sd_cf ~ normal(mu_sd_cf, sigma_sd_cf);
        lp_cf_os ~ normal(lp_cf_global, sd_cf);
        lp_cf_pfs ~ normal(lp_cf_global, sd_cf);
      } else if (cf_model == 2) {
        alpha_os ~ normal(mu_alpha_os, sigma_alpha_os);
        alpha_pfs ~ normal(mu_alpha_pfs, sigma_alpha_pfs);
      } else {
        cf_pooled ~ beta(a_cf, b_cf);
      }\n")

  scode$generated_quantities <-
    c("\n")

  scode
}

#
create_code_skeleton <- function() {

  scode <- list()

  scode$data <- list(
    def =
      c("\tint<lower=1> nTx;\n"),
    main =
      c("int<lower=1, upper=2> bg_model;
      vector[bg_model == 1 ? H_os : 0] mu_bg;
      vector<lower=0>[bg_model == 1 ? H_os : 0] sigma_bg;
      vector<lower=0>[bg_model == 2 ? N_os : 0] h_bg_os;
      vector<lower=0>[bg_model == 2 ? N_pfs : 0] h_bg_pfs;
      int<lower=0, upper=1> joint_model;
      real mu_joint[joint_model];
      real<lower=0> sigma_joint[joint_model];
      matrix[nTx, nTx] Tx_dmat;         // treatment design matrix
      vector[cf_model == 3 ? nTx : 0] mu_alpha;             // treatment regression coefficients
      vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_alpha;
      vector[cf_model == 2 ? nTx : 0] mu_alpha_os;
      vector[cf_model == 2 ? nTx : 0] mu_alpha_pfs;
      vector<lower=0>[cf_model == 2 ? nTx : 0] sigma_alpha_os;
      vector<lower=0>[cf_model == 2 ? nTx : 0] sigma_alpha_pfs;
      int<lower=0> t_max;\n"))

  scode$parameters <-
    c("\tvector[H_os] beta_os;       // coefficients in linear predictor (including intercept)
      vector[H_pfs] beta_pfs;
      vector[bg_model == 1 ? H_os : 0] beta_bg;
      real beta_joint[joint_model];
      vector[cf_model != 2 ? nTx : 0] alpha;
      vector[cf_model == 2 ? nTx : 0] alpha_os;
      vector[cf_model == 2 ? nTx : 0] alpha_pfs;\n")

  scode$trans_params <- list(
    def =
      c("\tvector[N_os] lp_os_bg;
      vector[N_os] lp_pfs_bg;

      vector<lower=0>[N_os] lambda_os_bg;
      vector<lower=0>[N_os] lambda_pfs_bg;\n"),
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
    c("\tint idx_os;
      int idx_pfs;

      beta_os ~ normal(mu_0_os, sigma_0_os);
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
      matrix[t_max, nTx] S_os_pred;
      matrix[t_max, nTx] S_pfs_pred;

      int idx_os;
      int idx_pfs;

      // prior pred
      // real pmean_os;
      // real pmean_pfs;
      // real pmean_bg;
      // real pmean_cf_os;
      // real pmean_cf_pfs;

      // vector[t_max] pS_bg;
      // vector[t_max] pS_os;
      // vector[t_max] pS_pfs;
      // vector[t_max] S_os_prior;
      // vector[t_max] S_pfs_prior;

      vector[N_os] log_lik_os;
      vector[N_pfs] log_lik_pfs;
      vector[N_os] log_lik;

      // real pbeta_os = normal_rng(mu_0_os[1], sigma_0_os[1]);
      // real pbeta_pfs = normal_rng(mu_0_pfs[1], sigma_0_pfs[1]);

      // real pbeta_bg;\n\n")

  scode$generated_quantities$main <-
    c("// background rate
      if (bg_model == 1) {
        mean_bg = exp(beta_bg[1]);
      } else {
      // mean_bg = 0.001;
        mean_bg = mean(h_bg_os);
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
    paste0(pfs_params[grep("mu", pfs_params)], "[j]")

  pfs_params[grep("lambda", pfs_params)] <-
    paste0(pfs_params[grep("lambda", pfs_params)], "[j]")

  pfs_params <- paste(pfs_params, collapse = ", ")
  os_params <- paste(os_params, collapse = ", ")

  surv_lpdf_os <- glue("joint_exp_{os_model}_lpdf")
  surv_lpdf_pfs <- glue("joint_exp_{pfs_model}_lpdf")

  scode <-
    glue("
    idx_os = 1;
    idx_pfs = 1;

    // likelihood
    for (Tx in 1:nTx) {{
      for (i in idx_os:(idx_os + n_os[Tx] - 1)) {{

       {tp()}log_sum_exp(
        log(cf_os[Tx]) +
          surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
        log1m(cf_os[Tx]) +
          {surv_lpdf_os}(t_os[i] | d_os[i], {os_params}, lambda_os_bg[i]));
      }
      for (j in idx_pfs:(idx_pfs + n_pfs[Tx] - 1)) {{

        {tp()}log_sum_exp(
          log(cf_pfs[Tx]) +
            surv_exp_lpdf(t_pfs[j] | d_pfs[j], lambda_pfs_bg[j]),
          log1m(cf_pfs[Tx]) +
            {surv_lpdf_pfs}(t_pfs[j] | d_pfs[j], {pfs_params}, lambda_pfs_bg[j]));
         }

      idx_os = idx_os + n_os[Tx];
      idx_pfs = idx_pfs + n_pfs[Tx];
    }\n")

  scode
}

# from brms::
tp <- function(wsp = 2) {
  wsp <- glue_collapse(rep(" ", wsp))
  paste0(wsp, "target += ")
}

#
common_code_event_data <- function(e) {
  glue(
    " int<lower=0> N_{e};\n",
    " int<lower=0> n_{e}[nTx];\n",
    " int<lower=0> H_{e};\n",
    " vector<lower=0>[N_{e}] t_{e};\n",
    " vector<lower=0, upper=1>[N_{e}] d_{e};\n",
    " matrix[N_{e}, H_{e}] X_{e};\n",
    " vector[H_{e}] mu_0_{e};\n",
    " vector<lower=0>[H_{e}] sigma_0_{e};\n\n")
}

#
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
    for (j in 1:nTx) {{
      for (i in 1:t_max) {{
        S_bg[i] = exp_Surv(i, mean_bg);
        S_os[i] = exp_{os_model}_Surv(i, {os_params}, mean_bg);
        S_pfs[i] = exp_{pfs_model}_Surv(i, {pfs_params}, mean_bg);
        S_os_pred[i, j] = cf_os[j]*S_bg[i] + (1 - cf_os[j])*S_os[i];
        S_pfs_pred[i, j] = cf_pfs[j]*S_bg[i] + (1 - cf_pfs[j])*S_pfs[i];
      }
    }\n\n")
}

#
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
    // pmean_os = exp(pbeta_os);
    // pmean_pfs = exp(pbeta_pfs);
    // pmean_bg = exp(pbeta_bg);

    // for (i in 1:t_max) {{
    //  pS_bg[i] = exp_Surv(i, pmean_bg);
    //  pS_os[i] = exp_{os_model}_Surv(i, {os_params}, pmean_bg);
    //  pS_pfs[i] = exp_{pfs_model}_Surv(i, {pfs_params}, pmean_bg);
    //  S_os_prior[i] = pmean_cf_os*pS_bg[i] + (1 - pmean_cf_os)*pS_os[i];
    //  S_pfs_prior[i] = pmean_cf_pfs*pS_bg[i] + (1 - pmean_cf_pfs)*pS_pfs[i];
    // }\n\n")
}


##TODO: remove duplication with make_loglik()
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
    paste0(pfs_params[grep("mu", pfs_params)], "[j]")

  pfs_params[grep("lambda", pfs_params)] <-
    paste0(pfs_params[grep("lambda", pfs_params)], "[j]")

  pfs_params <- paste(pfs_params, collapse = ", ")
  os_params <- paste(os_params, collapse = ", ")

  surv_lpdf_os <- glue("joint_exp_{os_model}_lpdf")
  surv_lpdf_pfs <- glue("joint_exp_{pfs_model}_lpdf")

  scode <-
    glue("
    // likelihood
      idx_os = 1;
      idx_pfs = 1;

      for (Tx in 1:nTx) {{

      for (i in idx_os:(idx_os + n_os[Tx] - 1)) {{
         log_lik_os[i] = log_sum_exp(
          log(cf_os[Tx]) +
          surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
          log1m(cf_os[Tx]) +
          {surv_lpdf_os}(t_os[i] | d_os[i], {os_params}, lambda_os_bg[i]));
        }

      for (j in idx_pfs:(idx_pfs + n_pfs[Tx] - 1)) {{
        log_lik_pfs[j] = log_sum_exp(
          log(cf_pfs[Tx]) +
            surv_exp_lpdf(t_pfs[j] | d_pfs[j], lambda_pfs_bg[j]),
          log1m(cf_pfs[Tx]) +
            {surv_lpdf_pfs}(t_pfs[j] | d_pfs[j], {pfs_params}, lambda_pfs_bg[j]));
        }

        idx_os = idx_os + n_os[Tx];
        idx_pfs = idx_pfs + n_pfs[Tx];
      }
      log_lik = log_lik_os + log_lik_pfs;\n\n")

  scode
}

