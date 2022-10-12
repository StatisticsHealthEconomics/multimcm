# Stan code blocks for create_stancode() ----------------------------------
# all treatments in single model


#' @examples
#' lapply(create_latent_model_code("lognormal"), cat)
#'
make_latent_model_code <- function(model, id = 1L) {

  scode <- list()

  scode$data <-
    common_code_event_data(id)

  scode$trans_params <- list(
    def =
      glue("\tvector[N_{id}] lp_{id};\n"))

  if (model == "exponential") {
    scode$trans_params$def <-
      glue(scode$trans_params$def,
             c("// rate parameters
             vector<lower=0>[N_] lambda_{id};\n"))

    scode$trans_params$main <-
      glue("lambda_{id} = exp(lp_{id});\n")

    scode$generated_quantities$main <-
      glue("\tmean_{id} = exp(beta_{id}[1]);\n")
  }

  if (model %in% c("gompertz", "loglogistic", "weibull")) {
    scode$data <-
      glue(scode$data,
             "\treal<lower=0> a_shape_{id};
              real<lower=0> b_shape_{id};\n")

    scode$parameters <-
      glue("real<lower=0> shape_{id};\n")

    scode$trans_params$def <-
      glue(scode$trans_params$def,
             c("// rate parameters
               vector[N_{id}] lambda_{id};\n"))

    scode$trans_params$main <-
      glue("\tlambda_{id} = exp(lp_{id});\n")

    scode$model <-
      glue("shape_{id} ~ gamma(a_shape_{id}, b_shape_{id});\n")

    scode$generated_quantities$def <-
      glue("\treal pshape_{id} = gamma_rng(a_shape_{id}, b_shape_{id});\n")
    scode$generated_quantities$main <-
      glue("\tmean_{id} = exp(beta_{id}[1]);\n")
  }

  if (model == "lognormal") {
    scode$data <-
      glue(scode$data,
             "\treal<lower=0> a_sd_{id};    // gamma hyper-parameters
              real<lower=0> b_sd_{id};\n")

    scode$parameters <-
      glue("\treal<lower=0> sd_{id};\n")

    scode$trans_params$def <-
      glue(scode$trans_params$def,
             c("// rate parameters
              vector[N_{id}] mu_{id};\n"))

    scode$trans_params$main <-
      glue("mu_{id} = lp_{id};\n")

    scode$model <-
      glue("\t sd_{id} ~ gamma(a_sd_{id}, b_sd_{id});\n")

    scode$generated_quantities$def <-
      glue("\t real psd_{id} = gamma_rng(a_sd_{id}, b_sd_{id});\n")

    scode$generated_quantities$main <-
      glue("\t mean_{id} = beta_{id}[1];\n")
  }

  scode
}


#
create_cf_code <- function(n_grp) {

  scode <- list()
  ids <- data.frame(id = 1:n_grp)

  scode$data <- list(
    def =
      c("\t int<lower=1, upper=3> cf_model;         // cure fraction\n"),
    main =
      c("real<lower=0> a_cf[cf_model == 1 ? 1 : 0];
      real<lower=0> b_cf[cf_model == 1 ? 1 : 0];
      vector[cf_model == 3 ? nTx : 0] mu_sd_cf;
      vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_sd_cf;\n"))

  scode$parameters <-
    paste0("\t vector<lower=0, upper=1>[cf_model == 1 ? nTx : 0] cf_pooled;",
           glue_data(ids, "vector[cf_model == 3 ? nTx : 0] lp_cf_{id};"),
            "vector<lower=0>[cf_model == 3 ? nTx : 0] sd_cf;\n")

  scode$trans_params <- list(
    def =
        paste0("\t vector<lower=0, upper=1>[cf_model == 3 ? nTx : 0] cf_global;",
               glue_data(ids,
                    "vector<lower=0, upper=1>[nTx] cf_{id};
                     vector[cf_model == 2 ? nTx : 0] tx_cf_{id};"),
              "vector[cf_model == 3 ? nTx : 0] lp_cf_global;\n"),
    main =
      paste0("if (cf_model == 1) {
        cf_{id} = cf_pooled;
      }\n
      if (cf_model == 3) {
        lp_cf_global = Tx_dmat*alpha;
        cf_global = inv_logit(lp_cf_global);",
      glue_data(ids, "cf_{id} = inv_logit(lp_cf_{id});"),
      "}",
      "if (cf_model == 2) {",
      glue_data(ids, "tx_cf_{id} = Tx_dmat*alpha_{id};
                      cf_{id} = inv_logit(tx_cf_{id});"),
      "}\n"))

  scode$model <-
    paste0("\t// cure fraction
      if (cf_model == 3) {
        alpha ~ normal(mu_alpha, sigma_alpha);
        sd_cf ~ normal(mu_sd_cf, sigma_sd_cf);",
        glue_data(ids, "lp_cf_{id} ~ normal(lp_cf_global, sd_cf);"),
      "} else if (cf_model == 2) {",
        glue_data(ids, "alpha_{id} ~ normal(mu_alpha_{id}, sigma_alpha_{id});"),
      "} else {
        cf_pooled ~ beta(a_cf, b_cf);
      }\n")

  scode$generated_quantities <-
    c("\n")

  scode
}

#
create_code_skeleton <- function(n_grp) {

  scode <- list()
  ids <- data.frame(id = 1:n_grp)

  scode$data <- list(
    def =
      c("\tint<lower=1> nTx;\n"),
    main =
      paste0("int<lower=1, upper=2> bg_model;
      vector[bg_model == 1 ? H_os : 0] mu_bg;
      vector<lower=0>[bg_model == 1 ? H_os : 0] sigma_bg;",

      glue_data(ids, "vector<lower=0>[bg_model == 2 ? N_{id} : 0] h_bg_{id};"),

      "matrix[nTx, nTx] Tx_dmat;         // treatment design matrix
      vector[cf_model == 3 ? nTx : 0] mu_alpha;             // treatment regression coefficients
      vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_alpha;
      int<lower=0> t_max;",
      glue_data(ids,
                "vector[cf_model == 2 ? nTx : 0] mu_alpha_{id};
                 vector<lower=0>[cf_model == 2 ? nTx : 0] sigma_alpha_{id};\n")))

  scode$parameters <-
    paste0("\t // coefficients in linear predictor (including intercept)
      vector[bg_model == 1 ? H_1 : 0] beta_bg;
      vector[cf_model != 2 ? nTx : 0] alpha;",
      glue_data(ids, "vector[cf_model == 2 ? nTx : 0] alpha_{id};
                      vector[H_{id}] beta_{id};\n"))

  scode$trans_params <- list(
    def =
      glue_data(ids,
                "\t vector[N_{id}] lp_{id}_bg;
                vector<lower=0>[N_{id}] lambda_{id}_bg;\n"),
    main =
      glue_data(ids,
      "// correlated event times
        lp_{id} = X_{id}*beta_{id};

      // background survival with uncertainty\n
      if (bg_model == 1) {
        lp_{id}_bg = X_{id}*beta_bg;
      } else {
        lp_{id}_bg = log(h_bg_{id});
      }\n
      lambda_{id}_bg = exp(lp_{id}_bg);\n"))

  scode$model <-
    paste0(
      glue_data(ids, "\t int idx_{id};
      beta_{id} ~ normal(mu_S_{id}, sigma_S_{id});\n"),
      "if (bg_model == 1) {
        beta_bg ~ normal(mu_bg, sigma_bg);
      }\n")

  scode$generated_quantities$def <-
    paste0("\t real mean_bg;
      // real pbeta_bg;
      vector[N_1] log_lik;
      vector[t_max] S_bg;",
      glue_data(ids,
      "vector[t_max] S_{id};
      matrix[t_max, nTx] S_{id}_pred;
      real mean_{id};
      int idx_{id};
      vector[N_{id}] log_lik_{id};
      // real pbeta_{id} = normal_rng(mu_S_{id}[1], sigma_S_{id}[1]);
      \n\n"))

  scode$generated_quantities$main <-
    paste0("// background rate
      if (bg_model == 1) {
        mean_bg = exp(beta_bg[1]);
      } else {
      // mean_bg = 0.001;",
        glue_data(ids, "mean_bg = mean(h_bg_{id});"),
      "}\n")

  scode
}


#' @importFrom glue glue
#'
make_loglik <- function(model, id) {

  distn_params <-
    list(exponential = "lambda",
         weibull = c("shape", "lambda"),
         gompertz = c("shape", "lambda"),
         loglogistic = c("shape", "lambda"),
         lognormal = c("mu", "sd"),
         gengamma = "")

  ll_params <- glue("distn_params[[ll_params]]}_{id}")

  ll_params[grep("mu", ll_params)] <-
    paste0(ll_params[grep("mu", ll_params)], "[i]")

  ll_params[grep("lambda", ll_params)] <-
    paste0(ll_params[grep("lambda", ll_params)], "[i]")

  ll_params <- paste(ll_params, collapse = ", ")

  surv_lpdf <- glue("joint_exp_{model}_lpdf")

  scode <-
    glue("
    idx_{id} = 1;

    // likelihood
    for (Tx in 1:nTx) {{
      for (i in idx_{id}:(idx_{id} + n_{id}[Tx] - 1)) {{

       {tp()}log_sum_exp(
        log(cf_{id}[Tx]) +
          surv_exp_lpdf(t_{id}[i] | d_{id}[i], lambda_{id}_bg[i]),
        log1m(cf_{id}[Tx]) +
          {surv_lpdf_{id}}(t_{id}[i] | d_{id}[i], {ll_params}, lambda_{id}_bg[i]));
      }

      idx_{id} = idx_{id} + n_{id}[Tx];
    }\n")

  scode
}

# from brms::
tp <- function(wsp = 2) {
  wsp <- glue_collapse(rep(" ", wsp))
  paste0(wsp, "target += ")
}

#
common_code_event_data <- function(id) {
  glue_(
    " int<lower=0> N_{id};\n",
    " int<lower=0> n_{id}[nTx];\n",
    " int<lower=0> H_{id};\n",
    " vector<lower=0>[N_{id}] t_{id};\n",
    " vector<lower=0, upper=1>[N_{id}] d_{id};\n",
    " matrix[N_{id}, H_{id}] X_{id};\n",
    " vector[H_{id}] mu_S_{id};\n",
    " vector<lower=0>[H_{id}] sigma_S_{id};\n\n")
}

#
make_postpred <- function(model, id) {

  distn_params <-
    list(exponential = "mean",
         weibull = c("shape", "mean"),
         gompertz = c("shape", "mean"),
         loglogistic = c("shape", "mean"),
         lognormal = c("mean", "sd"),
         gengamma = "")

  pp_params <- paste0(distn_params[[model]], "_{id}")
  pp_params <- paste(pp_params, collapse = ", ")

  glue(
    "// posterior mean checks
    for (j in 1:nTx) {{
      for (i in 1:t_max) {{
        S_bg[i] = exp_Surv(i, mean_bg);
        S_{id}[i] = exp_{os_model}_Surv(i, {pp_params}, mean_bg);
        S_{id}_pred[i, j] = cf_{id}[j]*S_bg[i] + (1 - cf_{id}[j])*S_{id}[i];
      }
    }\n\n")
}

#
make_priorpred <- function(model, is) {

  distn_params <-
    list(exponential = "pmean",
         weibull = c("pshape", "pmean"),
         gompertz = c("pshape", "pmean"),
         loglogistic = c("pshape", "pmean"),
         lognormal = c("pmean", "psd"),
         gengamma = "")

  pp_params <- paste0(distn_params[[model]], "_{id}")
  pp_params <- paste(pp_params, collapse = ", ")

  glue(
    "// prior mean checks
    // pmean_pfs = exp(pbeta_pfs);
    // pmean_bg = exp(pbeta_bg);

    // for (i in 1:t_max) {{
    //  pS_bg[i] = exp_Surv(i, pmean_bg);
    //  pS_{id}[i] = exp_{pp_model}_Surv(i, {pp_params}, pmean_bg);
    //  S_{id}_prior[i] = pmean_cf_{id}*pS_bg[i] + (1 - pmean_cf_{id})*pS_{id}[i];
    // }\n\n")
}


##TODO: remove duplication with make_loglik()
make_loo <- function(model, id) {

  distn_params <-
    list(exponential = "lambda",
         weibull = c("shape", "lambda"),
         gompertz = c("shape", "lambda"),
         loglogistic = c("shape", "lambda"),
         lognormal = c("mu", "sd"),
         gengamma = "")

  loo_params <- paste0(distn_params[[model]], "_os")

  loo_params[grep("mu", loo_params)] <-
    paste0(loo_params[grep("mu", loo_params)], "[i]")

  loo_params[grep("lambda", loo_params)] <-
    paste0(os_params[grep("lambda", loo_params)], "[i]")

  loo_params <- paste(loo_params, collapse = ", ")

  surv_lpdf_os <- glue("joint_exp_{loo_params}_lpdf")

  scode <-
    glue("
    // likelihood
      idx_{id} = 1;

      for (Tx in 1:nTx) {{

      for (i in idx_{id}:(idx_{id} + n_{id}[Tx] - 1)) {{
         log_lik_{id}[i] = log_sum_exp(
          log(cf_{id}[Tx]) +
          surv_exp_lpdf(t_{id}[i] | d_{id}[i], lambda_{id}_bg[i]),
          log1m(cf_os[Tx]) +
          {surv_lpdf_{id}}(t_{id}[i] | d_{id}[i], {loo_params}, lambda_{id}_bg[i]));
        }

        idx_pfs = idx_pfs + n_pfs[Tx];
      }
      log_lik = log_lik_os + log_lik_pfs;\n\n")

  scode
}

