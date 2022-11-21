# Stan code blocks for create_stancode() ----------------------------------
# all treatments in single model


#
distn_params <- function(distn) {
  switch(distn,
         exp = "lambda",
         weibull = c("shape", "lambda"),
         gompertz = c("shape", "lambda"),
         loglogistic = c("shape", "lambda"),
         lognormal = c("mu", "sd"),
         gengamma = "")
}

#
mean_params <- function(distn) {
  switch(distn,
         exp = "mean",
         weibull = c("shape", "mean"),
         gompertz = c("shape", "mean"),
         loglogistic = c("shape", "mean"),
         lognormal = c("mean", "sd"),
         gengamma = "")
}

#' make_latent_model_code
#'
#' @examples
#' lapply(make_latent_model_code("lognormal"), cat)
#'
make_latent_model_code <- function(model, id = 1L) {

  scode <- list()

  scode$data_def <-
    common_code_event_data(id)

  scode$trans_params_def <-
    glue("\nvector[N_{id}] lp_{id};\n")

  if (model == "exp") {
    scode$trans_params_def <-
      glue(scode$trans_params_def,
           "\n// rate parameters
             vector<lower=0>[N_{id}] lambda_{id};\n")

    scode$trans_params_main <-
      glue("\nlambda_{id} = exp(lp_{id});\n")

    scode$generated_quantities_main <-
      glue("mean_{id} = exp(beta_{id}[1]);\n")
  }

  if (model %in% c("gompertz", "loglogistic", "weibull")) {
    scode$data_def <-
      glue(scode$data_def,
           "real<lower=0> a_shape_{id};\n",
           "real<lower=0> b_shape_{id};\n")

    scode$parameters <-
      glue("real<lower=0> shape_{id};\n")

    scode$trans_params_def <-
      glue(scode$trans_params_def,
           "// rate parameters
            vector[N_{id}] lambda_{id};\n")

    scode$trans_params_main <-
      glue("lambda_{id} = exp(lp_{id});\n")

    scode$model <-
      glue("shape_{id} ~ gamma(a_shape_{id}, b_shape_{id});\n")

    scode$generated_quantities_def <-
      glue("real pshape_{id} = gamma_rng(a_shape_{id}, b_shape_{id});\n")
    scode$generated_quantities_main <-
      glue("mean_{id} = exp(beta_{id}[1]);\n")
  }

  if (model == "lognormal") {
    scode$data_def <-
      glue(scode$data_def,
           "real<lower=0> a_sd_{id};    // gamma hyper-parameters
              real<lower=0> b_sd_{id};\n")

    scode$parameters <-
      glue("real<lower=0> sd_{id};\n")

    scode$trans_params_def <-
      glue(scode$trans_params_def,
           "// rate parameters
            vector[N_{id}] mu_{id};\n")

    scode$trans_params_main <-
      glue("mu_{id} = lp_{id};\n")

    scode$model <-
      glue("sd_{id} ~ gamma(a_sd_{id}, b_sd_{id});\n")

    scode$generated_quantities_def <-
      glue("real psd_{id} = gamma_rng(a_sd_{id}, b_sd_{id});\n")

    scode$generated_quantities_main <-
      glue("mean_{id} = beta_{id}[1];\n")
  }

  scode
}


#
create_cf_code <- function(n_grp) {

  scode <- list()
  ids <- data.frame(id = 1:n_grp)

  scode$data_def <-
    c("int<lower=1, upper=3> cf_model;         // cure fraction\n")

  scode$data_main <-
    paste0("\n real<lower=0> a_cf[cf_model == 1 ? 1 : 0];\n",
           "real<lower=0> b_cf[cf_model == 1 ? 1 : 0];\n",
           "vector[cf_model == 3 ? nTx : 0] mu_sd_cf;\n",
           "vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_sd_cf;\n")

  scode$parameters <-
    paste0("\n vector<lower=0, upper=1>[cf_model == 1 ? nTx : 0] cf_pooled;\n",
           cglue_data(ids, "vector[cf_model == 3 ? nTx : 0] lp_cf_{id};\n"),
           "\n vector<lower=0>[cf_model == 3 ? nTx : 0] sd_cf;\n")

  scode$trans_params_def <-
    paste0("\n vector<lower=0, upper=1>[cf_model == 3 ? nTx : 0] cf_global;\n",
           cglue_data(ids,
                      "vector<lower=0, upper=1>[nTx] cf_{id};
                     vector[cf_model == 2 ? nTx : 0] tx_cf_{id};\n"),
           "\n vector[cf_model == 3 ? nTx : 0] lp_cf_global;\n")

  scode$trans_params_main <-
    paste0(c("\n if (cf_model == 1) {\n"),
           cglue_data(ids, "cf_{id} = cf_pooled;"),
           "\n}\n",
           "if (cf_model == 3) {\n",
           "lp_cf_global = Tx_dmat*alpha;\n",
           "cf_global = inv_logit(lp_cf_global);\n",
           cglue_data(ids, "cf_{id} = inv_logit(lp_cf_{id});"),
           "\n}\n",
           "if (cf_model == 2) {\n",
           cglue_data(ids, "tx_cf_{id} = Tx_dmat*alpha_{id};
                      cf_{id} = inv_logit(tx_cf_{id});"),
           "\n}\n")

  scode$model <-
    paste0(paste("// cure fraction \n if (cf_model == 3) {\n",
                 "alpha ~ normal(mu_alpha, sigma_alpha);\n",
                 "sd_cf ~ normal(mu_sd_cf, sigma_sd_cf);\n", collapse = "n"),
           cglue_data(ids, "lp_cf_{id} ~ normal(lp_cf_global, sd_cf);\n"),
           "\n} else if (cf_model == 2) {\n",
           cglue_data(ids, "alpha_{id} ~ normal(mu_alpha_{id}, sigma_alpha_{id});\n"),
           "\n} else {\n",
           "cf_pooled ~ beta(a_cf, b_cf);\n",
           "}\n")

  scode$generated_quantities <-
    c("\n")    ##TODO: what is this for? just so its not empty?

  scode
}

#
create_code_skeleton <- function(n_grp) {

  scode <- list()
  ids <- data.frame(id = 1:n_grp)

  scode$data_def <-
    c(" int<lower=1> nTx;\n")

  scode$data_main <-
    paste0(
      c("\nint<lower=1, upper=2> bg_model;\n
        vector[bg_model == 1 ? H_1 : 0] mu_bg;\n
        vector<lower=0>[bg_model == 1 ? H_1 : 0] sigma_bg;\n"),
      cglue_data(ids, " vector<lower=0>[bg_model == 2 ? N_{id} : 0] h_bg_{id};\n"),
      c("\n matrix[nTx, nTx] Tx_dmat;         // treatment design matrix\n
      vector[cf_model == 3 ? nTx : 0] mu_alpha;             // treatment regression coefficients\n
      vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_alpha;\n
      int<lower=0> t_max;\n"),
      cglue_data(ids,
                 "vector[cf_model == 2 ? nTx : 0] mu_alpha_{id};
                 vector<lower=0>[cf_model == 2 ? nTx : 0] sigma_alpha_{id};\n"))

  scode$parameters <-
    paste0(c("// coefficients in linear predictor (including intercept)\n
      vector[bg_model == 1 ? H_1 : 0] beta_bg;\n
      vector[cf_model != 2 ? nTx : 0] alpha;\n"),
      cglue_data(ids, "vector[cf_model == 2 ? nTx : 0] alpha_{id};
                      vector[H_{id}] beta_{id};\n"), collapse = "\n")

  scode$trans_params_def <-
    cglue_data(ids,
               "vector[N_{id}] lp_{id}_bg;\n
                vector<lower=0>[N_{id}] lambda_{id}_bg;\n
                ")

  scode$trans_params_main <-
    cglue_data(ids,
               "// correlated event times
        lp_{id} = X_{id}*beta_{id};

      // background survival with uncertainty\n
      if (bg_model == 1) {{
        lp_{id}_bg = X_{id}*beta_bg;
      } else {{
        lp_{id}_bg = log(h_bg_{id});
      }\n
      lambda_{id}_bg = exp(lp_{id}_bg);\n
      ")

  scode$model <-
    paste0(
      cglue_data(ids, "\nint idx_{id};"),
      cglue_data(ids,"
      \nbeta_{id} ~ normal(mu_S_{id}, sigma_S_{id});\n"),
      c("\n if (bg_model == 1) {
        beta_bg ~ normal(mu_bg, sigma_bg);
      }\n"), collapse = "\n")

  scode$generated_quantities_def <-
    paste0(c("real mean_bg;\n
           // real pbeta_bg;\n
           real log_lik = 0;\n
           vector[t_max] S_bg;\n"),
           cglue_data(ids,
                      "vector[t_max] S_{id};
      matrix[t_max, nTx] S_{id}_pred;
      real mean_{id};
      int idx_{id};
      real log_lik_{id};
      // real pbeta_{id} = normal_rng(mu_S_{id}[1], sigma_S_{id}[1]);
      \n\n"))

  scode$generated_quantities_main <-
    paste0(c("// background rate\n
           if (bg_model == 1) {\n
           mean_bg = exp(beta_bg[1]);\n
           } else {\n
           // mean_bg = 0.001;\n"),
           cglue_data(ids, "mean_bg = mean(h_bg_{id});"),
           "\n}\n")

  scode
}


#' @importFrom glue glue
#'
make_loglik <- function(model, id) {

  ll_params <- glue("{distn_params(model)}_{id}")

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
          {surv_lpdf}(t_{id}[i] | d_{id}[i], {ll_params}, lambda_{id}_bg[i]));
      }

      idx_{id} = idx_{id} + n_{id}[Tx];
    }\n")

  scode
}

# from brms package
tp <- function(wsp = 2) {
  wsp <- glue_collapse(rep(" ", wsp))
  paste0(wsp, "target += ")
}

#
common_code_event_data <- function(id) {
  glue(
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

  params <-
    paste0(mean_params(model), "_{id}") |>
    paste(collapse = ", ")

  glue(
    "// posterior mean checks
    for (j in 1:nTx) {{
      for (i in 1:t_max) {{
        S_bg[i] = exp_Surv(i, mean_bg);
        S_{id}[i] = exp_{model}_Surv(i, ", params, ", mean_bg);
        S_{id}_pred[i, j] = cf_{id}[j]*S_bg[i] + (1 - cf_{id}[j])*S_{id}[i];
      }
    }\n\n")
}

#
make_priorpred <- function(model, id) {

  pp_params <- paste0(distn_params(model), "_{id}")
  pp_params <- paste(pp_params, collapse = ", ")

  glue(
    "// prior mean checks
    // pmean_pfs = exp(pbeta_pfs);
    // pmean_bg = exp(pbeta_bg);

    // for (i in 1:t_max) {{
    //  pS_bg[i] = exp_Surv(i, pmean_bg);
    //  pS_{id}[i] = exp_{model}_Surv(i, ", pp_params, "[j], pmean_bg);
    //  S_{id}_prior[i] = pmean_cf_{id}*pS_bg[i] + (1 - pmean_cf_{id})*pS_{id}[i,j];
    // }\n\n")
}


##TODO: remove duplication with make_loglik()
make_loo <- function(model, id) {

  loo_params <- glue(distn_params(model), "_{id}")

  loo_params[grep("mu", loo_params)] <-
    paste0(loo_params[grep("mu", loo_params)], "[i]")

  loo_params[grep("lambda", loo_params)] <-
    paste0(loo_params[grep("lambda", loo_params)], "[i]")

  loo_params <- paste(loo_params, collapse = ", ")

  surv_lpdf <- glue("joint_exp_{model}_lpdf")

  scode <-
    glue("
    // likelihood
      idx_{id} = 1;

      for (Tx in 1:nTx) {{
        for (i in idx_{id}:(idx_{id} + n_{id}[Tx] - 1)) {{
          log_lik_{id} += log_sum_exp(
          log(cf_{id}[Tx]) +
          surv_exp_lpdf(t_{id}[i] | d_{id}[i], lambda_{id}_bg[i]),
          log1m(cf_{id}[Tx]) +
          {surv_lpdf}(t_{id}[i] | d_{id}[i], {loo_params}, lambda_{id}_bg[i]));
        }

        idx_{id} = idx_{id} + n_{id}[Tx];
      }
      log_lik = log_lik + log_lik_{id};\n\n")

  scode
}

