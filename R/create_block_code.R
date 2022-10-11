# Stan code blocks for create_stancode() ----------------------------------
# all treatments in single model


#' @examples
#' lapply(create_latent_model_code("lognormal"), cat)
#'
create_latent_model_code <- function(model, id = 1L) {

  scode <- list()

  scode$data <-
    common_code_event_data()

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
create_cf_code <- function() {

  scode <- list()

  scode$data <- list(
    def =
      c("\t int<lower=1, upper=3> cf_model;         // cure fraction\n"),
    main =
      c("real<lower=0> a_cf[cf_model == 1 ? 1 : 0];
      real<lower=0> b_cf[cf_model == 1 ? 1 : 0];
      vector[cf_model == 3 ? nTx : 0] mu_sd_cf;
      vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_sd_cf;\n"))

  scode$parameters <-
    glue("\tvector<lower=0, upper=1>[cf_model == 1 ? nTx : 0] cf_pooled;


         ##TODO: loop and then glue text
         for (i in 1:n_group) {
            vector[cf_model == 3 ? nTx : 0] lp_cf_{id};
         }


      vector<lower=0>[cf_model == 3 ? nTx : 0] sd_cf;\n")

  scode$trans_params <- list(
    def =
      c("\tvector<lower=0, upper=1>[cf_model == 3 ? nTx : 0] cf_global;


        ##TODO: loop here too...
        vector<lower=0, upper=1>[nTx] cf_{id};
        vector[cf_model == 2 ? nTx : 0] tx_cf_{id};


        vector[cf_model == 3 ? nTx : 0] lp_cf_global;\n"),
    main =
      c("if (cf_model == 3) {
        lp_cf_global = Tx_dmat*alpha;
        cf_global = inv_logit(lp_cf_global);


        ##TODO: loop
        cf_{id} = inv_logit(lp_cf_{id});



    }\n
      if (cf_model == 2) {
        tx_cf_{id} = Tx_dmat*alpha_{id};
        cf_{id} = inv_logit(tx_cf_{id});
      }\n
      if (cf_model == 1) {
        cf_{id} = cf_pooled;
      }\n"))

  scode$model <-
    c("\t// cure fraction
      if (cf_model == 3) {
        alpha ~ normal(mu_alpha, sigma_alpha);
        sd_cf ~ normal(mu_sd_cf, sigma_sd_cf);
        lp_cf_{id} ~ normal(lp_cf_global, sd_cf);
      } else if (cf_model == 2) {
        alpha_{id} ~ normal(mu_alpha_{id}, sigma_alpha_{id});
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

      vector<lower=0>[bg_model == 2 ? N_{id} : 0] h_bg_{id};

      matrix[nTx, nTx] Tx_dmat;         // treatment design matrix
      vector[cf_model == 3 ? nTx : 0] mu_alpha;             // treatment regression coefficients
      vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_alpha;
      vector[cf_model == 2 ? nTx : 0] mu_alpha_{id};
      vector<lower=0>[cf_model == 2 ? nTx : 0] sigma_alpha_{id};
      int<lower=0> t_max;\n"))

  scode$parameters <-
    c("\tvector[H_{id}] beta_{id};       // coefficients in linear predictor (including intercept)
      vector[bg_model == 1 ? H_{id} : 0] beta_bg;
      vector[cf_model != 2 ? nTx : 0] alpha;
      vector[cf_model == 2 ? nTx : 0] alpha_{id};\n")

  scode$trans_params <- list(
    def =
      c("\tvector[N_{id}] lp_{id}_bg;

      vector<lower=0>[N_{id}] lambda_{id}_bg;\n"),

    main =
      c("// correlated event times
        lp_{id} = X_{id}*beta_{id};

      // background survival with uncertainty\n
      if (bg_model == 1) {
        lp_{id}_bg = X_{id}*beta_bg;
      } else {
        lp_{id}_bg = log(h_bg_{id});
      }\n
      lambda_{id}_bg = exp(lp_{id}_bg);\n"))

  scode$model <-
    c("\t int idx_{id};

      beta_{id} ~ normal(mu_S_{id}, sigma_S_{id});\n
      if (bg_model == 1) {
        beta_bg ~ normal(mu_bg, sigma_bg);
      }\n")

  scode$generated_quantities$def <-
    c("\t real mean_{id};
      real mean_bg;

      vector[t_max] S_bg;
      vector[t_max] S_{id};
      matrix[t_max, nTx] S_{id}_pred;

      int idx_{id};

      vector[N_{id}] log_lik_{id};
      vector[N_{id}] log_lik;

      // real pbeta_{id} = normal_rng(mu_S_{id}[1], sigma_S_{id}[1]);

      // real pbeta_bg;\n\n")

  scode$generated_quantities$main <-
    c("// background rate
      if (bg_model == 1) {
        mean_bg = exp(beta_bg[1]);
      } else {
      // mean_bg = 0.001;
        mean_bg = mean(h_bg_{id});
      }\n")

  scode
}


#' @importFrom glue glue
#'
make_loglik <- function(model) {

  distn_params <-
    list(exponential = "lambda",
         weibull = c("shape", "lambda"),
         gompertz = c("shape", "lambda"),
         loglogistic = c("shape", "lambda"),
         lognormal = c("mu", "sd"),
         gengamma = "")

  ll_params <- glue("{distn_params[[ll_params]]}_{id}")

  ll_params[grep("mu", ll_params)] <-
    paste0(os_params[grep("mu", ll_params)], "[i]")

  ll_params[grep("lambda", ll_params)] <-
    paste0(os_params[grep("lambda", ll_params)], "[i]")

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
common_code_event_data <- function(e) {
  glue(
    " int<lower=0> N_{e};\n",
    " int<lower=0> n_{e}[nTx];\n",
    " int<lower=0> H_{e};\n",
    " vector<lower=0>[N_{e}] t_{e};\n",
    " vector<lower=0, upper=1>[N_{e}] d_{e};\n",
    " matrix[N_{e}, H_{e}] X_{e};\n",
    " vector[H_{e}] mu_S_{e};\n",
    " vector<lower=0>[H_{e}] sigma_S_{e};\n\n")
}

#
make_postpred <- function(model) {

  distn_params <-
    list(exponential = "mean",
         weibull = c("shape", "mean"),
         gompertz = c("shape", "mean"),
         loglogistic = c("shape", "mean"),
         lognormal = c("mean", "sd"),
         gengamma = "")

  pp_params <- paste0(distn_params[[pp_model]], "_{id}")
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
make_priorpred <- function(model) {

  distn_params <-
    list(exponential = "pmean",
         weibull = c("pshape", "pmean"),
         gompertz = c("pshape", "pmean"),
         loglogistic = c("pshape", "pmean"),
         lognormal = c("pmean", "psd"),
         gengamma = "")

  pp_params <- paste0(distn_params[[pp_model]], "_{id}")
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
make_loo <- function(model) {

  distn_params <-
    list(exponential = "lambda",
         weibull = c("shape", "lambda"),
         gompertz = c("shape", "lambda"),
         loglogistic = c("shape", "lambda"),
         lognormal = c("mu", "sd"),
         gengamma = "")

  os_params <- paste0(distn_params[[os_model]], "_os")

  os_params[grep("mu", os_params)] <-
    paste0(os_params[grep("mu", os_params)], "[i]")

  os_params[grep("lambda", os_params)] <-
    paste0(os_params[grep("lambda", os_params)], "[i]")

  os_params <- paste(os_params, collapse = ", ")

  surv_lpdf_os <- glue("joint_exp_{os_model}_lpdf")

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
          {surv_lpdf_{id}}(t_{id}[i] | d_{id}[i], {os_params}, lambda_{id}_bg[i]));
        }

        idx_pfs = idx_pfs + n_pfs[Tx];
      }
      log_lik = log_lik_os + log_lik_pfs;\n\n")

  scode
}

