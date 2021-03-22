// exponential mixture cure model
// joint relative survival


functions {
  #include /include/distributions.stan
}

data {
  int<lower=1> nTx;
  int<lower=0> n_os[nTx];         // number of observations
  int<lower=0> n_pfs[nTx];
  int<lower=0> H_os;             // number of covariates
  int<lower=0> H_pfs;

  matrix[n_os, nTx] t_os;        // observation times
  matrix[n_pfs, nTx] t_pfs;

  matrix[n_os, nTx] d_os;             // censoring indicator (1 = observed, 0 = censored)
  matrix[n_pfs, nTx] d_pfs;

  matrix[n_os, H_os, nTx] X_os;       // matrix of covariates (with n rows and H columns)
  matrix[n_pfs, H_pfs, nTx] X_pfs;

  vector[H_os] mu_0_os;
  vector[H_pfs] mu_0_pfs;
  vector<lower=0> [H_os] sigma_0_os;
  vector<lower=0> [H_pfs] sigma_0_pfs;

  //TODO: what to do when different type/number covariates for os and pfs?
  int<lower=1, upper=2> bg_model;
  vector[bg_model == 1 ? H_os : 0] mu_bg;
  vector<lower=0>[bg_model == 1 ? H_os : 0] sigma_bg;
  vector[bg_model == 2 ? n_os : 0] h_bg_os;
  vector[bg_model == 2 ? n_pfs : 0] h_bg_pfs;

  int<lower=0, upper=1> joint_model;
  real mu_joint[joint_model];
  real<lower=0> sigma_joint[joint_model];

  int<lower=1, upper=3> cf_model;         // cure fraction
  real mu_cf_gl[cf_model == 3 ? 1 : 0];   // 1- shared; 2- separate; 3- hierarchical
  real mu_cf_os[cf_model == 2 ? 1 : 0];
  real mu_cf_pfs[cf_model == 2 ? 1 : 0];
  real<lower=0> sigma_cf_gl[cf_model == 3 ? 1 : 0];
  real<lower=0> sd_cf_os[cf_model != 1 ? 1 : 0];
  real<lower=0> sd_cf_pfs[cf_model != 1 ? 1 : 0];
  real a_cf[cf_model == 1 ? 1 : 0];
  real b_cf[cf_model == 1 ? 1 : 0];

  int<lower=0> t_max;
  matrix[nTx, nTx] Tx_flag;
}

parameters {
  vector[H_os] beta_os;       // coefficients in linear predictor (including intercept)
  vector[H_pfs] beta_pfs;
  vector[bg_model == 1 ? H_os : 0] beta_bg;
  real beta_joint[joint_model];

  real alpha0;
  vector[nTx] alpha;

  real<lower=0, upper=1> cf_pooled[cf_model == 1 ? 1 : 0];
  real lp_cf_global[cf_model == 3 ? 1 : 0];
  real lp_cf_os[cf_model != 1 ? 1 : 0];
  real lp_cf_pfs[cf_model != 1 ? 1 : 0];
}

transformed parameters {
  matrix[n_os, nTx] lp_os;
  matrix[n_pfs, nTx] lp_pfs;
  matrix[n_os, nTx] lp_os_bg;
  matrix[n_os, nTx] lp_pfs_bg;

  matrix[n_os, nTx] lambda_os;
  matrix[n_pfs, nTx] lambda_pfs;
  matrix[n_os, nTx] lambda_os_bg;
  matrix[n_os, nTx] lambda_pfs_bg;

  real<lower=0, upper=1> cf_global[cf_model == 3 ? 1 : 0];
  real<lower=0, upper=1> cf_os;
  real<lower=0, upper=1> cf_pfs;

  lp_os[Tx] = X_os[,,Tx]*beta_os;
  lp_pfs[Tx] = X_pfs[,,Tx]*beta_pfs;

  if (bg_model == 1) {         // background survival with uncertainty
  lp_os_bg[Tx] = X_os[,,Tx]*beta_bg;
  lp_pfs_bg[Tx] = X_pfs[,,Tx]*beta_bg;
  } else {
    lp_os_bg = log(h_bg_os);
    lp_pfs_bg = log(h_bg_pfs);
  }

  lambda_os_bg = exp(lp_os_bg);
  lambda_pfs_bg = exp(lp_pfs_bg);

  // rate parameters
  lambda_os = exp(lp_os);
  lambda_pfs = exp(lp_pfs);

  if (cf_model == 3) {
    cf_global = inv_logit(lp_cf_global);
  }
  if (cf_model != 1) {
    cf_os = inv_logit(lp_cf_os[1]);
    cf_pfs = inv_logit(lp_cf_pfs[1]);
  } else {
    cf_os = cf_pooled[1];
    cf_pfs = cf_pooled[1];
  }
}

model {
  beta_os ~ normal(mu_0_os, sigma_0_os);
  beta_pfs ~ normal(mu_0_pfs, sigma_0_pfs);

  if (bg_model == 1) {
    beta_bg ~ normal(mu_bg, sigma_bg);
  }

  if (joint_model) {
    beta_joint ~ normal(mu_joint, sigma_joint);
  }

  // cure fraction
  if (cf_model == 3) {
    mu_cf_gl = alpha0 + alpha*Tx_flag

    lp_cf_global ~ normal(mu_cf_gl, sigma_cf_gl);
    lp_cf_os ~ normal(lp_cf_global, sd_cf_os);
    lp_cf_pfs ~ normal(lp_cf_global, sd_cf_pfs);

  } else if (cf_model == 2) {
    lp_cf_os ~ normal(mu_cf_os, sd_cf_os);
    lp_cf_pfs ~ normal(mu_cf_pfs, sd_cf_pfs);
  } else {
    cf_pooled ~ beta(a_cf, b_cf);
  }

  // likelihood
  for (i in 1:n_os) {
    target += log_sum_exp(
      log(cf_os[Tx]) +
      surv_exp_lpdf(t_os[i, Tx] | d_os[i, Tx], lambda_os_bg[i, Tx]),
      log1m(cf_os[Tx]) +
      surv_exp_lpdf(t_os[i, Tx] | d_os[i, Tx], lambda_os_bg[i, Tx] + lambda_os[i, Tx])) +
      log_sum_exp(
        log(cf_pfs[Tx]) +
        surv_exp_lpdf(t_pfs[i, Tx] | d_pfs[i, Tx], lambda_pfs_bg[i, Tx]),
        log1m(cf_pfs[Tx]) +
        surv_exp_lpdf(t_pfs[i, Tx] | d_pfs[i, Tx], lambda_pfs_bg[i, Tx] + lambda_pfs[i, Tx]));
  }
}

generated quantities {
  // // posterior
  // real mean_os;
  // real mean_pfs;
  // real mean_bg;
  //
  // vector[t_max] S_bg;
  // vector[t_max] S_os;
  // vector[t_max] S_pfs;
  // vector[t_max] S_os_pred;
  // vector[t_max] S_pfs_pred;
  //
  // // prior pred
  // real pmean_os;
  // real pmean_pfs;
  // real pmean_bg;
  // real pmean_cf_os;
  // real pmean_cf_pfs;
  //
  // vector[t_max] pS_bg;
  // vector[t_max] pS_os;
  // vector[t_max] pS_pfs;
  // vector[t_max] S_os_prior;
  // vector[t_max] S_pfs_prior;
  //
  // vector[n_os] log_lik;
  //
  // real pbeta_os = normal_rng(mu_0_os[1], sigma_0_os[1]);
  // real pbeta_pfs = normal_rng(mu_0_pfs[1], sigma_0_pfs[1]);
  //
  // real pbeta_bg;
  //
  // // if (cf_model == 3) {
    //   // real vpc_os;
    //   // real vpc_pfs;
    //   //
    //   // vpc_os = sd_cf_os/(sigma_cf_gl + sd_cf_os);
    //   // vpc_pfs = sd_cf_pfs/(sigma_cf_gl + sd_cf_pfs);
    // // }
    //
    // if (bg_model == 1) {
      //   pbeta_bg = normal_rng(mu_bg[1], sigma_bg[1]);
      // } else {
        //   // pbeta_bg = log(mean(h_bg_os));
        //   pbeta_bg = log(0.001);
        // }
        //
        // // cure fraction prior
        // if (cf_model == 3) {
          //   //TODO: include extra sd_cf_os, sd_cf_pfs variation?
          //   real pcurefrac = normal_rng(mu_cf_gl[1], sigma_cf_gl[1]);
          //   pmean_cf_os = inv_logit(pcurefrac);
          //   pmean_cf_pfs = inv_logit(pcurefrac);
          // } else if (cf_model == 2) {
            //   real pcf_os = normal_rng(mu_cf_os[1], sd_cf_os[1]);
            //   real pcf_pfs = normal_rng(mu_cf_pfs[1], sd_cf_pfs[1]);
            //   pmean_cf_os = inv_logit(pcf_os);
            //   pmean_cf_pfs = inv_logit(pcf_pfs);
            // } else {
              //   real pcurefrac = beta_rng(a_cf[1], b_cf[1]);
              //   pmean_cf_os = pcurefrac;
              //   pmean_cf_pfs = pcurefrac;
              // }
              //
              // // intercepts
              // mean_os = exp(beta_os[1]);
              // mean_pfs = exp(beta_pfs[1]);
              //
              // //TODO: this is a short-term hack
              // if (bg_model == 1) {
                //   mean_bg = exp(beta_bg[1]);
                // } else {
                  //   mean_bg = 0.001;
                  //   // mean_bg = mean(h_bg_os);
                  // }
                  //
                  // for (i in 1:t_max) {
                    //   S_bg[i] = exp_Surv(i, mean_bg);
                    //   S_os[i] = exp_Surv(i, mean_bg + mean_os);
                    //   S_pfs[i] = exp_Surv(i, mean_bg + mean_pfs);
                    //
                    //   S_os_pred[i] = cf_os*S_bg[i] + (1 - cf_os)*S_os[i];
                    //   S_pfs_pred[i] = cf_pfs*S_bg[i] + (1 - cf_pfs)*S_pfs[i];
                    // }
                    //
                    // // prior checks
                    // pmean_os = exp(pbeta_os);
                    // pmean_pfs = exp(pbeta_pfs);
                    // pmean_bg = exp(pbeta_bg);
                    //
                    // for (i in 1:t_max) {
                      //   pS_bg[i] = exp_Surv(i, pmean_bg);
                      //   pS_os[i] = exp_Surv(i, pmean_bg + pmean_os);
                      //   pS_pfs[i] = exp_Surv(i, pmean_bg + pmean_pfs);
                      //
                      //   S_os_prior[i] = pmean_cf_os*pS_bg[i] + (1 - pmean_cf_os)*pS_os[i];
                      //   S_pfs_prior[i] = pmean_cf_pfs*pS_bg[i] + (1 - pmean_cf_pfs)*pS_pfs[i];
                      // }
                      //
                      // // log-likelihood for loo
                      // // http://mc-stan.org/loo/reference/extract_log_lik.html
                      //
                      // for (n in 1:n_os) {
                        //   log_lik[n] = log_sum_exp(
                          //                  log(cf_os) +
                          //                   surv_exp_lpdf(t_os[n] | d_os[n], lambda_os_bg[n]),
                          //                 log1m(cf_os) +
                          //                   surv_exp_lpdf(t_os[n] | d_os[n], lambda_os_bg[n] + lambda_os[n])) +
                          //               log_sum_exp(
                            //                 log(cf_pfs) +
                            //                   surv_exp_lpdf(t_pfs[n] | d_pfs[n], lambda_pfs_bg[n]),
                            //                 log1m(cf_pfs) +
                            //                   surv_exp_lpdf(t_pfs[n] | d_pfs[n], lambda_pfs_bg[n] + lambda_pfs[n]));
                            // }

}

