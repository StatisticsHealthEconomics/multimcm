// exponential mixture cure model
// joint relative survival


functions {
#include /include/distributions.stan
}

// input data ----
data {
  int<lower=0> n_os;             // number of observations
  int<lower=0> n_pfs;
  int<lower=0> H_os;             // number of covariates
  int<lower=0> H_pfs;

  // vector<lower=t_pfs> [n_os] t_os;
  vector[n_os] t_os;             // observation times
  vector[n_pfs] t_pfs;

  vector[n_os] d_os;             // censoring indicator (1 = observed, 0 = censored)
  vector[n_pfs] d_pfs;

  matrix[n_os, H_os] X_os;       // matrix of covariates (with n rows and H columns)
  matrix[n_pfs, H_pfs] X_pfs;

  real<lower=0> a_sd_pfs;        // gamma hyper-parameters
  real<lower=0> b_sd_pfs;
  real<lower=0> a_sd_os;
  real<lower=0> b_sd_os;

  vector[H_os] mu_0_os;          // mean linear predictor os, pfs
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
}

parameters {
  vector[H_os] beta_os;       // coefficients in linear predictor (including intercept)
  vector[H_pfs] beta_pfs;
  vector[bg_model == 1 ? H_os : 0] beta_bg;
  real beta_joint[joint_model];

  real<lower=0> sd_pfs;
  real<lower=0> sd_os;

  real<lower=0, upper=1> cf_pooled[cf_model == 1 ? 1 : 0];
  real lp_cf_global[cf_model == 3 ? 1 : 0];
  real lp_cf_os[cf_model != 1 ? 1 : 0];
  real lp_cf_pfs[cf_model != 1 ? 1 : 0];
}

transformed parameters {
  vector[n_os] lp_os;
  vector[n_pfs] lp_pfs;
  vector[n_os] lp_os_bg;
  vector[n_os] lp_pfs_bg;

  vector[n_os] mean_os;
  vector[n_pfs] mean_pfs;
  vector[n_os] lambda_os_bg;
  vector[n_os] lambda_pfs_bg;

  real<lower=0, upper=1> cf_global[cf_model == 3 ? 1 : 0];
  real<lower=0, upper=1> cf_os;
  real<lower=0, upper=1> cf_pfs;

  # correlated event times
  if (joint_model) {
    lp_os = X_os*beta_os + beta_joint[1]*(t_pfs - 1/exp(beta_pfs[1]));
  } else {
    lp_os = X_os*beta_os;
  }

  lp_pfs = X_pfs*beta_pfs;

  if (bg_model == 1) {         // background survival with uncertainty
    lp_os_bg = X_os*beta_bg;
    lp_pfs_bg = X_pfs*beta_bg;
  } else {
    lp_os_bg = log(h_bg_os);
    lp_pfs_bg = log(h_bg_pfs);
  }

  lambda_os_bg = exp(lp_os_bg);
  lambda_pfs_bg = exp(lp_pfs_bg);

  mean_os = lp_os;
  mean_pfs = lp_pfs;

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

  sd_pfs ~ gamma(a_sd_pfs, b_sd_pfs);
  sd_os ~ gamma(a_sd_os, b_sd_os);

  if (bg_model == 1) {
    beta_bg ~ normal(mu_bg, sigma_bg);
  }

  if (joint_model) {
    beta_joint ~ normal(mu_joint, sigma_joint);
  }

  // cure fraction
  if (cf_model == 3) {
    lp_cf_global ~ normal(mu_cf_gl, sigma_cf_gl);
    lp_cf_os ~ normal(lp_cf_global, sd_cf_os);
    lp_cf_pfs ~ normal(lp_cf_global, sd_cf_pfs);
  } else if (cf_model == 2) {
    lp_cf_os ~ normal(mu_cf_os, sd_cf_os);
    lp_cf_pfs ~ normal(mu_cf_pfs, sd_cf_pfs);
  } else {
    cf_pooled ~ beta(a_cf, b_cf);
  }

  //TODO: could just use surv point estimate directly for life-table background?
  //      instead of via exp distn

  // likelihood
  for (i in 1:n_os) {
    target += log_sum_exp(
                log(cf_os) +
                surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
                log1m(cf_os) +
                joint_exp_lognormal_lpdf(t_os[i] | d_os[i], mean_os[i], sd_os, lambda_os_bg[i])) +
              log_sum_exp(
                log(cf_pfs) +
                surv_exp_lpdf(t_pfs[i] | d_pfs[i], lambda_pfs_bg[i]),
                log1m(cf_pfs) +
                joint_exp_lognormal_lpdf(t_pfs[i] | d_pfs[i], mean_pfs[i], sd_pfs, lambda_pfs_bg[i]));
  }
}

generated quantities {
  // posterior
  real mean_lp_os;
  real mean_lp_pfs;
  real mean_lp_bg;

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

  real psd_pfs = gamma_rng(a_sd_pfs, b_sd_pfs);
  real psd_os = gamma_rng(a_sd_os, b_sd_os);

  real pbeta_bg;

  real<lower=0, upper=1> cf_tilde[n_os] ;
  real<lower=0> t_os_tilde[n_os];
  real<lower=0> t_pfs_tilde[n_os];

  // if (cf_model == 3) {
    // real vpc_os;
    // real vpc_pfs;
    //
    // vpc_os = sd_cf_os/(sigma_cf_gl + sd_cf_os);
    // vpc_pfs = sd_cf_pfs/(sigma_cf_gl + sd_cf_pfs);
  // }

  if (bg_model == 1) {
    pbeta_bg = normal_rng(mu_bg[1], sigma_bg[1]);
  } else {
    // pbeta_bg = log(mean(h_bg_os));
    pbeta_bg = log(0.001);
  }

  // cure fraction prior
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
  }

  // intercepts
  mean_lp_os = beta_os[1];
  mean_lp_pfs = beta_pfs[1];

  //TODO: this is a short-term hack
  if (bg_model == 1) {
    mean_lp_bg = exp(beta_bg[1]);
  } else {
    mean_lp_bg = 0.001;
    // mean_bg = mean(h_bg_os);
  }

  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, mean_lp_bg);
    S_os[i] = exp_lognormal_Surv(i, mean_lp_os, sd_os, mean_lp_bg);
    S_pfs[i] = exp_lognormal_Surv(i, mean_lp_pfs, sd_pfs, mean_lp_bg);

    S_os_pred[i] = cf_os*S_bg[i] + (1 - cf_os)*S_os[i];
    S_pfs_pred[i] = cf_pfs*S_bg[i] + (1 - cf_pfs)*S_pfs[i];
  }

  // prior checks
  pmean_os = pbeta_os;
  pmean_pfs = pbeta_pfs;
  pmean_bg = exp(pbeta_bg);

  for (i in 1:t_max) {
    pS_bg[i] = exp_Surv(i, pmean_bg);
    pS_os[i] = exp_lognormal_Surv(i, pmean_os, psd_os, pmean_bg);
    pS_pfs[i] = exp_lognormal_Surv(i, pmean_pfs, psd_pfs, pmean_bg);

    S_os_prior[i] = pmean_cf_os*pS_bg[i] + (1 - pmean_cf_os)*pS_os[i];
    S_pfs_prior[i] = pmean_cf_pfs*pS_bg[i] + (1 - pmean_cf_pfs)*pS_pfs[i];
  }

  // log-likelihood for loo
  // http://mc-stan.org/loo/reference/extract_log_lik.html

  for (n in 1:n_os) {
    log_lik[n] = log_sum_exp(
                   log(cf_os) +
                    surv_exp_lpdf(t_os[n] | d_os[n], lambda_os_bg[n]),
                  log1m(cf_os) +
                    joint_exp_lognormal_lpdf(t_os[n] | d_os[n], mean_os[n], sd_os, lambda_os_bg[n])) +
                log_sum_exp(
                  log(cf_pfs) +
                    surv_exp_lpdf(t_pfs[n] | d_pfs[n], lambda_pfs_bg[n]),
                  log1m(cf_pfs) +
                    joint_exp_lognormal_lpdf(t_pfs[n] | d_pfs[n], mean_pfs[n], sd_pfs, lambda_pfs_bg[n]));
  }


  // posterior predictive values
  for (i in 1:n_os) {

    cf_tilde[i] = uniform_rng(0, 1);

    if (cf_tilde[i] < cf_os) {
      t_os_tilde[i] = exponential_rng(lambda_os_bg[i]);
    } else {
      t_os_tilde[i] = exp_lognormal_rng(mean_os[i], sd_os, lambda_os_bg[i]);
    }

    if (cf_tilde[i] < cf_pfs) {
      t_pfs_tilde[i] = exponential_rng(lambda_pfs_bg[i]);
    } else {
      t_pfs_tilde[i] = exp_lognormal_rng(mean_pfs[i], sd_pfs, lambda_pfs_bg[i]);
    }
  }

}

