// exponential mixture cure model
// hierarchical cure fraction
// joint cured/uncured survival across treatment


functions {
#include /include/distributions.stan
}

data {
  int<lower=1> nTx;
  int<lower=0> N_os;             // total number of observations
  int<lower=0> N_pfs;
  int<lower=0> n_os[nTx];        // group sizes
  int<lower=0> n_pfs[nTx];
  int<lower=0> H_os;             // number of covariates
  int<lower=0> H_pfs;
  
  vector[N_os] t_os;        // observation times
  vector[N_pfs] t_pfs;
  
  vector[N_os] d_os;        // censoring indicator (1 = observed, 0 = censored)
  vector[N_pfs] d_pfs;
  
  matrix[N_os, H_os] X_os;       // matrix of covariates (with n rows and H columns)
  matrix[N_pfs, H_pfs] X_pfs;
  
  vector[H_os] mu_0_os;
  vector[H_pfs] mu_0_pfs;
  vector<lower=0> [H_os] sigma_0_os;
  vector<lower=0> [H_pfs] sigma_0_pfs;
  
  int<lower=1, upper=2> bg_model;
  vector[bg_model == 1 ? H_os : 0] mu_bg;
  vector<lower=0>[bg_model == 1 ? H_os : 0] sigma_bg;
  vector[bg_model == 2 ? N_os : 0] h_bg_os;
  vector[bg_model == 2 ? N_pfs : 0] h_bg_pfs;
  
  int<lower=0, upper=1> joint_model;
  real mu_joint[joint_model];
  real<lower=0> sigma_joint[joint_model];
  
  int<lower=1, upper=3> cf_model;         // cure fraction
  // 1- shared; 2- separate; 3- hierarchical
  real mu_cf_os[cf_model == 2 ? 1 : 0];
  real mu_cf_pfs[cf_model == 2 ? 1 : 0];
  vector<lower=0>[cf_model == 2 ? nTx : 0] sd_cf_os;
  vector<lower=0>[cf_model == 2 ? nTx : 0] sd_cf_pfs;
  real a_cf[cf_model == 1 ? 1 : 0];
  real b_cf[cf_model == 1 ? 1 : 0];
  
  int<lower=0> t_max;
  
  matrix[nTx, nTx] Tx_dmat;         // treatment design matrix
  vector[nTx] mu_alpha;
  vector<lower=0>[nTx] sigma_alpha;
  
  vector[cf_model == 3 ? nTx : 0] mu_sd_cf;
  vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_sd_cf;
  
  //// background hazard ratio adjustment
  // matrix[N_os, nTx] dmat_os;
  // matrix[N_pfs, nTx] dmat_pfs;
  // vector[nTx] mu_hr;
  // vector<lower=0>[nTx] sigma_hr;
}

parameters {
  vector[H_os] beta_os;       // coefficients in linear predictor (including intercept)
  vector[H_pfs] beta_pfs;
  vector[bg_model == 1 ? H_os : 0] beta_bg;
  real beta_joint[joint_model];
  
  vector[nTx] alpha;
  
  vector<lower=0, upper=1>[cf_model == 1 ? nTx : 0] cf_pooled;
  vector[cf_model != 1 ? nTx : 0] lp_cf_os;
  vector[cf_model != 1 ? nTx : 0] lp_cf_pfs;
  
  vector[cf_model == 3 ? nTx : 0] log_sd_cf;
}

transformed parameters {
  vector[N_os] lp_os;
  vector[N_pfs] lp_pfs;
  vector[N_os] lp_os_bg;
  vector[N_pfs] lp_pfs_bg;
  
  vector[N_os] lambda_os;
  vector[N_pfs] lambda_pfs;
  vector[N_os] lambda_os_bg;
  vector[N_pfs] lambda_pfs_bg;
  
  vector<lower=0, upper=1>[cf_model == 3 ? nTx : 0] cf_global;
  vector<lower=0, upper=1>[nTx] cf_os;
  vector<lower=0, upper=1>[nTx] cf_pfs;
  
  vector[cf_model == 3 ? nTx : 0] lp_cf_global;
  vector<lower=0>[cf_model == 3 ? nTx : 0] sd_cf;
  
  lp_os = X_os*beta_os;
  lp_pfs = X_pfs*beta_pfs;
  
  //TODO:
    // // treatment hazard ratio
  // loghr_os = dmat_os*tx_hr_os;
  // loghr_pfs = dmat_pfs*tx_hr_pfs;
  // lp_os = lp_os + loghr_os;
  // lp_pfs = lp_pfs + loghr_pfs;
  
  if (bg_model == 1) {          // background survival with uncertainty
    lp_os_bg = X_os*beta_bg;
    lp_pfs_bg = X_pfs*beta_bg;
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
    sd_cf = exp(log_sd_cf);
    
    lp_cf_global = Tx_dmat*alpha;
    cf_global = inv_logit(lp_cf_global);
  }
  if (cf_model != 1) {
    cf_os = inv_logit(lp_cf_os);
    cf_pfs = inv_logit(lp_cf_pfs);
  } else {
    cf_os = cf_pooled;
    cf_pfs = cf_pooled;
  }
}

model {
  int idx_os;
  int idx_pfs;
  
  // priors
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
    alpha ~ normal(mu_alpha, sigma_alpha);
    // tx_hr_os ~ normal(mu_hr_os, sigma_hr_os);
    // tx_hr_pfs ~ normal(mu_hr_pfs, sigma_hr_pfs);
    
    log_sd_cf ~ normal(mu_sd_cf, sigma_sd_cf);
    
    lp_cf_os ~ normal(lp_cf_global, sd_cf);
    lp_cf_pfs ~ normal(lp_cf_global, sd_cf);
    
  } else if (cf_model == 2) {
    lp_cf_os ~ normal(mu_cf_os, sd_cf_os);
    lp_cf_pfs ~ normal(mu_cf_pfs, sd_cf_pfs);
  } else {
    cf_pooled ~ beta(a_cf, b_cf);
  }
  
  // likelihood
  idx_os = 1;
  idx_pfs = 1;
  
  // cumsum_os = cumulative_sum(append_row(1, n_os));
  
  for (Tx in 1:nTx) {
    
    // for (i in cumsum_os[Tx]:cumsum_os[Tx + 1]) {
      for (i in idx_os:(idx_os + n_os[Tx] - 1)) {
        
        target += log_sum_exp(
          log(cf_os[Tx]) +
            surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
          log1m(cf_os[Tx]) +
            surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i] + lambda_os[i]));
      }
      
      for (j in idx_pfs:(idx_pfs + n_pfs[Tx] - 1)) {
        
        target += log_sum_exp(
          log(cf_pfs[Tx]) +
            surv_exp_lpdf(t_pfs[j] | d_pfs[j], lambda_pfs_bg[j]),
          log1m(cf_pfs[Tx]) +
            surv_exp_lpdf(t_pfs[j] | d_pfs[j], lambda_pfs_bg[j] + lambda_pfs[j]));
      }
      
      idx_os = idx_os + n_os[Tx];
      idx_pfs = idx_pfs + n_pfs[Tx];
    }
  }
  
  generated quantities {
    // posterior
    real mean_os;
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
    
    // vector[t_max, nTx] pS_bg;
    // vector[t_max] pS_os;
    // vector[t_max] pS_pfs;
    // vector[t_max] S_os_prior;
    // vector[t_max] S_pfs_prior;
    
    vector[N_os] log_lik_os;
    vector[N_pfs] log_lik_pfs;
    vector[N_os] log_lik;
    
    // real pbeta_os = normal_rng(mu_0_os[1], sigma_0_os[1]);
    // real pbeta_pfs = normal_rng(mu_0_pfs[1], sigma_0_pfs[1]);
    //
      // real pbeta_bg;
    
    // if (cf_model == 3) {
      // real vpc_os;
      // real vpc_pfs;
      //
        // vpc_os = sd_cf_os/(sigma_cf_gl + sd_cf_os);
        // vpc_pfs = sd_cf_pfs/(sigma_cf_gl + sd_cf_pfs);
        // }
    
    // if (bg_model == 1) {
      //   pbeta_bg = normal_rng(mu_bg[1], sigma_bg[1]);
      // } else {
        //   // pbeta_bg = log(mean(h_bg_os));
        //   pbeta_bg = log(0.001);
        // }
    //
      //   // cure fraction prior
    //   if (cf_model == 3) {
      //     real pcurefrac = normal_rng(mu_cf_gl[1], sigma_cf_gl[1]);
      //     pmean_cf_os = inv_logit(pcurefrac);
      //     pmean_cf_pfs = inv_logit(pcurefrac);
      //
        //   } else if (cf_model == 2) {
          //     real pcf_os = normal_rng(mu_cf_os[1], sd_cf_os[1]);
          //     real pcf_pfs = normal_rng(mu_cf_pfs[1], sd_cf_pfs[1]);
          //     pmean_cf_os = inv_logit(pcf_os);
          //     pmean_cf_pfs = inv_logit(pcf_pfs);
          //   } else {
            //     real pcurefrac = beta_rng(a_cf[1], b_cf[1]);
            //     pmean_cf_os = pcurefrac;
            //     pmean_cf_pfs = pcurefrac;
            //   }
    
    // intercepts
    mean_os = exp(beta_os[1]);
    mean_pfs = exp(beta_pfs[1]);
    
    if (bg_model == 1) {
      mean_bg = exp(beta_bg[1]);
    } else {
      // mean_bg = 0.001;
      mean_bg = mean(h_bg_os);
    }
    
    for (j in 1:nTx) {
      for (i in 1:t_max) {
        S_bg[i] = exp_Surv(i, mean_bg);
        S_os[i] = exp_Surv(i, mean_bg + mean_os);
        S_pfs[i] = exp_Surv(i, mean_bg + mean_pfs);
        
        S_os_pred[i, j] = cf_os[j]*S_bg[i] + (1 - cf_os[j])*S_os[i];
        S_pfs_pred[i, j] = cf_pfs[j]*S_bg[i] + (1 - cf_pfs[j])*S_pfs[i];
      }
    }
    
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
    
    // log-likelihood for loo
    // http://mc-stan.org/loo/reference/extract_log_lik.html
    
    idx_os = 1;
    idx_pfs = 1;
    
    for (Tx in 1:nTx) {
      
      for (i in idx_os:(idx_os + n_os[Tx] - 1)) {
        
        log_lik_os[i] = log_sum_exp(
          log(cf_os[Tx]) +
            surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
          log1m(cf_os[Tx]) +
            surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i] + lambda_os[i]));
      }
      
      for (j in idx_pfs:(idx_pfs + n_pfs[Tx] - 1)) {
        
        log_lik_pfs[j] = log_sum_exp(
          log(cf_pfs[Tx]) +
            surv_exp_lpdf(t_pfs[j] | d_pfs[j], lambda_pfs_bg[j]),
          log1m(cf_pfs[Tx]) +
            surv_exp_lpdf(t_pfs[j] | d_pfs[j], lambda_pfs_bg[j] + lambda_pfs[j]));
      }
      
      idx_os = idx_os + n_os[Tx];
      idx_pfs = idx_pfs + n_pfs[Tx];
    }
    
    log_lik = log_lik_os + log_lik_pfs;
  }
  