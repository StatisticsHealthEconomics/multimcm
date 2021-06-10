functions {
#include /include/distributions.stan
}

data {
	int<lower=1> nTx;
	int<lower=1, upper=3> cf_model;         // cure fraction
int<lower=0> N_pfs;
 int<lower=0> n_pfs[nTx];
 int<lower=0> H_pfs;
 vector<lower=0>[N_pfs] t_pfs;
 vector<lower=0, upper=1>[N_pfs] d_pfs;
 matrix[N_pfs, H_pfs] X_pfs;
 vector[H_pfs] mu_0_pfs;
 vector<lower=0>[H_pfs] sigma_0_pfs;
int<lower=0> N_os;
 int<lower=0> n_os[nTx];
 int<lower=0> H_os;
 vector<lower=0>[N_os] t_os;
 vector<lower=0, upper=1>[N_os] d_os;
 matrix[N_os, H_os] X_os;
 vector[H_os] mu_0_os;
 vector<lower=0>[H_os] sigma_0_os;
int<lower=1, upper=2> bg_model;
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
      int<lower=0> t_max;
real<lower=0> a_cf[cf_model == 1 ? 1 : 0];
      real<lower=0> b_cf[cf_model == 1 ? 1 : 0];
      vector[cf_model == 3 ? nTx : 0] mu_sd_cf;
      vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_sd_cf;

}

parameters {
	vector[H_os] beta_os;       // coefficients in linear predictor (including intercept)
      vector[H_pfs] beta_pfs;
      vector[bg_model == 1 ? H_os : 0] beta_bg;
      real beta_joint[joint_model];
      vector[cf_model != 2 ? nTx : 0] alpha;
      vector[cf_model == 2 ? nTx : 0] alpha_os;
      vector[cf_model == 2 ? nTx : 0] alpha_pfs;
	vector<lower=0, upper=1>[cf_model == 1 ? nTx : 0] cf_pooled;
      vector[cf_model == 3 ? nTx : 0] lp_cf_os;
      vector[cf_model == 3 ? nTx : 0] lp_cf_pfs;
      vector<lower=0>[cf_model == 3 ? nTx : 0] sd_cf;

}

transformed parameters {
	vector[N_os] lp_os_bg;
      vector[N_os] lp_pfs_bg;

      vector<lower=0>[N_os] lambda_os_bg;
      vector<lower=0>[N_os] lambda_pfs_bg;
	vector[N_os] lp_os;
// rate parameters
               vector<lower=0>[N_os] lambda_os;
	vector[N_pfs] lp_pfs;
// rate parameters
             vector<lower=0>[N_pfs] lambda_pfs;
	vector<lower=0, upper=1>[cf_model == 3 ? nTx : 0] cf_global;
        vector<lower=0, upper=1>[nTx] cf_os;
        vector<lower=0, upper=1>[nTx] cf_pfs;
        vector[cf_model == 3 ? nTx : 0] lp_cf_global;
        vector[cf_model == 2 ? nTx : 0] tx_cf_os;
        vector[cf_model == 2 ? nTx : 0] tx_cf_pfs;
// correlated event times
      if (joint_model) {
        lp_os = X_os*beta_os + beta_joint[1]*(t_pfs - 1/exp(beta_pfs[1]));
      } else {
        lp_os = X_os*beta_os;
      }

      lp_pfs = X_pfs*beta_pfs;

      // background survival with uncertainty

      if (bg_model == 1) {
        lp_os_bg = X_os*beta_bg;
        lp_pfs_bg = X_pfs*beta_bg;
      } else {
        lp_os_bg = log(h_bg_os);
        lp_pfs_bg = log(h_bg_pfs);
      }

      lambda_os_bg = exp(lp_os_bg);
      lambda_pfs_bg = exp(lp_pfs_bg);
	lambda_os = exp(lp_os);
lambda_pfs = exp(lp_pfs);
if (cf_model == 3) {
        lp_cf_global = Tx_dmat*alpha;
        cf_global = inv_logit(lp_cf_global);
        cf_os = inv_logit(lp_cf_os);
        cf_pfs = inv_logit(lp_cf_pfs);
      }

      if (cf_model == 2) {
        tx_cf_os = Tx_dmat*alpha_os;
        tx_cf_pfs = Tx_dmat*alpha_pfs;
        cf_os = inv_logit(tx_cf_os);
        cf_pfs = inv_logit(tx_cf_pfs);
      }

      if (cf_model == 1) {
        cf_os = cf_pooled;
        cf_pfs = cf_pooled;
      }

}

model {
	int idx_os;
      int idx_pfs;

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
        sd_cf ~ normal(mu_sd_cf, sigma_sd_cf);
        lp_cf_os ~ normal(lp_cf_global, sd_cf);
        lp_cf_pfs ~ normal(lp_cf_global, sd_cf);
      } else if (cf_model == 2) {
        alpha_os ~ normal(mu_alpha_os, sigma_alpha_os);
        alpha_pfs ~ normal(mu_alpha_pfs, sigma_alpha_pfs);
      } else {
        cf_pooled ~ beta(a_cf, b_cf);
      }
    idx_os = 1;
    idx_pfs = 1;

    // likelihood
    for (Tx in 1:nTx) {
      for (i in idx_os:(idx_os + n_os[Tx] - 1)) {

         target += log_sum_exp(
        log(cf_os[Tx]) +
          surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
        log1m(cf_os[Tx]) +
          joint_exp_exp_lpdf(t_os[i] | d_os[i], lambda_os[i], lambda_os_bg[i]));
      }
      for (j in idx_pfs:(idx_pfs + n_pfs[Tx] - 1)) {

          target += log_sum_exp(
          log(cf_pfs[Tx]) +
            surv_exp_lpdf(t_pfs[j] | d_pfs[j], lambda_pfs_bg[j]),
          log1m(cf_pfs[Tx]) +
            joint_exp_exp_lpdf(t_pfs[j] | d_pfs[j], lambda_pfs[j], lambda_pfs_bg[j]));
         }

      idx_os = idx_os + n_os[Tx];
      idx_pfs = idx_pfs + n_pfs[Tx];
    }
}

generated quantities {
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

      // real pbeta_bg;

	mean_pfs = exp(beta_pfs[1]);
	mean_os = exp(beta_os[1]);
// background rate
      if (bg_model == 1) {
        mean_bg = exp(beta_bg[1]);
      } else {
      // mean_bg = 0.001;
        mean_bg = mean(h_bg_os);
      }

// posterior mean checks
    for (j in 1:nTx) {
      for (i in 1:t_max) {
        S_bg[i] = exp_Surv(i, mean_bg);
        S_os[i] = exp_exp_Surv(i, mean_os, mean_bg);
        S_pfs[i] = exp_exp_Surv(i, mean_pfs, mean_bg);
        S_os_pred[i, j] = cf_os[j]*S_bg[i] + (1 - cf_os[j])*S_os[i];
        S_pfs_pred[i, j] = cf_pfs[j]*S_bg[i] + (1 - cf_pfs[j])*S_pfs[i];
      }
    }
// prior mean checks
    // pmean_os = exp(pbeta_os);
    // pmean_pfs = exp(pbeta_pfs);
    // pmean_bg = exp(pbeta_bg);

    // for (i in 1:t_max) {
    //  pS_bg[i] = exp_Surv(i, pmean_bg);
    //  pS_os[i] = exp_exp_Surv(i, pmean_os, pmean_bg);
    //  pS_pfs[i] = exp_exp_Surv(i, pmean_pfs, pmean_bg);
    //  S_os_prior[i] = pmean_cf_os*pS_bg[i] + (1 - pmean_cf_os)*pS_os[i];
    //  S_pfs_prior[i] = pmean_cf_pfs*pS_bg[i] + (1 - pmean_cf_pfs)*pS_pfs[i];
    // }
    // likelihood
      idx_os = 1;
      idx_pfs = 1;

      for (Tx in 1:nTx) {

      for (i in idx_os:(idx_os + n_os[Tx] - 1)) {
         log_lik[i] = log_sum_exp(
          log(cf_os[Tx]) +
          surv_exp_lpdf(t_os[i] | d_os[i], lambda_os_bg[i]),
          log1m(cf_os[Tx]) +
          joint_exp_exp_lpdf(t_os[i] | d_os[i], lambda_os[i], lambda_os_bg[i]));
        }

      for (j in idx_pfs:(idx_pfs + n_pfs[Tx] - 1)) {
        log_lik[j] = log_sum_exp(
          log(cf_pfs[Tx]) +
            surv_exp_lpdf(t_pfs[j] | d_pfs[j], lambda_pfs_bg[j]),
          log1m(cf_pfs[Tx]) +
            joint_exp_exp_lpdf(t_pfs[j] | d_pfs[j], lambda_pfs[j], lambda_pfs_bg[j]));
        }

        idx_os = idx_os + n_os[Tx];
        idx_pfs = idx_pfs + n_pfs[Tx];
      }
      log_lik = log_lik_os + log_lik_pfs;
}

