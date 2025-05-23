functions {
#include /include/distributions.stan
}

data {
 int<lower=1> nTx;
int<lower=1, upper=3> cf_model;         // cure fraction
int<lower=0> N_1;
array[nTx] int<lower=0> n_1;
int<lower=0> H_1;
vector<lower=0>[N_1] t_1;
vector<lower=0, upper=1>[N_1] d_1;
matrix[N_1, H_1] X_1;
vector[H_1] mu_S_1;
vector<lower=0>[H_1] sigma_S_1;

 int<lower=0> N_2;
array[nTx] int<lower=0> n_2;
int<lower=0> H_2;
vector<lower=0>[N_2] t_2;
vector<lower=0, upper=1>[N_2] d_2;
matrix[N_2, H_2] X_2;
vector[H_2] mu_S_2;
vector<lower=0>[H_2] sigma_S_2;

 int<lower=0> N_3;
array[nTx] int<lower=0> n_3;
int<lower=0> H_3;
vector<lower=0>[N_3] t_3;
vector<lower=0, upper=1>[N_3] d_3;
matrix[N_3, H_3] X_3;
vector[H_3] mu_S_3;
vector<lower=0>[H_3] sigma_S_3;

 int<lower=0> N_4;
array[nTx] int<lower=0> n_4;
int<lower=0> H_4;
vector<lower=0>[N_4] t_4;
vector<lower=0, upper=1>[N_4] d_4;
matrix[N_4, H_4] X_4;
vector[H_4] mu_S_4;
vector<lower=0>[H_4] sigma_S_4;

int<lower=1, upper=2> bg_model;
 vector[bg_model == 1 ? H_1 : 0] mu_bg;
 vector<lower=0>[bg_model == 1 ? H_1 : 0] sigma_bg;
vector<lower=0>[bg_model == 2 ? N_1 : 0] h_bg_1;
vector<lower=0>[bg_model == 2 ? N_2 : 0] h_bg_2;
vector<lower=0>[bg_model == 2 ? N_3 : 0] h_bg_3;
vector<lower=0>[bg_model == 2 ? N_4 : 0] h_bg_4;
 matrix[nTx, nTx] Tx_dmat;         // treatment design matrix
 vector[cf_model == 3 ? nTx : 0] mu_alpha;             // treatment regression coefficients
 vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_alpha;
 int<lower=0> t_max;
vector[cf_model == 2 ? nTx : 0] mu_alpha_1;
vector<lower=0>[cf_model == 2 ? nTx : 0] sigma_alpha_1;
vector[cf_model == 2 ? nTx : 0] mu_alpha_2;
vector<lower=0>[cf_model == 2 ? nTx : 0] sigma_alpha_2;
vector[cf_model == 2 ? nTx : 0] mu_alpha_3;
vector<lower=0>[cf_model == 2 ? nTx : 0] sigma_alpha_3;
vector[cf_model == 2 ? nTx : 0] mu_alpha_4;
vector<lower=0>[cf_model == 2 ? nTx : 0] sigma_alpha_4;
 array[cf_model == 1 ? 1 : 0] real<lower=0> a_cf;

 array[cf_model == 1 ? 1 : 0] real<lower=0> b_cf;
vector[cf_model == 3 ? nTx : 0] mu_sd_cf;
vector<lower=0>[cf_model == 3 ? nTx : 0] sigma_sd_cf;

}

parameters {

 
 
 // coefficients in linear predictor (including intercept)
 vector[bg_model == 1 ? H_1 : 0] beta_bg;
 vector[cf_model != 2 ? nTx : 0] alpha;
vector[cf_model == 2 ? nTx : 0] alpha_1;
vector[H_1] beta_1;
vector[cf_model == 2 ? nTx : 0] alpha_2;
vector[H_2] beta_2;
vector[cf_model == 2 ? nTx : 0] alpha_3;
vector[H_3] beta_3;
vector[cf_model == 2 ? nTx : 0] alpha_4;
vector[H_4] beta_4;
 vector<lower=0, upper=1>[cf_model == 1 ? nTx : 0] cf_pooled;
vector[cf_model == 3 ? nTx : 0] lp_cf_1;
vector[cf_model == 3 ? nTx : 0] lp_cf_2;
vector[cf_model == 3 ? nTx : 0] lp_cf_3;
vector[cf_model == 3 ? nTx : 0] lp_cf_4;
 vector<lower=0>[cf_model == 3 ? nTx : 0] sd_cf;

}

transformed parameters {
vector[N_1] lp_1_bg;

vector<lower=0>[N_1] lambda_1_bg;

vector[N_2] lp_2_bg;

vector<lower=0>[N_2] lambda_2_bg;

vector[N_3] lp_3_bg;

vector<lower=0>[N_3] lambda_3_bg;

vector[N_4] lp_4_bg;

vector<lower=0>[N_4] lambda_4_bg;
vector[N_1] lp_1;
// rate parameters
             vector<lower=0>[N_1] lambda_1;
 vector[N_2] lp_2;
// rate parameters
             vector<lower=0>[N_2] lambda_2;
 vector[N_3] lp_3;
// rate parameters
             vector<lower=0>[N_3] lambda_3;
 vector[N_4] lp_4;
// rate parameters
             vector<lower=0>[N_4] lambda_4;
 vector<lower=0, upper=1>[cf_model == 3 ? nTx : 0] cf_global;
vector<lower=0, upper=1>[nTx] cf_1;
vector[cf_model == 2 ? nTx : 0] tx_cf_1;
vector<lower=0, upper=1>[nTx] cf_2;
vector[cf_model == 2 ? nTx : 0] tx_cf_2;
vector<lower=0, upper=1>[nTx] cf_3;
vector[cf_model == 2 ? nTx : 0] tx_cf_3;
vector<lower=0, upper=1>[nTx] cf_4;
vector[cf_model == 2 ? nTx : 0] tx_cf_4;
 vector[cf_model == 3 ? nTx : 0] lp_cf_global;
// correlated event times
  lp_1 = X_1*beta_1;

// background survival with uncertainty

if (bg_model == 1) {
  lp_1_bg = X_1*beta_bg;
} else {
  lp_1_bg = log(h_bg_1);
}

lambda_1_bg = exp(lp_1_bg);

// correlated event times
  lp_2 = X_2*beta_2;

// background survival with uncertainty

if (bg_model == 1) {
  lp_2_bg = X_2*beta_bg;
} else {
  lp_2_bg = log(h_bg_2);
}

lambda_2_bg = exp(lp_2_bg);

// correlated event times
  lp_3 = X_3*beta_3;

// background survival with uncertainty

if (bg_model == 1) {
  lp_3_bg = X_3*beta_bg;
} else {
  lp_3_bg = log(h_bg_3);
}

lambda_3_bg = exp(lp_3_bg);

// correlated event times
  lp_4 = X_4*beta_4;

// background survival with uncertainty

if (bg_model == 1) {
  lp_4_bg = X_4*beta_bg;
} else {
  lp_4_bg = log(h_bg_4);
}

lambda_4_bg = exp(lp_4_bg);
lambda_1 = exp(lp_1);
 lambda_2 = exp(lp_2);
 lambda_3 = exp(lp_3);
 lambda_4 = exp(lp_4);
 if (cf_model == 1) {
	 cf_1 = cf_pooled;
	 cf_2 = cf_pooled;
	 cf_3 = cf_pooled;
	 cf_4 = cf_pooled;
}
if (cf_model == 3) {
	 lp_cf_global = Tx_dmat*alpha;
	 cf_global = inv_logit(lp_cf_global);
	 cf_1 = inv_logit(lp_cf_1);
	 cf_2 = inv_logit(lp_cf_2);
	 cf_3 = inv_logit(lp_cf_3);
	 cf_4 = inv_logit(lp_cf_4);
}
if (cf_model == 2) {
tx_cf_1 = Tx_dmat*alpha_1;
cf_1 = inv_logit(tx_cf_1);
tx_cf_2 = Tx_dmat*alpha_2;
cf_2 = inv_logit(tx_cf_2);
tx_cf_3 = Tx_dmat*alpha_3;
cf_3 = inv_logit(tx_cf_3);
tx_cf_4 = Tx_dmat*alpha_4;
cf_4 = inv_logit(tx_cf_4);
}

}

model {
int idx_1;
int idx_2;
int idx_3;
int idx_4;      
beta_1 ~ normal(mu_S_1, sigma_S_1);
      
beta_2 ~ normal(mu_S_2, sigma_S_2);
      
beta_3 ~ normal(mu_S_3, sigma_S_3);
      
beta_4 ~ normal(mu_S_4, sigma_S_4);
if (bg_model == 1) {
	 beta_bg ~ normal(mu_bg, sigma_bg);
}

 
 
 // cure fraction 
 if (cf_model == 3) {
 	 alpha ~ normal(mu_alpha, sigma_alpha);
 sd_cf ~ cauchy(mu_sd_cf, sigma_sd_cf);  // truncated

lp_cf_1 ~ normal(lp_cf_global, sd_cf);
lp_cf_2 ~ normal(lp_cf_global, sd_cf);
lp_cf_3 ~ normal(lp_cf_global, sd_cf);
lp_cf_4 ~ normal(lp_cf_global, sd_cf);
} else if (cf_model == 2) {
alpha_1 ~ normal(mu_alpha_1, sigma_alpha_1);
alpha_2 ~ normal(mu_alpha_2, sigma_alpha_2);
alpha_3 ~ normal(mu_alpha_3, sigma_alpha_3);
alpha_4 ~ normal(mu_alpha_4, sigma_alpha_4);
} else {
	 cf_pooled ~ beta(a_cf, b_cf);
}
idx_1 = 1;

// likelihood
for (Tx in 1:nTx) {
  for (i in idx_1:(idx_1 + n_1[Tx] - 1)) {

     target += log_sum_exp(
    log(cf_1[Tx]) +
      exp_log_S(t_1[i], lambda_1_bg[i]),
    log1m(cf_1[Tx]) +
      exp_log_S(t_1[i], lambda_1_bg[i]) + exp_log_S(t_1[i], lambda_1[i]));

     target += d_1[i] * log_sum_exp(
    log(lambda_1_bg[i]),
    log1m(cf_1[Tx]) +
      exp_lpdf(t_1[i] | lambda_1[i]) - log(cf_1[Tx] + (1 - cf_1[Tx])*exp_Surv(t_1[i], lambda_1[i])));
  }

  idx_1 = idx_1 + n_1[Tx];
}
 idx_2 = 1;

// likelihood
for (Tx in 1:nTx) {
  for (i in idx_2:(idx_2 + n_2[Tx] - 1)) {

     target += log_sum_exp(
    log(cf_2[Tx]) +
      exp_log_S(t_2[i], lambda_2_bg[i]),
    log1m(cf_2[Tx]) +
      exp_log_S(t_2[i], lambda_2_bg[i]) + exp_log_S(t_2[i], lambda_2[i]));

     target += d_2[i] * log_sum_exp(
    log(lambda_2_bg[i]),
    log1m(cf_2[Tx]) +
      exp_lpdf(t_2[i] | lambda_2[i]) - log(cf_2[Tx] + (1 - cf_2[Tx])*exp_Surv(t_2[i], lambda_2[i])));
  }

  idx_2 = idx_2 + n_2[Tx];
}
 idx_3 = 1;

// likelihood
for (Tx in 1:nTx) {
  for (i in idx_3:(idx_3 + n_3[Tx] - 1)) {

     target += log_sum_exp(
    log(cf_3[Tx]) +
      exp_log_S(t_3[i], lambda_3_bg[i]),
    log1m(cf_3[Tx]) +
      exp_log_S(t_3[i], lambda_3_bg[i]) + exp_log_S(t_3[i], lambda_3[i]));

     target += d_3[i] * log_sum_exp(
    log(lambda_3_bg[i]),
    log1m(cf_3[Tx]) +
      exp_lpdf(t_3[i] | lambda_3[i]) - log(cf_3[Tx] + (1 - cf_3[Tx])*exp_Surv(t_3[i], lambda_3[i])));
  }

  idx_3 = idx_3 + n_3[Tx];
}
 idx_4 = 1;

// likelihood
for (Tx in 1:nTx) {
  for (i in idx_4:(idx_4 + n_4[Tx] - 1)) {

     target += log_sum_exp(
    log(cf_4[Tx]) +
      exp_log_S(t_4[i], lambda_4_bg[i]),
    log1m(cf_4[Tx]) +
      exp_log_S(t_4[i], lambda_4_bg[i]) + exp_log_S(t_4[i], lambda_4[i]));

     target += d_4[i] * log_sum_exp(
    log(lambda_4_bg[i]),
    log1m(cf_4[Tx]) +
      exp_lpdf(t_4[i] | lambda_4[i]) - log(cf_4[Tx] + (1 - cf_4[Tx])*exp_Surv(t_4[i], lambda_4[i])));
  }

  idx_4 = idx_4 + n_4[Tx];
}
}

generated quantities {
real mean_bg;
 // real pbeta_bg;
 real log_lik = 0;
 vector[t_max] S_bg;
vector[t_max] S_1;
vector[nTx] rmst_1;
vector[nTx] median_1;
matrix[t_max, nTx] S_1_pred;
real mean_1;
int idx_1;
real log_lik_1;
// real pbeta_1 = normal_rng(mu_S_1[1], sigma_S_1[1]);


vector[t_max] S_2;
vector[nTx] rmst_2;
vector[nTx] median_2;
matrix[t_max, nTx] S_2_pred;
real mean_2;
int idx_2;
real log_lik_2;
// real pbeta_2 = normal_rng(mu_S_2[1], sigma_S_2[1]);


vector[t_max] S_3;
vector[nTx] rmst_3;
vector[nTx] median_3;
matrix[t_max, nTx] S_3_pred;
real mean_3;
int idx_3;
real log_lik_3;
// real pbeta_3 = normal_rng(mu_S_3[1], sigma_S_3[1]);


vector[t_max] S_4;
vector[nTx] rmst_4;
vector[nTx] median_4;
matrix[t_max, nTx] S_4_pred;
real mean_4;
int idx_4;
real log_lik_4;
// real pbeta_4 = normal_rng(mu_S_4[1], sigma_S_4[1]);


 
 
 mean_1 = exp(beta_1[1]);
 mean_2 = exp(beta_2[1]);
 mean_3 = exp(beta_3[1]);
 mean_4 = exp(beta_4[1]);// background rate
if (bg_model == 1) {
	mean_bg = exp(beta_bg[1]);
} else {
// mean_bg = 0.001;
mean_bg = mean(h_bg_1);
mean_bg = mean(h_bg_2);
mean_bg = mean(h_bg_3);
mean_bg = mean(h_bg_4);
}
// posterior mean checks
for (j in 1:nTx) {
  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, mean_bg);
    S_1[i] = exp_exp_Surv(i, mean_1, mean_bg);
    S_1_pred[i, j] = cf_1[j]*S_bg[i] + (1 - cf_1[j])*S_1[i];
  }
}

 // posterior mean checks
for (j in 1:nTx) {
  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, mean_bg);
    S_2[i] = exp_exp_Surv(i, mean_2, mean_bg);
    S_2_pred[i, j] = cf_2[j]*S_bg[i] + (1 - cf_2[j])*S_2[i];
  }
}

 // posterior mean checks
for (j in 1:nTx) {
  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, mean_bg);
    S_3[i] = exp_exp_Surv(i, mean_3, mean_bg);
    S_3_pred[i, j] = cf_3[j]*S_bg[i] + (1 - cf_3[j])*S_3[i];
  }
}

 // posterior mean checks
for (j in 1:nTx) {
  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, mean_bg);
    S_4[i] = exp_exp_Surv(i, mean_4, mean_bg);
    S_4_pred[i, j] = cf_4[j]*S_bg[i] + (1 - cf_4[j])*S_4[i];
  }
}
// restricted mean survival time
for (i in 1:nTx) {
  rmst_1[i] = cf_1[i]*t_max + (1 - cf_1[i])*rmst_exp(mean_1, t_max);
  median_1[i] = median_surv_cf_exp(mean_1, cf_1[i]);
  //median_1[i] = median_surv_exp(mean_1);  // uncured
}

 // restricted mean survival time
for (i in 1:nTx) {
  rmst_2[i] = cf_2[i]*t_max + (1 - cf_2[i])*rmst_exp(mean_2, t_max);
  median_2[i] = median_surv_cf_exp(mean_2, cf_2[i]);
  //median_2[i] = median_surv_exp(mean_2);  // uncured
}

 // restricted mean survival time
for (i in 1:nTx) {
  rmst_3[i] = cf_3[i]*t_max + (1 - cf_3[i])*rmst_exp(mean_3, t_max);
  median_3[i] = median_surv_cf_exp(mean_3, cf_3[i]);
  //median_3[i] = median_surv_exp(mean_3);  // uncured
}

 // restricted mean survival time
for (i in 1:nTx) {
  rmst_4[i] = cf_4[i]*t_max + (1 - cf_4[i])*rmst_exp(mean_4, t_max);
  median_4[i] = median_surv_cf_exp(mean_4, cf_4[i]);
  //median_4[i] = median_surv_exp(mean_4);  // uncured
}
// prior mean checks
// pmean_pfs = exp(pbeta_pfs);
// pmean_bg = exp(pbeta_bg);

// for (i in 1:t_max) {
//  pS_bg[i] = exp_Surv(i, pmean_bg);
//  pS_1[i] = exp_exp_Surv(i, lambda_1[j], pmean_bg);
//  S_1_prior[i] = pmean_cf_1*pS_bg[i] + (1 - pmean_cf_1)*pS_1[i,j];
// }

 // prior mean checks
// pmean_pfs = exp(pbeta_pfs);
// pmean_bg = exp(pbeta_bg);

// for (i in 1:t_max) {
//  pS_bg[i] = exp_Surv(i, pmean_bg);
//  pS_2[i] = exp_exp_Surv(i, lambda_2[j], pmean_bg);
//  S_2_prior[i] = pmean_cf_2*pS_bg[i] + (1 - pmean_cf_2)*pS_2[i,j];
// }

 // prior mean checks
// pmean_pfs = exp(pbeta_pfs);
// pmean_bg = exp(pbeta_bg);

// for (i in 1:t_max) {
//  pS_bg[i] = exp_Surv(i, pmean_bg);
//  pS_3[i] = exp_exp_Surv(i, lambda_3[j], pmean_bg);
//  S_3_prior[i] = pmean_cf_3*pS_bg[i] + (1 - pmean_cf_3)*pS_3[i,j];
// }

 // prior mean checks
// pmean_pfs = exp(pbeta_pfs);
// pmean_bg = exp(pbeta_bg);

// for (i in 1:t_max) {
//  pS_bg[i] = exp_Surv(i, pmean_bg);
//  pS_4[i] = exp_exp_Surv(i, lambda_4[j], pmean_bg);
//  S_4_prior[i] = pmean_cf_4*pS_bg[i] + (1 - pmean_cf_4)*pS_4[i,j];
// }
// likelihood
  idx_1 = 1;

  for (Tx in 1:nTx) {
    for (i in idx_1:(idx_1 + n_1[Tx] - 1)) {
      log_lik_1 += log_sum_exp(
      log(cf_1[Tx]) +
      surv_exp_lpdf(t_1[i] | d_1[i], lambda_1_bg[i]),
      log1m(cf_1[Tx]) +
      joint_exp_exp_lpdf(t_1[i] | d_1[i], lambda_1[i], lambda_1_bg[i]));
    }

    idx_1 = idx_1 + n_1[Tx];
  }
  log_lik = log_lik + log_lik_1;

 // likelihood
  idx_2 = 1;

  for (Tx in 1:nTx) {
    for (i in idx_2:(idx_2 + n_2[Tx] - 1)) {
      log_lik_2 += log_sum_exp(
      log(cf_2[Tx]) +
      surv_exp_lpdf(t_2[i] | d_2[i], lambda_2_bg[i]),
      log1m(cf_2[Tx]) +
      joint_exp_exp_lpdf(t_2[i] | d_2[i], lambda_2[i], lambda_2_bg[i]));
    }

    idx_2 = idx_2 + n_2[Tx];
  }
  log_lik = log_lik + log_lik_2;

 // likelihood
  idx_3 = 1;

  for (Tx in 1:nTx) {
    for (i in idx_3:(idx_3 + n_3[Tx] - 1)) {
      log_lik_3 += log_sum_exp(
      log(cf_3[Tx]) +
      surv_exp_lpdf(t_3[i] | d_3[i], lambda_3_bg[i]),
      log1m(cf_3[Tx]) +
      joint_exp_exp_lpdf(t_3[i] | d_3[i], lambda_3[i], lambda_3_bg[i]));
    }

    idx_3 = idx_3 + n_3[Tx];
  }
  log_lik = log_lik + log_lik_3;

 // likelihood
  idx_4 = 1;

  for (Tx in 1:nTx) {
    for (i in idx_4:(idx_4 + n_4[Tx] - 1)) {
      log_lik_4 += log_sum_exp(
      log(cf_4[Tx]) +
      surv_exp_lpdf(t_4[i] | d_4[i], lambda_4_bg[i]),
      log1m(cf_4[Tx]) +
      joint_exp_exp_lpdf(t_4[i] | d_4[i], lambda_4[i], lambda_4_bg[i]));
    }

    idx_4 = idx_4 + n_4[Tx];
  }
  log_lik = log_lik + log_lik_4;
}

