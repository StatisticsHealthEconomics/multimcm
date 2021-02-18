// TODO


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

}



parameters {
  vector[H_os] beta_os;       // coefficients in linear predictor (including intercept)
  vector[H_pfs] beta_pfs;
  vector[bg_model == 1 ? H_os : 0] beta_bg;
  real beta_joint[joint_model];
}

transformed parameters {
  vector[n_os] lp_os;
  vector[n_pfs] lp_pfs;
  vector[n_os] lp_os_bg;
  vector[n_os] lp_pfs_bg;

  vector[n_os] mean_os;
  vector[n_pfs] mean_pfs;

}

model {

  // likelihood
  for (i in 1:n_os) {
    target += log_sum_exp(
  }
}

generated quantities {
  // posterior
  real mean_os;
  real mean_pfs;

}

