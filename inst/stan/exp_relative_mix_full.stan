// exponential mixture cure model
// relative survival

// ideas:
//  * more than 2 mixture components
//  * prob group membership per individual

functions {
#include /include/distributions.stan
}

// input data ----
data {
  int<lower=0> n;             // number of observations
  vector[n] t;                // observed times
  vector[n] d;                // censoring indicator (1 = observed, 0 = censored)
  int<lower = 0> H;           // number of covariates
  matrix[n,H] X;              // matrix of covariates (with n rows and H columns)

  vector[H] mu_0;               // disease mean
  vector<lower=0> [H] sigma_0;  // disease sd
  vector[H] mu_bg;              // background mean
  vector<lower=0> [H] sigma_bg; // background sd
  vector[H] mu_cf;              // cure fraction mean
  vector<lower=0> [H] sigma_cf; // cure fraction sd

  int<lower=0> t_max;
}

parameters {
  // coefficients in linear predictor (including intercept)
  vector[H] beta0;    // disease
  vector[H] beta_bg;  // background
  vector[H] beta_cf;  // cure fraction
}

transformed parameters {
  vector[n] linpred0;   // linear predictors
  vector[n] linpred_bg;
  vector[n] linpred_cf;
  vector[n] lambda0;    // hazards
  vector[n] lambda_bg;
  vector[n] curefrac;

  linpred0 = X*beta0;
  linpred_bg = X*beta_bg;
  linpred_cf = X*beta_cf;

  // rate parameters
  lambda0 = exp(linpred0);
  lambda_bg = exp(linpred_bg);

  curefrac = inv_logit(linpred_cf);
}

model {
  // priors
  beta0 ~ normal(mu_0, sigma_0);
  beta_bg ~ normal(mu_bg, sigma_bg);
  beta_cf ~ normal(mu_cf, sigma_cf);

  for (i in 1:n) {

    target += log_sum_exp(log(curefrac[i]) +
                surv_exp_lpdf(t[i] | d[i], lambda_bg[i]),
                log1m(curefrac[i]) +
                surv_exp_lpdf(t[i] | d[i], lambda_bg[i] + lambda0[i]));
  }
}

generated quantities {
  real rate0;
  real rate_bg;
  real gen_cf;
  vector[t_max] S_bg;
  vector[t_max] S0;
  vector[t_max] S_pred;

  # intercept
  rate0 = exp(beta0[1]);
  rate_bg = exp(beta_bg[1]);
  gen_cf = logit(beta_cf[1]);

  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, rate_bg);
    S0[i] = exp_Surv(i, rate_bg + rate0);
    S_pred[i] = gen_cf*S_bg[i] + (1 - gen_cf)*S0[i];
  }

  ////TODO: posterior predictions ----
  // match input data case-mix
  //
  // vector<lower=0> t_tilde[n] =
  //   exp_mix_rng(curefrac, lambda0, lambda_bg)
  //
  //   exp_mix_reg = function(real curefrac, vector lambda0, vector lambda_bg) {
  //
  //     vector<lower=0> t[n]
  //
  //     for (i in 1:n) {
  //       //TODO: how is this vectorised over posterior draws?
  //       real U = uniform_rng(0,1)
  //
  //       if (curefrac > U) {
  //         t[i] = exponential_rng(lambda_bg[i])
  //       } else {
  //         t[i] = exponential_rng(lambda_bg[i] + lambda0[i])
  //       }
  //     }
  //     return(t)
  //   }
  //
  // vector<lower=0> lambda_tilde[n] =
  //   rate_mix_rng(curefrac, lambda0, lambda_bg)
  //
  //   rate_mix_reg = function(real curefrac, vector lambda0, vector lambda_bg) {
  //
  //     vector<lower=0> lambda[n]
  //
  //     for (i in 1:n) {
  //       // same U for all posterior samples for given individual
  //       real U = uniform_rng(0,1)
  //
  //       if (curefrac > U) {
  //         lambda[i] = lambda_bg[i]
  //       } else {
  //         lambda[i] = lambda_bg[i] + lambda0[i]
  //       }
  //     }
  //     return(lambda)
  //   }

}

