// exponential mixture cure model
// relative survival

//ideas:
// * more than 2 mixture components
// * prob group membership per individual

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

  // intercept only -
  // real mu_beta;	              // means of the covariates coefficients
  // real mu_bg;
  // real<lower=0> sigma_beta;    // sds of the covariates coefficients
  // real<lower=0> sigma_bg;
  // intercept and gradient -
  vector[H] mu_0;
  vector<lower=0> [H] sigma_0;
  vector[H] mu_bg;
  vector<lower=0> [H] sigma_bg;

  real a_cf;                  // cure fraction ~ Beta(a,b)
  real b_cf;
  // vector[H] mu_cf;
  // vector<lower=0> [H] sigma_cf;

  // vector[n] h_bg;

  int<lower=0> t_max;
}

parameters {
  vector[H] beta0;         // coefficients in linear predictor (including intercept)
  vector[H] beta_bg;
  // vector[H] beta_cf;
  real<lower=0, upper=1> curefrac;  //TODO: define as simplex?
}

transformed parameters {
  vector[n] linpred0;
  vector[n] linpred_bg;
  vector[n] lambda0;
  vector[n] lambda_bg;

  linpred0 = X*beta0;
  linpred_bg = X*beta_bg;
  // linpred_cf = X*beta_cf;

  // rate parameters
  lambda0 = exp(linpred0);
  lambda_bg = exp(linpred_bg); // background survival with uncertainty
  // lambda_bg = h_bg;           // _known_ point estimate for background survival

  // cure_frac = inv_logit(linpred_cf)
  //TODO:
  // lambda_bg = is_fixed ? h_bg : exp(linpred_bg);
}

model {
  beta0 ~ normal(mu_0, sigma_0);
  beta_bg ~ normal(mu_bg, sigma_bg);

  // beta_cf ~ normal(mu_cf, sigma_cf);
  curefrac ~ beta(a_cf, b_cf);

  for (i in 1:n) {

    // target += log_mix(curefrac,
    //                   surv_exp_lpdf(t[i] | d[i], lambda_bg[i]),
    //                   surv_exp_lpdf(t[i] | d[i], lambda_bg[i] + lambda0[i]));

    // equivalently
    target += log_sum_exp(log(curefrac) +
                surv_exp_lpdf(t[i] | d[i], lambda_bg[i]),
                log1m(curefrac) +
                surv_exp_lpdf(t[i] | d[i], lambda_bg[i] + lambda0[i]));
  }
}

generated quantities {
  real rate0;
  real rate_bg;
  vector[t_max] S_bg;
  vector[t_max] S_0;
  vector[t_max] S_pred;

  real pmean_0;
  real pmean_bg;

  vector[t_max] pS_bg;
  vector[t_max] pS_0;
  vector[t_max] S_prior;

  real pbeta_0 = normal_rng(mu_0[1], sigma_0[1]);
  real pbeta_bg = normal_rng(mu_bg[1], sigma_bg[1]);
  real pmean_cf = beta_rng(a_cf, b_cf);

  # intercept
  rate0 = exp(beta0[1]);
  rate_bg = exp(beta_bg[1]);

  for (i in 1:t_max) {
    S_bg[i] = exp_Surv(i, rate_bg);
    S_0[i] = exp_Surv(i, rate_bg + rate0);
    S_pred[i] = curefrac*S_bg[i] + (1 - curefrac)*S_0[i];
  }

  # prior checks
  pmean_0 = exp(pbeta_0);
  pmean_bg = exp(pbeta_bg);

  for (i in 1:t_max) {
    pS_bg[i] = exp_Surv(i, pmean_bg);
    pS_0[i] = exp_Surv(i, pmean_bg + pmean_0);
    S_prior[i] = pmean_cf*pS_bg[i] + (1 - pmean_cf)*pS_0[i];
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

