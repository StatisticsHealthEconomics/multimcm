
////TODO: stand-alone posterior predictions ----
// match input data case-mix?
// https://mc-stan.org/docs/2_23/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html

functions {

    // return sample mean rates
    row_vector rate_mean_rng(real curefrac, real lambda0, real lambda_bg, int n) {

    row_vector[n] lambda;
    real cf[n];

    for (i in 1:n) {
      cf[i] = uniform_rng(0, 1);

      if (cf[i] < curefrac) {
        lambda[i] = lambda_bg;
      } else {
        lambda[i] = lambda_bg + lambda0;
      }
    }
    return(lambda);
  }

    // return sample casemix rates
    row_vector rate_casemix_rng(real curefrac, row_vector lambda0, row_vector lambda_bg, int n) {

    row_vector[n] lambda;
    real cf[n];

    for (i in 1:n) {
      cf[i] = uniform_rng(0, 1);

      if (cf[i] < curefrac) {
        lambda[i] = lambda_bg[i];
      } else {
        lambda[i] = lambda_bg[i] + lambda0[i];
      }
    }
    return(lambda);
  }
}

data {
  int<lower=1> n;

  //TODO: in parameters block instead?
  int<lower=1> n_samples;
  real<lower=0, upper=1> cf_os[n_samples];
  real<lower=0, upper=1> cf_pfs[n_samples];
  matrix[n_samples, n] lambda_os;
  matrix[n_samples, n] lambda_pfs;
  matrix[n_samples, n] lambda_os_bg;
  matrix[n_samples, n] lambda_pfs_bg;

  // using means
  // real beta_os[n_samples, 2];
  // real beta_bg[n_samples, 2];
}

parameters {
  //TODO: should these be here?
  // real<lower=0, upper=1> curefrac;
  // real beta_os[2];
  // real beta_bg[2];
}

generated quantities {

  // // don't
  // vector[n] lambda_tilde;
  // real<lower=0> t_tilde[n];
  // real lambda0 = exp(beta_os[1]);
  // real lambda_bg = exp(beta_bg[1]);
  // lambda_tilde = rate_mean_rng(curefrac, lambda0, lambda_bg, n);
  // t_tilde = exponential_rng(lambda_tilde);

  // explicitly loop over samples
  matrix[n_samples, n] lambda_os_tilde;
  real<lower=0> t_os_tilde[n_samples, n];
  matrix[n_samples, n] lambda_pfs_tilde;
  real<lower=0> t_pfs_tilde[n_samples, n];
  // real lambda0;
  // real lambda_bg;

  for (i in 1:n_samples) {

    // // using means
    // lambda0 = exp(beta_os[i, 1]);
    // lambda_bg = exp(beta_bg[i, 1]);
    // lambda_tilde[i, 1:n] = rate_mean_rng(curefrac[i], lambda0, lambda_bg, n);

    lambda_os_tilde[i, ] = rate_casemix_rng(cf_os[i], lambda_os[i, ], lambda_os_bg[i, ], n);
    t_os_tilde[i, ] = exponential_rng(lambda_os_tilde[i, ]);

    lambda_pfs_tilde[i, ] = rate_casemix_rng(cf_pfs[i], lambda_pfs[i, ], lambda_pfs_bg[i, ], n);
    t_pfs_tilde[i, ] = exponential_rng(lambda_pfs_tilde[i, ]);
  }

}

