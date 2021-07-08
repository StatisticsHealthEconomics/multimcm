
functions {
    // return sample mean rates
    row_vector rate_mean_rng(row_vector curefrac, real lambda0, real lambda_bg, int n) {

    row_vector[n] lambda;
    real cf[n];

    for (i in 1:n) {
      cf[i] = uniform_rng(0, 1);

      if (cf[i] < curefrac[i]) {
        lambda[i] = lambda_bg;
      } else {
        lambda[i] = lambda_bg + lambda0;
      }
    }
    return(lambda);
  }

    // return sample casemix rates
    row_vector rate_casemix_rng(row_vector curefrac, row_vector lambda0, row_vector lambda_bg, int n) {

    row_vector[n] lambda;
    real cf[n];

    for (i in 1:n) {
      cf[i] = uniform_rng(0, 1);

      if (cf[i] < curefrac[i]) {
        lambda[i] = lambda_bg[i];
      } else {
        lambda[i] = lambda_bg[i] + lambda0[i];
      }
    }
    return(lambda);
  }
}

data {
  int<lower=0> N_os;         // total number of observations
  int<lower=0> N_pfs;

  real<lower=1> os_model;
  real<lower=1> pfs_model;

  int<lower=1> n_samples;
  matrix[n_samples, N_os] cf_os;
  matrix[n_samples, N_pfs] cf_pfs;

  matrix[n_samples, N_os] lambda_os;
  matrix[n_samples, N_pfs] lambda_pfs;
  matrix[n_samples, N_os] lambda_os_bg;
  matrix[n_samples, N_pfs] lambda_pfs_bg;
}

parameters {
}

generated quantities {

  // explicitly loop over samples
  matrix[n_samples, N_os] lambda_os_tilde;
  matrix[n_samples, N_pfs] lambda_pfs_tilde;

  real<lower=0> t_os_tilde[n_samples, N_os];
  real<lower=0> t_pfs_tilde[n_samples, N_pfs];

  for (i in 1:n_samples) {

    // case-mix
    lambda_os_tilde[i, ] = rate_casemix_rng(cf_os[i, ], lambda_os[i, ], lambda_os_bg[i, ], N_os);
    t_os_tilde[i, ] = exponential_rng(lambda_os_tilde[i, ]);

    lambda_pfs_tilde[i, ] = rate_casemix_rng(cf_pfs[i, ], lambda_pfs[i, ], lambda_pfs_bg[i, ], N_pfs);
    t_pfs_tilde[i, ] = exponential_rng(lambda_pfs_tilde[i, ]);
  }
}

