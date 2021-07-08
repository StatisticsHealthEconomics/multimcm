functions {
    row_vector os_casemix_rng(row_vector curefrac, real shape, row_vector lambda0, row_vector lambda_bg) {

    int n = num_elements(curefrac);
    row_vector[n] time;
    real cf[n];

    for (i in 1:n) {
      cf[i] = uniform_rng(0, 1);

      if (cf[i] < curefrac[i]) {
        time[i] = exponential_rng(lambda_bg[i]);
      } else {
        time[i] = fmin(weibull_rng(shape, lambda0[i]), exponential_rng(lambda_bg[i]));
      }
    }
    return(time);
    }


    row_vector pfs_casemix_rng(row_vector curefrac, real shape, row_vector lambda0, row_vector lambda_bg) {

    int n = num_elements(curefrac);
    row_vector[n] time;
    real cf[n];

    for (i in 1:n) {
      cf[i] = uniform_rng(0, 1);

      if (cf[i] < curefrac[i]) {
        time[i] = exponential_rng(lambda_bg[i]);
      } else {
        time[i] = fmin(weibull_rng(shape, lambda0[i]), exponential_rng(lambda_bg[i]));
      }
    }
    return(time);
    }
}

data {
    int<lower=0> N_os;         // total number of observations
    int<lower=0> N_pfs;

    int<lower=1> n_samples;
    matrix[n_samples, N_os] cf_os;
    matrix[n_samples, N_pfs] cf_pfs;

    matrix[n_samples, N_os] lambda_os;
    matrix[n_samples, N_pfs] lambda_pfs;

    matrix[n_samples, N_os] lambda_os_bg;
    matrix[n_samples, N_pfs] lambda_pfs_bg;
vector[n_samples] shape_os;
vector[n_samples] shape_pfs;
}

parameters {
}

generated quantities {
    // explicitly loop over samples
    matrix[n_samples, N_os] t_os_tilde;
    matrix[n_samples, N_pfs] t_pfs_tilde;

    for (i in 1:n_samples) {
      t_os_tilde[i, ] = os_casemix_rng(cf_os[i, ], shape_os[i], lambda_os[i, ], lambda_os_bg[i, ]);
      t_pfs_tilde[i, ] = pfs_casemix_rng(cf_pfs[i, ], shape_pfs[i], lambda_pfs[i, ], lambda_pfs_bg[i, ]);
    }

}
