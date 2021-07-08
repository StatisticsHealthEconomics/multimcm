
##TODO: currently only for exp-exp hard-coded
#
create_stancode_postpred <- function() {

  scode <- list()

  scode$functions <-
    "functions {
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
      }\n}\n\n"

  scode$data <- paste0(
    "data {\n",
    "int<lower=1> n; // num individuals
    real<lower=1> os_model;
    real<lower=1> pfs_model;

    int<lower=1> n_samples;
    real<lower=0, upper=1> cf_os[n_samples];
    real<lower=0, upper=1> cf_pfs[n_samples];

    matrix[n_samples, n] lambda_os;
    matrix[n_samples, n] lambda_pfs;
    matrix[n_samples, n] lambda_os_bg;
    matrix[n_samples, n] lambda_pfs_bg;

    // using means
    real beta_os[n_samples, 2];
    real beta_pfs[n_samples, 2];
    real beta_bg[n_samples];",
    "\n}\n\n"
  )

  scode$parameters <- "parameters {\n}\n\n"

  scode$generated_quantities <- paste0(
    "generated quantities {\n",
    "// explicitly loop over samples
    matrix[n_samples, n] lambda_os_tilde;
    matrix[n_samples, n] lambda_pfs_tilde;

    real<lower=0> t_os_tilde[n_samples, n];
    real<lower=0> t_pfs_tilde[n_samples, n];

    real lambda_os_mean[n_samples];
    real lambda_pfs_mean[n_samples];
    real lambda_bg[n_samples];

    matrix[n_samples, n] lambda_os_bar;
    matrix[n_samples, n] lambda_pfs_bar;

    real<lower=0> t_os_bar[n_samples, n];
    real<lower=0> t_pfs_bar[n_samples, n];

    for (i in 1:n_samples) {

      // using means
      lambda_os_mean[i] = exp(beta_os[i, 1]);
      lambda_pfs_mean[i] = exp(beta_pfs[i, 1]);
      lambda_bg[i] = exp(beta_bg[i]);

      lambda_os_bar[i, 1:n] = rate_mean_rng(cf_os[i], lambda_os_mean[i], lambda_bg[i], n);
      t_os_bar[i, ] = exponential_rng(lambda_os_bar[i, ]);

      lambda_pfs_bar[i, 1:n] = rate_mean_rng(cf_pfs[i], lambda_pfs_mean[i], lambda_bg[i], n);
      t_pfs_bar[i, ] = exponential_rng(lambda_pfs_bar[i, ]);

      // case-mix
      lambda_os_tilde[i, ] = rate_casemix_rng(cf_os[i], lambda_os[i, ], lambda_os_bg[i, ], n);
      t_os_tilde[i, ] = exponential_rng(lambda_os_tilde[i, ]);

      lambda_pfs_tilde[i, ] = rate_casemix_rng(cf_pfs[i], lambda_pfs[i, ], lambda_pfs_bg[i, ], n);
      t_pfs_tilde[i, ] = exponential_rng(lambda_pfs_tilde[i, ]);
    }",
    "\n}\n\n"
  )

  paste0(
    scode$functions,
    scode$data,
    scode$parameters,
    scode$generated_quantities)
}

