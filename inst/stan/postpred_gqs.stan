
////TODO: stand-alone posterior predictions ----
// match input data case-mix?
// https://mc-stan.org/docs/2_23/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html

functions {

  // return sample mean rates
  real[] rate_mean_rng(real curefrac, real lambda0, real lambda_bg, int n) {

    real lambda[n];
    real cf[n];

    for (i in 1:n) {
      cf[i] = uniform_rng(0, 1);

      if (cf[i] < curefrac) {
        lambda[i] = lambda_bg;
      } else {
        lambda[i] = lambda_bg + lambda0;
      }
    }
    return lambda;
  }

  real[] rate_casemix_rng(real curefrac, real[] lambda0, real[] lambda_bg) {

    int n = num_elements(lambda0);
    real lambda[n];
    real cf[n];

    for (i in 1:n) {
      cf[i] = uniform_rng(0, 1);

      if (cf[i] < curefrac) {
        lambda[i] = lambda_bg[i];
      } else {
        lambda[i] = lambda_bg[i] + lambda0[i];
      }
    }
    return lambda;
  }

}

data {
  int<lower=1> n;  // num individuals

  real<lower=1> os_model;
  real<lower=1> pfs_model;
}

parameters {
  real<lower=0, upper=1> cf_os;
  real<lower=0, upper=1> cf_pfs;

  real<lower=0> lambda_os[os_model == 1 ? n : 0];
  real<lower=0> lambda_pfs[pfs_model == 1 ? n : 0];
  real<lower=0> lambda_os_bg[n];
  real<lower=0> lambda_pfs_bg[n];

  // // using means
  // real beta_os[2];
  // real beta_pfs[2];
  // real beta_bg[2];
}

generated quantities {
  real<lower=0> lambda_os_tilde[os_model == 1 ? n : 0] ;
  real<lower=0> t_os_tilde[os_model == 1 ? n : 0];
  real<lower=0> lambda_pfs_tilde[pfs_model == 1 ? n : 0];
  real<lower=0> t_pfs_tilde[pfs_model == 1 ? n : 0];
    print(lambda_os_bg);

  if (os_model == 1) {

    lambda_os_tilde = rate_casemix_rng(cf_os, lambda_os, lambda_os_bg);

    // lambda_os_tilde = rate_mean_rng(cf_os, lambda_os[1], lambda_os_bg[1], n);
    t_os_tilde = exponential_rng(lambda_os_tilde);

    lambda_pfs_tilde = rate_casemix_rng(cf_pfs, lambda_pfs, lambda_pfs_bg);
    // lambda_pfs_tilde = rate_mean_rng(cf_pfs, lambda_pfs[1], lambda_pfs_bg[1], n);
    t_pfs_tilde = exponential_rng(lambda_pfs_tilde);
  }
}

