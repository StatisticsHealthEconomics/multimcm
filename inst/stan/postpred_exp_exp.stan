
////TODO: stand-alone posterior predictions ----
// match input data case-mix?
// https://mc-stan.org/docs/2_23/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html

functions {

    // return sample mean rates
    real rate_mean_rng(real curefrac, real lambda0, real lambda_bg, int n) {

    vector[n] lambda;
    real ncf;

    ncf = binomial_rng(n, curefrac);
    lambda[1:ncf] = lambda_bg;
    lambda[(ncf + 1):n] = lambda_bg + lambda0;
    return(lambda);
  }

  //   // return sample case-mix rates
  //   real rate_casemix_rng(real curefrac, vector lambda0, vector lambda_bg, int n) {
  //
  //   vector[n] lambda;
  //   real U;
  //
  //   for (i in 1:n) {
  //     U = bernoulli_rng(curefrac);
  //     lambda[i] = U ? lambda_bg[i] : lambda_bg[i] + lambda0[i];
  //   }
  //   return(lambda);
  // }

  // real exp_casemix_rng(real curefrac, vector lambda0, vector lambda_bg, int n) {
  //
  //   vector[n] t;
  //
  //   for (i in 1:n) {
  //     //TODO: how is this vectorised over posterior draws?
  //     real U = uniform_rng(0,1);
  //
  //     if (curefrac > U) {
  //       t[i] = exponential_rng(lambda_bg[i]);
  //     } else {
  //       t[i] = exponential_rng(lambda_bg[i] + lambda0[i]);
  //     }
  //   }
  //   return(t);
  // }

  // real rate_casemix_rng(real curefrac, vector lambda0, vector lambda_bg, int n) {
  //
  //   vector[n] lambda;
  //
  //   for (i in 1:n) {
  //     // same U for all posterior samples for given individual
  //     real U = uniform_rng(0,1);
  //
  //     if (curefrac > U) {
  //       lambda[i] = lambda_bg[i];
  //     } else {
  //       lambda[i] = lambda_bg[i] + lambda0[i];
  //     }
  //   }
  //   return(lambda);
  // }
}

data {
  int<lower=1> n;
}

parameters {
  real<lower=0, upper=1> curefrac;
  real<lower=0> lambda0;
  real<lower=0> lambda_bg;
}

generated quantities {

  // // single step
  // vector<lower=0> t_tilde[n] =
  //   exp_casemix_rng(curefrac, lambda0, lambda_bg, n)

  // // keep individual lambdas
  // vector<lower=0> lambda_tilde[n] =
  //   rate_casemix_rng(curefrac, lambda0, lambda_bg, n)
  // vector<lower=0> t_tilde[n] = exponential_rng(lambda_tilde)

  // keep mean lambdas
  vector<lower=0> lambda_tilde[n] =
    rate_mean_rng(curefrac, lambda0, lambda_bg, n)
  vector<lower=0> t_tilde[n] = exponential_rng(lambda_tilde)
}

