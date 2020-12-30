
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

  vector<lower=0> lambda_tilde[n] =
    rate_mean_rng(curefrac, lambda0, lambda_bg, n)
  vector<lower=0> t_tilde[n] = exponential_rng(lambda_tilde)
}

