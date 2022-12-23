// check prior sensible

data {

}

parameters {
  real a_mu;
  real<lower=0> b_mu;
  real a_scale;
  real<lower=0> b_scale;
  real a_Q;
  real<lower=0> b_Q;

}

model {
  real scale;
  real Q;
  real mu;

  scale ~ lognormal(a_scale, b_scale);
  Q ~ normal(a_Q, b_Q);
  mu ~ normal(a_mu, b_mu);
}

generated quantities {
  int<lower=0> y[100];

  for (n in 1:100)
    y[n] = gengamma_rgn(mu, scale, Q);
}
