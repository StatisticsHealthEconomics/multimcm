
data {
  int<lower=0> n;             // number of observations
  vector[n] d;                // censoring indicator (1 = observed, 0 = censored)
  vector[n] S_hat;
  vector[n] h_hat;

  real a_alpha;
  real b_alpha;
}

parameters {
  real alpha;
}

transformed parameters {
}

model {
  alpha ~ gamma(a_alpha, b_alpha);

  // likelihood
  for (i in 1:n) {
    target += alpha*log(S_hat[i]) + d[i]*log(h_hat[i] * alpha);
  }
}

generated quantities {
}
