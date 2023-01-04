//TODO:

// // check prior sensible
//
// data {
//
// }
//
// parameters {
//
// }
//
// model {
//
// }
//
// generated quantities {
//   int<lower=0> y[100];
//
//   real a_mu = 0;
//   real<lower=0> b_mu = 1;
//   real a_scale = 0;
//   real<lower=0> b_scale = 1;
//   real a_Q = 0;
//   real<lower=0> b_Q = 1;
//
//   real scale[100];
//   real Q[100];
//   real mu[100];
//
//   for (n in 1:100) {
//     scale[n] = lognormal_rng(a_scale, b_scale);
//     Q[n] = normal_rng(a_Q, b_Q);
//     mu[n] = normal_rng(a_mu, b_mu);
//
//     y[n] = gengamma_rng(mu[n], scale[n], Q[n]);
//   }
// }
