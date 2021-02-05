//
// ////TODO: stand-alone posterior predictions ----
// // match input data case-mix?
// // https://mc-stan.org/docs/2_23/stan-users-guide/stand-alone-generated-quantities-and-ongoing-prediction.html
//
// functions {
//
//   // return sample mean rates
//   real rate_mean_rng(real curefrac, real lambda0, real lambda_bg, int n) {
//
//     real lambda[n];
//     real cf[n];
//
//     for (i in 1:n) {
//       cf[i] = uniform_rng(0, 1);
//
//       if (cf[i] < curefrac) {
//         lambda[i] = lambda_bg;
//       } else {
//         lambda[i] = lambda_bg + lambda0;
//       }
//     }
//     return lambda;
//   }
//
// //TODO: why am I getting a type mismatch error??
//   // return sample casemix rates
//   vector rate_casemix_rng(real curefrac, vector lambda0, vector lambda_bg) {
//
//     // int n = num_elements(lambda0);
//     // vector[n] lambda;
//     // real cf[n];
//     //
//     // for (i in 1:n) {
//     //   cf[i] = uniform_rng(0, 1);
//     //
//     //   if (cf[i] < curefrac) {
//     //     lambda[i] = 1;
//     //   } else {
//     //     lambda[i] = 1;
//     //   }
//     // }
//     return lambda0;
//
//   }
//
// }
//
// data {
//   int<lower=1> n;
// }
//
// parameters {
//   real<lower=0, upper=1> cf_os;
//   real<lower=0, upper=1> cf_pfs;
//   real lambda_os[n];
//   real lambda_pfs[n];
//   real lambda_os_bg[n];
//   real lambda_pfs_bg[n];
// }
//
// generated quantities {
//   real lambda_os_tilde[n];
//   real<lower=0> t_os_tilde[n];
//   real lambda_pfs_tilde[n];
//   real<lower=0> t_pfs_tilde[n];
//
//   // lambda_os_tilde = rate_casemix_rng(cf_os, lambda_os, lambda_os_bg);
//   t_os_tilde = exponential_rng(lambda_os_tilde);
//
//   // lambda_pfs_tilde = rate_casemix_rng(cf_pfs, lambda_pfs, lambda_pfs_bg);
//   t_pfs_tilde = exponential_rng(lambda_pfs_tilde);
// }
//
