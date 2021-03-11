
//
real normal_pdf (real t, real mu, real sigma) {
  real pdf;
  pdf = 1/(sigma*sqrt(2*pi())) * exp(-0.5*pow((t - mu)/sigma, 2));
  return pdf;
}


/**
* exponential distribution
*
* @param t time
* @param rate
* @return A real
*/

//  log hazard
real exp_log_h (real t, real rate) {
  real logh;
  logh = log(rate);
  return logh;
}

// exponential distribution hazard
real exp_haz (real t, real rate) {
  real h;
  h = rate;
  return h;
}

// exponential distribution log survival
// inbuilt exponential_lccdf(y | beta)
real exp_log_S (real t, real rate) {
  real logS;
  logS = -rate * t;
  return logS;
}

// exponential distribution survival
real exp_Surv (real t, real rate) {
  real S;
  S = exp(-rate * t);
  return S;
}

// exponential sampling distribution
real surv_exp_pdf (real t, real d, real rate) {
  real lik;
  lik = exp_haz(t, rate)^d * exp_Surv(t, rate);
  return lik;
}

// log exponential sampling distribution
real surv_exp_lpdf (real t, real d, real rate) {
  real log_lik;
  log_lik = d * exp_log_h(t, rate) + exp_log_S(t, rate);
  return log_lik;
}


/**
* weibull
*
* @param t time
* @param rate
* @return A real
*/

// log hazard
real weibull_log_h (real t, real shape, real scale) {
  real logh;
  logh = log(shape) + (shape - 1)*log(t/scale) - log(scale);
  return logh;
}

// hazard
real weibull_haz (real t, real shape, real scale) {
  real h;
  h = shape / scale * pow(t/scale, shape - 1);
  return h;
}

// weibull log survival
// inbuilt weibull_lccdf(y | alpha, sigma)
real weibull_log_S (real t, real shape, real scale) {
  real logS;
  logS = -pow(t/scale, shape);
  return logS;
}

// weibull survival
real weibull_Surv (real t, real shape, real scale) {
  real S;
  S = exp(-pow(t/scale, shape));
  return S;
}

// weibull sampling distribution
real surv_weibull_lpdf (real t, real d, real shape, real scale) {
  real log_lik;
  log_lik = d * weibull_log_h(t, shape, scale) + weibull_log_S(t, shape, scale);
  return log_lik;
}


/**
* gompertz
*
* @param t time
* @param rate
* @return A real
*/

// log hazard
real gompertz_log_h (real t, real shape, real scale) {
  real log_h;
  log_h = log(scale) + (shape * t);
  return log_h;
}

// hazard
real gompertz_haz (real t, real shape, real scale) {
  real h;
  h = scale*exp(shape*t);
  return h;
}

// gompertz log survival
real gompertz_log_S (real t, real shape, real scale) {
  real log_S;
  log_S = -scale/shape * (exp(shape * t) - 1);
  return log_S;
}

// gompertz survival
real gompertz_Surv (real t, real shape, real scale) {
  real S;
  S = exp(-scale/shape * (exp(shape * t) - 1));
  return S;
}

// gompertz sampling distribution
real surv_gompertz_lpdf (real t, real d, real shape, real scale) {
  real log_lik;
  log_lik = d * gompertz_log_h(t, shape, scale) + gompertz_log_S(t, shape, scale);
  return log_lik;
}


/**
* log-logistic
*
* @param t time
* @param shape
* @param scale
* @return A real
*/

// log hazard
real loglogistic_log_h (real t, real shape, real scale) {
 real log_h;
 log_h = log(shape) - log(scale) +
            (shape - 1)*(log(t) - log(scale)) -
            log(1 + pow(t/scale, shape));
 return log_h;
}

// hazard
real loglogistic_haz (real t, real shape, real scale) {
 real haz;
 haz = (shape/scale * pow(t/scale, shape - 1))/
         (1 + pow((t/scale), shape));
 return haz;
}

// log survival
real loglogistic_log_S (real t, real shape, real scale) {
 real log_S;
 log_S = -log(1 + pow(t/scale, shape));
 return log_S;
}

// survival
real loglogistic_Surv (real t, real shape, real scale) {
 real Surv;
 Surv = 1/(1 + pow(t/scale, shape));
 return Surv;
}

// sampling distribution
real surv_loglogistic_lpdf (real t, real d, real shape, real scale) {
  real log_lik;
  log_lik = d * loglogistic_log_h(t, shape, scale) + loglogistic_log_S(t, shape, scale);
  return log_lik;
}


/**
* generalised gamma
*
* @param t time
* @param mu location
* @param sigma scale
* @param Q shape
* @return Real
*/
// without censoring
real gen_gamma_lpdf(real t, real mu, real sigma, real Q) {
  real prob;
  real w;
  w = (log(t) - mu)/sigma;
  prob = -log(sigma*t) + log(fabs(Q)) + pow(Q, -2)*log(pow(Q, -2)) + pow(Q, -2)*(Q*w-exp(Q*w)) - lgamma(pow(Q, -2));
  return prob;
}

real gen_gamma_Surv(real t, real mu, real sigma, real Q) {
  real Surv;
  real w = (log(t) - mu) / sigma;
  real qq = 1/(Q * Q);
  real expnu = exp(Q * w) * qq;
  Surv =  1 - gamma_cdf(expnu, qq, 1);
  return Surv;
}

real gen_gamma_log_S(real t, real mu, real sigma, real Q) {
  real log_S;
  log_S = log(gen_gamma_Surv(t, mu, sigma, Q));
  return log_S;
}

real gen_gamma_log_h(real t, real mu, real sigma, real Q) {
  real log_h;
  log_h = gen_gamma_lpdf(t | mu, sigma, Q) - gen_gamma_log_S(t, mu, sigma, Q);
  return log_h;
}

real gen_gamma_haz(real t, real mu, real sigma, real Q) {
  real haz;
  haz = exp(gen_gamma_log_h(t, mu, sigma, Q));
  return haz;
}

real surv_gen_gamma_lpdf(real t, real d, real mu, real sigma, real Q) {
  // rescale the distribution accounting for right censoring
  real prob;
  real w;
  real tr;
  tr = t * d;
  w = (log(tr) - mu)/sigma;
  prob = log(d) - log(sigma*tr) + log(fabs(Q)) + pow(Q, -2)*log(pow(Q, -2)) + pow(Q, -2)*(Q*w - exp(Q*w)) - lgamma(pow(Q, -2));
  return prob;
}


/**
* log-normal
*
* @param t time
* @param mu
* @param sigma
* @return real
*/
// log survival
//lognormal_lccdf()
real lognormal_log_S (real t, real mu, real sigma) {
  real log_S;
  log_S = log(1 - Phi((log(t) - mu)/sigma));
  return log_S;
}

// log hazard
real lognormal_log_h (real t, real mu, real sigma) {
  real log_h;
  // log_h = -log(t*sigma) + normal_lpdf(log(t) | mu, sigma) - log(1 - Phi((log(t) - mu)/sigma));
  log_h = lognormal_lpdf(t | mu, sigma) - lognormal_lccdf(t | mu, sigma);
  return log_h;
}

// survival
//1 - lognormal_cdf()
real lognormal_Surv (real t, real mu, real sigma) {
 real Surv;
 Surv = 1 - Phi((log(t) - mu)/sigma);
 return Surv;
}

// hazard
real lognormal_haz (real t, real mu, real sigma) {
  real haz;
  haz = (1/(t*sigma)) * normal_pdf(log(t), mu, sigma) / (1 - Phi((log(t) - mu)/sigma));
  return haz;
}

// sampling distribution
real surv_lognormal_pdf (real t, real d, real mu, real sigma) {
  real lik;
  lik = pow(lognormal_haz(t, mu, sigma), d) * lognormal_Surv(t, mu, sigma);
  return lik;
}

real surv_lognormal_lpdf (real t, real d, real mu, real sigma) {
  real log_lik;
  log_lik = d * lognormal_log_h(t, mu, sigma) + lognormal_log_S(t, mu, sigma);
  return log_lik;
}


/**
* combined (non-cured) mortality
* background (all-cause) and cancer
*/

// alias for automated function name
 real joint_exp_exp_lpdf(real t, real d, real rate1, real rate2) {
  real lik;
  lik = surv_exp_lpdf(t | d, rate1 + rate2);
  return lik;
 }


// weibull

real joint_exp_weibull_pdf(real t, real d, real shape, real scale, real rate) {
  real lik;
  lik = exp_Surv(t, rate) * weibull_Surv(t, shape, scale) *
            pow(exp_haz(t, rate) + weibull_haz(t, shape, scale), d);
  return lik;
}

real joint_exp_weibull_lpdf(real t, real d, real shape, real scale, real rate) {
  real log_lik;
  log_lik = d * log(exp_haz(t, rate) + weibull_haz(t, shape, scale)) +
            exp_log_S(t, rate) + weibull_log_S(t, shape, scale);
  return log_lik;
}

real exp_weibull_Surv(real t, real shape, real scale, real rate) {
  real Surv;
  Surv = exp_Surv(t, rate) * weibull_Surv(t, shape, scale);
  return Surv;
}

// gompertz

real joint_exp_gompertz_pdf(real t, real d, real shape, real scale, real rate) {
  real lik;
  lik = exp_Surv(t, rate) * gompertz_Surv(t, shape, scale) *
            pow(exp_haz(t, rate) + gompertz_haz(t, shape, scale), d);
  return lik;
}

real joint_exp_gompertz_lpdf(real t, real d, real shape, real scale, real rate) {
  real log_lik;
  log_lik = d * log(exp_haz(t, rate) + gompertz_haz(t, shape, scale)) +
            exp_log_S(t, rate) + gompertz_log_S(t, shape, scale);
  return log_lik;
}

real exp_gompertz_Surv(real t, real shape, real scale, real rate) {
  real Surv;
  Surv = exp_Surv(t, rate) * gompertz_Surv(t, shape, scale);
  return Surv;
}

// log-logistic

real joint_exp_loglogistic_pdf(real t, real d, real shape, real scale, real rate) {
  real lik;
  lik = exp_Surv(t, rate) * loglogistic_Surv(t, shape, scale) *
            pow(exp_haz(t, rate) + loglogistic_haz(t, shape, scale), d);
  return lik;
}

real joint_exp_loglogistic_lpdf(real t, real d, real shape, real scale, real rate) {
  real log_lik;
  log_lik = d * log(exp_haz(t, rate) + loglogistic_haz(t, shape, scale)) +
            exp_log_S(t, rate) + loglogistic_log_S(t, shape, scale);
  return log_lik;
}

real exp_loglogistic_Surv(real t, real shape, real scale, real rate) {
  real Surv;
  Surv = exp_Surv(t, rate) * loglogistic_Surv(t, shape, scale);
  return Surv;
}

// log-normal

real joint_exp_lognormal_pdf(real t, real d, real mu, real sigma, real rate) {
  real lik;
  lik = exp_Surv(t, rate) * lognormal_Surv(t, mu, sigma) *
        pow(exp_haz(t, rate) + lognormal_haz(t, mu, sigma), d);
  return lik;
}

real joint_exp_lognormal_lpdf(real t, real d, real mu, real sigma, real rate) {
  real log_lik;
  log_lik = d * log(exp_haz(t, rate) + lognormal_haz(t, mu, sigma)) +
            exp_log_S(t, rate) + lognormal_log_S(t, mu, sigma);
  return log_lik;
}

real exp_lognormal_Surv(real t, real mu, real sigma, real rate) {
  real Surv;
  Surv = exp_Surv(t, rate) * lognormal_Surv(t, mu, sigma);
  return Surv;
}

real exp_lognormal_rng(real mu, real sigma, real lambda) {
 real t_latent[2];
 real t_min;
 t_latent[1] = exponential_rng(lambda);
 t_latent[2] = lognormal_rng(mu, sigma);
 t_min = min(t_latent);
 return t_min;
}


// generalised gamma

real joint_exp_gengamma_pdf(real t, real d, real mu, real scale, real Q, real rate) {
  real lik;
  lik = exp_Surv(t, rate) * gen_gamma_Surv(t, mu, scale, Q) *
            pow(exp_haz(t, rate) + gen_gamma_haz(t, mu, scale, Q), d);
  return lik;
}

real joint_exp_gengamma_lpdf(real t, real d, real mu, real scale, real Q, real rate) {
  real log_lik;
  log_lik = d * log(exp_haz(t, rate) + gen_gamma_haz(t, mu, scale, Q)) +
            exp_log_S(t, rate) + gen_gamma_log_S(t, mu, scale, Q);
  return log_lik;
}

real exp_gengamma_Surv(real t, real mu, real scale, real Q, real rate) {
  real Surv;
  Surv = exp_Surv(t, rate) * gen_gamma_Surv(t, mu, scale, Q);
  return Surv;
}

