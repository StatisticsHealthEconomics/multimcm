
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
real gompertz_log_h (real t, real shape, real rate) {
  real log_h;
  log_h = log(rate) + (shape * t);
  return log_h;
}

// hazard
real gompertz_haz (real t, real shape, real rate) {
  real h;
  h = rate*exp(shape*t);
  return h;
}

// gompertz log survival
real gompertz_log_S (real t, real shape, real rate) {
  real log_S;
  log_S = -rate/shape * (exp(shape * t) - 1);
  return log_S;
}

// gompertz survival
real gompertz_Surv (real t, real shape, real rate) {
  real S;
  S = exp(-rate/shape * (exp(shape * t) - 1));
  return S;
}

// gompertz sampling distribution
real surv_gompertz_lpdf (real t, real d, real shape, real rate) {
  real log_lik;
  log_lik = d * gompertz_log_h(t, shape, rate) + gompertz_log_S(t, shape, rate);
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
* @param shape
* @param scale
* @return A real
*/
real gen_gamma_lpdf(vector x, vector mu, real sigma, real Q) {
  // Uses the same parameterisation as flexsurv
  // mu = location
  // sigma = scale
  // Q = shape
  vector[num_elements(x)] prob;
  real lprob;
  vector[num_elements(x)] w;
  // Constructs the log-density for each observation
  w = ((log(x)-mu))/sigma;
  for (i in 1:num_elements(x)) {
    prob[i] = -log(sigma*x[i]) + log(fabs(Q)) + pow(Q, -2)*log(pow(Q, -2)) + pow(Q, -2)*(Q*w[i]-exp(Q*w[i])) - lgamma(pow(Q, -2));
  }
  // And the total log-density (as a sum of the individual terms)
  lprob = sum((prob));
  return lprob;
}

real gen_gamma_cens_lpdf(vector x, vector mu, real sigma, real Q, vector u) {
  // Rescales the distribution accounting for right censoring
  vector[num_elements(x)] prob;
  real lprob;
  vector[num_elements(x)] w;
  vector[num_elements(x)] tr;
  // Constructs the log-density for each observation
  tr = x .* u;
  w = ((log(tr)-mu))/sigma;
  for (i in 1:num_elements(x)) {
    prob[i] = log(u[i]) - log(sigma*tr[i]) + log(fabs(Q)) + pow(Q, -2)*log(pow(Q, -2)) + pow(Q, -2)*(Q*w[i] - exp(Q*w[i])) - lgamma(pow(Q, -2));
  }
  // And the total log-density (as a sum of the individual terms)
  lprob = sum((prob));
  return lprob;
}



/**
* log-normal
*
* @param t time
* @param shape
* @param scale
* @return A real
*/
// Defines the log survival
vector log_S (vector t, vector mean, real sd) {
  vector[num_elements(t)] log_S;
  for (i in 1:num_elements(t)) {
    log_S[i] = log(1 - Phi((log(t[i]) - mean[i])/sd));
  }
  return log_S;
}

// Defines the log hazard
vector log_h (vector t, vector mean, real sd) {
  vector[num_elements(t)] log_h;
  vector[num_elements(t)] ls;
  ls = log_S(t, mean, sd);
  for (i in 1:num_elements(t)) {
    log_h[i] = lognormal_lpdf(t[i]|mean[i],sd) - ls[i];
  }
  return log_h;
}

// Defines the sampling distribution
real surv_lognormal_lpdf (vector t, vector d, vector mean, real sd) {
  vector[num_elements(t)] log_lik;
  real prob;
  log_lik = d .* log_h(t, mean, sd) + log_S(t, mean, sd);
  prob = sum(log_lik);
  return prob;
}


/**
* combined (non-cured) mortality
* background (all-cause) and cancer
*/

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

real joint_exp_lognormal_pdf(real t, real d, real shape, real scale, real rate) {
  real lik;
  return lik;
}

real joint_exp_lognormal_lpdf(real t, real d, real shape, real scale, real rate) {
  real log_lik;
  return log_lik;
}

real exp_lognormal_Surv(real t, real shape, real scale, real rate) {
  real Surv;
  return Surv;
}

// generalised gamma

real joint_exp_gengamma_pdf(real t, real d, real shape, real scale, real rate) {
  real lik;
  return lik;
}

real joint_exp_gengamma_lpdf(real t, real d, real shape, real scale, real rate) {
  real log_lik;
  return log_lik;
}

real exp_gengamma_Surv(real t, real shape, real scale, real rate) {
  real Surv;
  return Surv;
}

