
/**
* exponential distribution log hazard
*
* @param t time
* @param rate
* @return A real
*/
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
real surv_exp_pdf (real t, data real d, real rate) {
  real lik;
  lik = exp_haz(t, rate)^d * exp_Surv(t,rate);
  return lik;
}

// log exponential sampling distribution
real surv_exp_lpdf (real t, data real d, real rate) {
  real log_lik;
  log_lik = d * exp_log_h(t, rate) + exp_log_S(t, rate);
  return log_lik;
}


/**
* weibull log hazard
*
* @param t time
* @param rate
* @return A real
*/
real weibull_log_h (real t, real shape, real scale) {
  real logh;
  logh = log(shape) + (shape - 1)*log(t/scale) - log(scale);
  return logh;
}

// weibull log survival
real weibull_log_S (real t, real shape, real scale) {
  real logS;
  logS = -pow((t/scale), shape);
  return logS;
}

// weibull survival
real weibull_Surv (real t, real alpha, real beta) {
  real S;
  S = exp(-(pow(t/alpha, beta)));
  return S;
}

// weibull sampling distribution
real surv_weibull_lpdf (real t, data real d, real shape, real scale) {
  real log_lik;
  log_lik = d * weibull_log_h(t, shape, scale) + weibull_log_S(t, shape, scale);
  return log_lik;
}


/**
* gompertz log hazard
*
* @param t time
* @param rate
* @return A real
*/
real gompertz_log_h (real t, real shape, real rate) {
  real log_h;
  log_h = log(rate) + (shape * t);
  return log_h;
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
real surv_gompertz_lpdf (real t, data real d, real shape, real rate) {
  real log_lik;
  real prob;
  log_lik = d * gompertz_log_h(t,shape,rate) + gompertz_log_S(t,shape,rate);
  return log_lik;
}

