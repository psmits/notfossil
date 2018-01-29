data {
  int<lower=0> N;  // # observations
  int T; // observation windows

  int<lower=0> y[N];  // value observed
  int t[N];  // window for observation
}
parameters {
  real theta_int[T];
  real lambda_int[T];

  real<lower=0> phi;

  real<lower=0> sigma_theta;
  real<lower=0> sigma_lambda;
}
transformed parameters {
  real<lower=0, upper=1> theta[T];
  real<lower=0> lambda[T];

  theta = inv_logit(theta_int);  // expand for covariates
  lambda = exp(theta_int);  // expand for covariates
}
model {
  theta_int[1] ~ normal(0, 1);  // initial conditions
  for(i in 2:T) 
    theta_int[i] ~ normal(theta_int[i - 1], sigma_theta);

  lambda_int[1] ~ normal(0, 1);  // initial conditions
  for(i in 2:T) 
    theta_int[i] ~ normal(lambda_int[i - 1], sigma_lambda);

  phi ~ normal(0, 1);

  for(n in 1:N) {
    if(y[n] == 0) {
      target += log(theta[t[n]]);
    } else {
      target += neg_binomial_2_lpmf(y[n] | lambda[t[n]], phi) 
        - neg_binomial_2_lccdf(0 | lambda[t[n]], phi);
    }
  }
}
