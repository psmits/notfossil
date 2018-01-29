data {
  int<lower=0> N;  // # observations
  int T; // observation windows

  int<lower=0> y[N];  // value observed
  int t[N];  // window for observation
}
parameters {
  real theta_int[T];
  real lambda_int[T];

  real<lower=0> sigma_theta;
  real<lower=0> sigma_lambda;
}
transformed parameters {
  real<lower=0, upper=1> theta[T];
  real<lower=0> lambda[T];

  theta = inv_logit(theta_int);
  lambda = exp(theta_int);
}
model {
  theta_int[1] ~ normal(0, sigma_theta);
  for(i in 2:T) 
    theta_int[i] ~ normal(0, sigma_theta);

  lambda_int[1] ~ normal(0, sigma_lambda);
  for(i in 2:T) 
    theta_int[i] ~ normal(0, sigma_lambda);


  for(n in 1:N) {
    if(y[n] == 0) {
      target += log(theta[t[n]]);
    } else {
      target += log1m(theta[t[n]]) + poisson_lpmf(y[n] | lambda[t[n]])
        - log1m_exp(-lambda[t[n]]);
    }
  }
}
