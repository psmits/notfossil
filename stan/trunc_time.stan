data {
  int<lower=0> N;  // # observations
  int T; // observation windows

  int<lower=0> y[N];  // value observed
  int t[N];  // window for observation
}
parameters {
  real mu_int[T];

  real<lower=0> phi;

  real<lower=0> sigma_mu;
}
transformed parameters {
  real<lower=0> mu[T];

  mu = exp(mu_int);  // expand for covariates
}
model {
  mu_int[1] ~ normal(0, 1);  // initial conditions
  for(i in 2:T) 
    mu_int[i] ~ normal(mu_int[i - 1], sigma_mu);

  sigma_mu ~ normal(0, 1);

  phi ~ normal(0, 1);

  for(n in 1:N) {
    y[n] ~ neg_binomial_2(mu[t[n]], phi) T[1, ];
  }
}

