data {
  int<lower=0> N;  // # observations
  int T;  // observation windows
  int K;  // number covariates

  int<lower=0> y[N];  // value observed
  int t[N];  // window for observation
  matrix[N, K] X;
}
parameters {
  corr_matrix[K] Omega;
  vector<lower=0>[K] tau; 
  vector[K] beta[T];
  vector[K] mu;
  
  real<lower=0> phi;  // over disperssion
}
transformed parameters {
  vector<lower=0>[N] location;
  // matrix algebra
  for(i in 1:N)
    location[i] = exp(X[t[i]] * beta[t[i]]);
}
model {
  mu ~ normal(0, 1);
  Omega ~ lkj_corr(2);
  tau ~ normal(0, 1);

  beta ~ multi_normal(mu, quad_form_diag(Omega, tau));

  phi ~ normal(0, 1);

  for(n in 1:N) 
    y[n] ~ neg_binomial_2(location[n], phi) T[1, ];
}
