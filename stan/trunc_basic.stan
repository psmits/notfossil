data {
  int<lower=0> N;  // # observations
  int T;  // observation windows
  int K;  // number covariates

  int<lower=0> y[N];  // value observed
  int t[N];  // window for observation
  matrix[N, K] X;
}
parameters {
  vector[K] mu;
  corr_matrix[K] Omega;
  vector<lower=0>[K] tau;

  vector[K] beta[T];

  real<lower=0> phi;
}
transformed parameters {
}
model {
  mu ~ normal(0, 1);
  Omega ~ lkj_corr(2);
  tau ~ normal(0, 1);

  beta ~ multi_normal(mu, quad_form_diag(Omega, tau));
  
  phi ~ normal(0, 1);

  for(n in 1:N) 
    y[n] ~ neg_binomial_2(exp(X[n] * beta[t[n]]), phi) T[1, ];
}

