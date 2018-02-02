data {
  int<lower=0> N;  // # observations
  int T;  // observation windows
  int K;  // number covariates

  int<lower=0> y[N];  // value observed
  int t[N];  // window for observation
  matrix[N, K] X;
}
parameters {
  vector[K] beta[T];
  
  //vector[K] mu;
  vector[K] mu[T];
  vector<lower=0>[K] sigma_mu;  // rw sd
  corr_matrix[K] Omega;
  vector<lower=0>[K] tau;

  real<lower=0> phi;
}
transformed parameters {
  vector[N] location;
  cov_matrix[K] Sigma;

  for(i in 1:N) {
    location[i] = exp(X[i] * beta[t[i]]);
  }
  Sigma = quad_form_diag(Omega, tau);
}
model {
  // rw prior for all covariates incl intercept
  for(k in 1:K) {
    mu[1, k] ~ normal(0, sigma_mu[k]);
    for(j in 2:T) {
      mu[j, k] ~ normal(mu[j - 1, k], sigma_mu[k]);
    }
  }
  //mu ~ normal(0, 1);
  Omega ~ lkj_corr(2);
  tau ~ normal(0, 1);
  sigma_mu ~ normal(0, 1);

  beta ~ multi_normal(mu, Sigma);
  
  phi ~ normal(0, 1);

  for(n in 1:N) 
    y[n] ~ neg_binomial_2(location[n], phi) T[1, ];
}
