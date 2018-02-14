data {
  int<lower=0> N;  // # observations
  int T;  // observation windows
  int K;  // number covariates

  int<lower=0> y[N];  // value observed
  int t[N];  // window for observation
  matrix[N, K] X;
}
parameters {
  matrix[T, K] mu_raw;  // group-prior
  //vector<lower=0>[K] sigma_mu;  // rw sd
  vector<lower=0>[K] sigma_mu;  // rw sd
  
  cholesky_factor_corr[K] L_Omega;  // chol corr
  matrix[K, T] z;  // for non-centered mv-normal
  vector<lower=0>[K] tau;  // scales of cov
  
  real<lower=0> phi;  // over disperssion
}
transformed parameters {
  matrix[T, K] mu;  // group-prior
  matrix[T, K] beta;  // regression coefficients time X covariate
  vector[N] location;  // put on right support
  
  // rw prior for all covariates incl intercept
  // this is non-centered which adds parameter but can improve sampling
  for(k in 1:K) {
    //mu[1, k] = 0 + sigma_mu[k] * mu_raw[1, k];
    mu[1, k] = mu_raw[1, k];
    for(j in 2:T) {
      mu[j, k] = mu[j - 1, k] + sigma_mu[k] * mu_raw[1, k];
    }
  }
  
  // non-centered mvn
  beta = mu + (diag_pre_multiply(tau, L_Omega) * z)';

  // matrix algebra
  location = exp(rows_dot_product(beta[t], X));
}
model {
  // rw prior
  to_vector(mu_raw) ~ normal(0, 2);
  sigma_mu ~ normal(0, 2);
  
  // effects
  to_vector(z) ~ normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  tau ~ normal(0, 2);

  phi ~ normal(0, 0.5);

  for(n in 1:N) 
    y[n] ~ neg_binomial_2(location[n], phi) T[1, ];
}

