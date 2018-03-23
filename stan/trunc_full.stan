data {
  int<lower=0> N;  // # observations
  int T;  // observation windows
  int K;  // number covariates
  int S;  // number of taxonomic groups
  int U;  // number of geologic units

  int<lower=0> y[N];  // value observed
  int t[N];  // window for observation
  matrix[N, K] X;
  
  int s[N];  // taxonomic membership
  int u[N];  // geologic unit membership
}
parameters {
  matrix[T, K] mu_raw;  // group-prior
  vector<lower=0>[K] sigma_mu;  // rw sd
  
  cholesky_factor_corr[K] L_Omega;  // chol corr
  matrix[K, T] z;  // for non-centered mv-normal
  vector<lower=0>[K] tau;  // scales of cov
  
  real<lower=0> phi;  // over disperssion

  vector[S] taxa;
  real<lower=0> sigma_taxa;
  vector[U] unit;
  real<lower=0> sigma_unit;

}
transformed parameters {
  matrix[T, K] mu;  // group-prior
  matrix[T, K] beta;  // regression coefficients time X covariate
  vector[N] location;  // put on right support
  
  // rw prior
  for(k in 1:K) {
    mu[1, k] = mu_raw[1, k];
    for(j in 2:T) {
      mu[j, k] = mu[j - 1, k] + sigma_mu[k] * mu_raw[j, k];
    }
  }
  
  // non-centered mvn
  beta = mu + (diag_pre_multiply(tau, L_Omega) * z)';

  // matrix algebra
  for(n in 1:N) {
    location[n] = exp((beta[t[n]] * X[n, ]') + taxa[s[n]] + unit[u[n]]);
  }
}
model {
  mu_raw[1, ] ~ normal(3, 3);
  to_vector(mu_raw[2:T, ]) ~ normal(0, 1);
  //to_vector(mu_raw) ~ normal(0, 1);
  sigma_mu ~ normal(0, 1);
  
  // effects
  to_vector(z) ~ normal(0, 1);
  L_Omega ~ lkj_corr_cholesky(2);
  tau ~ normal(0, 1);

  phi ~ normal(0, 5);

  taxa ~ normal(0, sigma_taxa);
  sigma_taxa ~ normal(0, 1);
  
  unit ~ normal(0, sigma_unit);
  sigma_unit ~ normal(0, 1);

  for(n in 1:N) 
    y[n] ~ neg_binomial_2(location[n], phi) T[1, ];
}
