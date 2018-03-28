data {
  int<lower=0> N;  // # observations
  int T;  // observation windows
  int K;  // number covariates
  int S;  // number of taxonomic groups
  int U;  // number of geologic units

  int<lower=0> y[N];  // value observed
  int t[N];  // window for observation
  matrix[N, K] X[S];
  
  int s[N];  // taxonomic membership
  int u[N];  // geologic unit membership
}
parameters {
  matrix[T, K] mu_raw[S];  // group-prior
  vector<lower=0>[K] sigma_mu[S];  // rw sd
  
  cholesky_factor_corr[K] L_Omega[S];  // chol corr
  matrix[K, T] z[S];  // for non-centered mv-normal
  vector<lower=0>[K] tau[S];  // scales of cov
  
  vector<lower=0>[S] phi;  // over disperssion

//  vector[S] taxa;
//  real<lower=0> sigma_taxa;
//  vector[U] unit;
//  real<lower=0> sigma_unit;

}
transformed parameters {
  matrix[T, K] mu[S];  // group-prior
  matrix[T, K] beta[S];  // regression coefficients time X covariate
  matrix[S, N] location;  // put on right support
  
  // rw prior
  for(i in 1:S) {
    for(k in 1:K) {
      mu[i, 1, k] = mu_raw[i, 1, k];
      for(j in 2:T) {
        mu[i, j, k] = mu[i, j - 1, k] + sigma_mu[i, k] * mu_raw[i, j, k];
      }
    }
  }
  
  // non-centered mvn
  for(i in 1:S) {
    beta[i] = mu[i] + (diag_pre_multiply(tau[i], L_Omega[i]) * z[i])';
    location[i] = exp(rows_dot_product(beta[i], X[i])');
  }

  // matrix algebra
}
model {
  for(i in 1:S) {
    mu_raw[i][1, ] ~ normal(3, 3);
    to_vector(mu_raw[i][2:T, ]) ~ normal(0, 1);
    //to_vector(mu_raw) ~ normal(0, 1);
    sigma_mu[i] ~ normal(0, 1);
  
    to_vector(z[i]) ~ normal(0, 1);
    L_Omega[i] ~ lkj_corr_cholesky(2);
    tau[i] ~ normal(0, 1);
  }
  

  phi ~ normal(0, 5);

//  taxa ~ normal(0, sigma_taxa);
//  sigma_taxa ~ normal(0, 1);
//  
//  unit ~ normal(0, sigma_unit);
//  sigma_unit ~ normal(0, 1);

  for(n in 1:N) 
    y[n] ~ neg_binomial_2(location[s[n], n], phi[s[n]]) T[1, ];
}
