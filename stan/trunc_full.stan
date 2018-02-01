data {
  int<lower=0> N;  // # observations
  int T;  // observation windows
  int D;  // number taxonomic groups
  int U;  // number of unique units

  int<lower=0> y[N];  // value observed
  int t[N];  // window for observation
  int d[N];  // taxonomic membership
  int u[N];  // unit membership
}
parameters {
  real mu_int[T];  // time varying intercept
  vector[D] taxon_eff;  // taxonomic group
  vector[U] unit_eff;  // geologic unit

  real<lower=0> phi;

  real<lower=0> sigma_mu;
  real<lower=0> sigma_unit;
  real<lower=0> sigma_taxon;
}
transformed parameters {
  real<lower=0> mu[N];
  
  // expand for covariates
  for(n in 1:N) {
    mu[n] = exp(mu_int[t[n]] + taxon_eff[d[n]] + unit_eff[u[n]]);  
  }
}
model {
  mu_int[1] ~ normal(0, 1);  // initial conditions
  for(i in 2:T) 
    mu_int[i] ~ normal(mu_int[i - 1], sigma_mu);

  sigma_mu ~ normal(0, 1);

  phi ~ normal(0, 1);

  taxon_eff ~ normal(0, sigma_taxon);
  sigma_taxon ~ normal(0, 1);
  unit_eff ~ normal(0, sigma_unit);
  sigma_unit ~ normal(0, 1);

  for(n in 1:N) 
    y[n] ~ neg_binomial_2(mu[n], phi) T[1, ];
}
