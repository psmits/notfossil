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
  vector[D] mu[T]; 
  vector[D] taxon_series[T]; 
  vector<lower=0>[D] sigma_mu;
  corr_matrix[D] Omega;
  vector<lower=0>[D] tau;

  vector[U] unit_eff;
  real<lower=0> sigma_unit;

  real<lower=0> phi;
}
transformed parameters {
  vector[N] location;
  cov_matrix[D] Sigma;

  for(n in 1:N)
    location[n] = exp(mu[t[n], d[n]] + unit_eff[u[n]]);

  Sigma = quad_form_diag(Omega, tau);
}
model {
  for(i in 1:D) {
    taxon_series[1, i] ~ normal(0, 1);  // initial conditions
    for(j in 2:T) {
      taxon_series[j, i] ~ normal(taxon_series[j - 1, i], sigma_mu[i]);
    }
  }
  mu ~ multi_normal(taxon_series, Sigma);
  sigma_mu ~ normal(0, 1);
  
  Omega ~ lkj_corr(2);
  tau ~ normal(0, 1);

  phi ~ normal(0, 1);

  sigma_unit ~ normal(0, 1);

  for(n in 1:N) 
    y[n] ~ neg_binomial_2(location[n], phi) T[1, ];
}

