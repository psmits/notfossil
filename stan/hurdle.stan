data {
  int<lower=0> Nz;  // num zeroes
  int<lower=0> yz[Nz];  // vector of zeroes
  int uz[Nz];  // unit membership for zeroes

  int<lower=0> Nnz;  // num non-zeroes
  int<lower=0> ynz[Nnz];  // vector of non-zeroes
  int unz[Nnz];  // unit membership for non-zeroes

  int Nu;  // number of units
  int K;  // number of unit covariates
  matrix[Nu, K] X;  // matrix of unit covariates

  int O;  // number of orders
  int o[Nnz];  // order membership
  int C;  // number of classes
  int c[O];  // class membership
  int P;  // number of phyla
  int p[C];  // phylum membership
}
transformed data {
  int N;
  int<lower=0> yones[Nz];  // vector of zeroes 1s

  N = Nz + Nnz;

  for(i in 1:Nz) {
    yones[i] = 1;
  }
}
parameters {
  vector[K] beta_theta;
  // horseshoe prior on effects of unit covariates
  vector<lower=0>[K] shrink_theta_local;
  real<lower=0> shrink_theta_global;
  
  vector[K] beta_lambda;
  // horseshoe prior on effects of unit covariates
  vector<lower=0>[K] shrink_lambda_local;
  real<lower=0> shrink_lambda_global;

  vector[O] h1;  // order effect
  real<lower=0> scale_h1;  // scale of order effect
  vector[C] h2;  // class effect
  real<lower=0> scale_h2;  // scale of class effect
  vector[P] h3;  // phylum effect
  real<lower=0> scale_h3;  // scale of phylum effect
}
transformed parameters {
  // assemble vectors of covariates and effects
  vector<lower=0, upper=1>[N] theta;
  vector<lower=0>[Nnz] lambda;

  theta[1:Nz] = inv_logit(X[uz, ] * beta_theta);
  theta[(Nz+1):(N)] = inv_logit(X[unz, ] * beta_theta);

  lambda = exp(X[unz, ] * beta_lambda + h1[o]);
}
model {
  //beta_theta ~ normal(0, 1);
  // horseshoe prior on effects of unit covariates
  beta_theta ~ normal(0, shrink_theta_local * shrink_theta_global);
  shrink_theta_local ~ cauchy(0, 1);
  shrink_theta_global ~ cauchy(0, 1);

  //beta_lambda ~ normal(0, 1);
  // horseshoe prior on effects of unit covariates
  beta_lambda ~ normal(0, shrink_lambda_local * shrink_lambda_global);
  shrink_lambda_local ~ cauchy(0, 1);
  shrink_lambda_global ~ cauchy(0, 1);

  // taxonomic elements
  // this is currently a vary-intercept element
  // something to consider is making beta vary by taxonomy
  h1 ~ normal(h2[c], scale_h1);
  scale_h1 ~ normal(0, 1);
  h2 ~ normal(h3[p], scale_h2);
  scale_h2 ~ normal(0, 1);
  h3 ~ normal(0, scale_h3);
  scale_h3 ~ normal(0, 1);

  // modified from the stan manual
  //yz ~ bernoulli(theta);
  yones[1:Nz] ~ bernoulli(theta[1:Nz]);  // use vectors ones because 1 means no fossil
  for(i in 1:Nnz) {
    0 ~ bernoulli(theta[Nz + i]);
    ynz[i] ~ poisson(lambda[i]) T[1, ];
  }
  //target += -Nnz * log1m_exp(-lambda);
}
generated quantities {
  // calculate log-lik 
  vector[N] log_lik;
  //// posterior predictive simulations
  //vector[N] y_tilde;
  for(i in 1:Nz) {
    log_lik[i] = bernoulli_lpmf(1 | theta[i]);
  }
  for(j in 1:Nnz) {
    log_lik[Nz + j] = bernoulli_lpmf(0 | theta[Nz + j]);
    log_lik[Nz + j] = log_lik[Nz + j] + poisson_lpmf(ynz[j] | lambda[j]);
    log_lik[Nz + j] = log_lik[Nz + j] - poisson_lccdf(0 | lambda[j]);
  }
}
