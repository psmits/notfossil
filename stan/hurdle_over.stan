data {
  int N;  // number of attempts at order observation at units
  int Nz;
  int Nnz;
  int K;  // number of unit covariates

  int y[N];  // counts at each attempted observation 
  int ynz[Nnz];  // counts at each attempted observation 

  matrix[N, K] X;  // covariates for each unit
  matrix[Nnz, K] Xnz;

  int zi[N];  // 1 if zero, 0 if not
}
parameters {
  real the;  // intercept of theta
  real lam;  // intercept of lambda

  vector[K] beta_the;
  vector<lower=0>[K] the_indiv;
  real<lower=0> the_global;
  
  vector[K] beta_lam;
  vector<lower=0>[K] lam_indiv;
  real<lower=0> lam_global;

  real<lower=0> phi;
}
transformed parameters {
  vector<lower=0, upper=1>[N] theta;
  vector<lower=0>[Nnz] lambda;

  theta[1:N] = inv_logit(the + X[1:N, ] * beta_the);
  lambda[1:Nnz] = exp(lam + Xnz[1:Nnz, ] * beta_lam);
}
model {
  the ~ normal(2, 1);
  lam ~ normal(0, 1);

  beta_the ~ normal(0, the_indiv * the_global);
  beta_lam ~ normal(0, lam_indiv * lam_global);

  the_indiv ~ cauchy(0, 1);
  the_global ~ cauchy(0, 1);
  lam_indiv ~ cauchy(0, 1);
  lam_global ~ cauchy(0, 1);
  
  phi ~ normal(0, 1);
  
  zi ~ bernoulli(theta);
  target += neg_binomial_2_lpmf(ynz | lambda, phi) 
    - neg_binomial_2_lccdf(0 | lambda, phi);
}
generated quantities {
  vector[N] lambda_est;  // for simulations

  for(n in 1:N) {
    lambda_est[n] = exp(lam + X[n] * beta_lam);
  }
}

