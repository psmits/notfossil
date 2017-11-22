data {
  int N_train;
  int N_test;
  int Nz_train;
  int Nnz_train;

  int K;  // number of unit covariates

  int y_train[N_train];  // counts at each attempted observation 
  int ynz_train[Nnz_train];  // counts at each attempted observation 

  matrix[N_train, K] X_train;  // covariates for each unit
  matrix[Nnz_train, K] Xnz_train;
  matrix[N_test, K] X_test;  // covariates for each unit

  int zi_train[N_train];  // 1 if zero, 0 if not
}
parameters {
  real the;  // intercept of theta
  real lam;  // intercept of lambda

  vector[K] beta_the;
  //vector<lower=0>[K] the_indiv;
  //real<lower=0> the_global;

  vector[K] beta_lam;
  //vector<lower=0>[K] lam_indiv;
  //real<lower=0> lam_global;
  
  real<lower=0> phi;
}
transformed parameters {
  vector<lower=0, upper=1>[N_train] theta;
  vector<lower=0>[Nnz_train] lambda;

  theta[1:N_train] = inv_logit(the + X_train[1:N_train, ] * beta_the);
  lambda[1:Nnz_train] = exp(lam + Xnz_train[1:Nnz_train, ] * beta_lam);
}
model {
  the ~ normal(2, 1);
  lam ~ normal(0, 1);

  beta_the ~ normal(0, 1);
  beta_lam ~ normal(0, 1);

  //the_indiv ~ cauchy(0, 1);
  //the_global ~ cauchy(0, 1);
  //lam_indiv ~ cauchy(0, 1);
  //lam_global ~ cauchy(0, 1);
  
  phi ~ normal(0, 1);
  
  zi_train ~ bernoulli(theta);
  target += neg_binomial_2_lpmf(ynz_train | lambda, phi) 
    - neg_binomial_2_lccdf(0 | lambda, phi);
}
generated quantities {
  vector[N_train] lambda_est;  // for trainset simulations
  vector[N_test] theta_test; // estimated for testset
  vector[N_test] lambda_test;  // estimated for testset

  for(n in 1:N_train) {
    lambda_est[n] = exp(lam + X_train[n] * beta_lam);
  }

  for(n in 1:N_test) {
    theta_test[n] = inv_logit(the + X_test[n, ] * beta_the);
    lambda_test[n] = exp(lam + X_test[n, ] * beta_lam);
  }
}

