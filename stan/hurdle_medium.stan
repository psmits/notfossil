data {
  int N;  // number of attempts at order observation at units
  int Nnz;
  int K;  // number of unit covariates

  int y[N];  // counts at each attempted observation 

  matrix[N, K] X;  // covariates for each unit
}
transformed data {
}
parameters {
  real the;  // intercept of theta
  real lam;  // intercept of lambda

  vector[K] beta_the;
  vector[K] beta_lam;
}
transformed parameters {
  vector<lower=0, upper=1>[N] theta;

  theta[1:N] = inv_logit(the + X[1:N, ] * beta_the);
}
model {
  the ~ normal(2, 1);
  lam ~ normal(0, 1);

  beta_the ~ normal(0, 1);
  
  for(n in 1:N) {
    if(y[n] == 0)
      target += log(theta[n]);
      //1 ~ bernoulli(theta[n]);
    else {
      target += log1m(theta[n]) + poisson_lpmf(y[n] | exp(lam + X[n, ] * beta_lam))
        - log1m_exp(-(exp(lam + X[n, ] * beta_lam)));
      //0 ~ bernoulli(theta[n]);
      //y[n] ~ poisson(exp(lam + X[n, ] * beta_lam)) T[1, ];
    }
  }
}
