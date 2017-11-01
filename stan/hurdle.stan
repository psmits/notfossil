data {
  int N;  // number of attempts at order observation at units
  int O;  // number of orders
  int U;  // number of units
  int K;  // number of unit covariates

  int y[N];  // counts at each attempted observation 
  int o[N];  // order membership of each attempted observation
  int u[N];  // unit membership of each attempted observation
  
  matrix[U, K] X;  // covariates for each unit

  vector[U] exposure;
}
parameters {
  real the;  // intercept of theta
  real lam;  // intercept of lambda

  vector[U] unit_eff_the;
  real<lower=0> unit_scale_the;
  vector[U] unit_eff_lam;
  real<lower=0> unit_scale_lam;

  vector[O] order_eff_the;
  real<lower=0> order_scale_the;
  vector[O] order_eff_lam;
  real<lower=0> order_scale_lam;
}
transformed parameters {
  vector<lower=0, upper=1>[N] theta;
  vector<lower=0>[N] lambda;

  theta[1:N] = inv_logit(the + unit_eff_the[u] + order_eff_the[o]);
  lambda[1:N] = exp(lam + unit_eff_lam[u] + order_eff_lam[o] + log(exposure[u]));
}
model {
  the ~ normal(1, 1);
  lam ~ normal(0, 1);

  unit_eff_the ~ normal(0, unit_scale_the);
  unit_scale_the ~ normal(0, 1);
  unit_eff_lam ~ normal(0, unit_scale_lam);
  unit_scale_lam ~ normal(0, 1);

  order_eff_the ~ normal(0, order_scale_the);
  order_scale_the ~ normal(0, 1);
  order_eff_lam ~ normal(0, order_scale_lam);
  order_scale_lam ~ normal(0, 1);

  for(n in 1:N) {
    if(y[n] == 0)
      target += log(theta[n]);
    else {
      target += log1m(theta[n]) + poisson_lpmf(y[n] | lambda[n]) 
        - log1m_exp(-lambda[n]);
    }
  }
}
