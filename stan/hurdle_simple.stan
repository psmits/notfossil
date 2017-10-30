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
  real<lower=0, upper=1> theta;

  vector[O] h1;  // order effect
  real<lower=0> scale_h1;  // scale of order effect
  vector[C] h2;  // class effect
  real<lower=0> scale_h2;  // scale of class effect
  vector[P] h3;  // phylum effect
  real<lower=0> scale_h3;  // scale of phylum effect

  real lam;
}
transformed parameters {
  vector<lower=0>[Nnz] lambda;

  lambda = exp(lam + h1[o]);
}
model {
  h1 ~ normal(h2[c], scale_h1);
  scale_h1 ~ normal(0, 1);
  h2 ~ normal(h3[p], scale_h2);
  scale_h2 ~ normal(0, 1);
  h3 ~ normal(0, scale_h3);
  scale_h3 ~ normal(0, 1);

  lam ~ normal(0, 1);

  Nz ~ binomial(N, theta);
  ynz ~ poisson(lambda);
  target += -Nnz * log1m_exp(-lambda);
}
