functions {
  int num_zero(int[] y) {
    int nz;
    nz = 0;
    for (n in 1:size(y))
      if (y[n] == 0)
        nz = nz + 1;
    return nz;
  }
}
data {
  int<lower=0> N;
  int<lower=0> y[N];
}
transformed data {
  int<lower=0, upper=N> N0;
  int<lower=0, upper=N> Ngt0;
  int<lower=1> y_nz[N - num_zero(y)];

  N0 = num_zero(y);
  Ngt0 = N - N0;
  {
    int pos;
    pos = 1;
    for (n in 1:N) {
      if (y[n] != 0) {
        y_nz[pos] = y[n];
        pos = pos + 1;
      }
    }
  }
}
parameters {
  real<lower=0, upper=1> theta;
  real<lower=0> lambda;
}
transformed parameters {
}
model {
  N0 ~ binomial(N, theta);
  y_nz ~ poisson(lambda);
  target += -Ngt0 * log1m_exp(-lambda);
}
generated quantities {
}
