/* 
   Linear regression

*/

data {
  int<lower=0> J;  // number of rows
  int<lower=0> K;  // number of columns
  real y[J];
  matrix[J,K] X;   // model matrix
}

parameters {
  vector[K] coeff;
  real<lower=0> sigma;
}

model {
  // priors
  sigma ~ normal(0, 10);
  coeff[1] ~ cauchy(0, 10);
  for (i in 2:K) {
    coeff[i] ~ cauchy(0, 2.5);
  }

  // likelihood
  y ~ normal(X * coeff, sigma);
}

