/* Mixture of Gaussians model
 * 
 * Note: for inference of posteriors in a mixture model,
 * the priors need to be non-exchangeable
 *
 */


data {
  int<lower=0> N;  // number of data points
  int<lower=0> K;  // number of Gaussians
  int<lower=0> J;  // number of predictors
  vector<lower=0, upper=2>[N] y;  // response
  vector[K] d_hyp;  // hyperparameter vector for Dirichlet
  real<lower=0> var_hyp;  // hyperparameter for prior variances
  // matrix[N,J] X;  // model matrix 
}

parameters {
  // matrix[J,J] b;  // regression weights
  // real[J] b0;  // regression biases
  ordered[K] mu;  // locations of Gaussians
  vector<lower=0>[K] sigma;  // scales of Gaussians
  simplex[K] theta;  // assignment probabilities
}
/*
transformed parameters {
  vector[N] y_regr;
  for (n in 1:N) {
    y_regr[] = b[X[n,1], X[n,2]] + b0[n];
  }
}  
*/
model {
  real ps[K];

  mu[1] ~ normal(0.3, var_hyp);
  mu[2] ~ normal(0.8, var_hyp);
  mu[3] ~ normal(1.4, var_hyp);

  sigma ~ normal(0, 0.1);
  theta ~ dirichlet(d_hyp);

  for (n in 1:N) {
    for (k in 1:K) {
      ps[k] = log(theta[k]) + normal_lpdf(y[n] | mu[k], sigma[k]);
    }
    target += log_sum_exp(ps);  // smooth approximation of max()
  }
}

generated quantities {
  vector[K] mu_pred;
  vector[K] w_pred;
  real y_pred;
  vector[N] log_lik;

  for (k in 1:K) {
    mu_pred[k] = normal_rng(mu[k], sigma[k]);
  }
  w_pred = dirichlet_rng(theta);
  y_pred = dot_product(w_pred, mu_pred);

  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | y_pred, var_hyp);
  }
}

