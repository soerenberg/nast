// Eight schools model
data {
  int<lower=0> N;          // num groups
  real y[N];               // observations
  real<lower=0> sigma[N];  // group std dev
}
parameters {
  real alpha;              // population mean
  real<lower=0> gamma;     // population std dev
  vector[N] mu;            // uncentered group means
}
transformed parameters {
  vector[N] mu_cent = alpha + gamma * mu;  // centered group means
}
model {
  // prior model
  mu ~ normal(0, 1);

  // likelihood model
  y  ~ normal(mu_cent, sigma);
}
