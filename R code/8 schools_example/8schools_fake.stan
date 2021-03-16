data {
  int<lower=0> J; // number of schools 
  real y[J]; // estimated treatment effects
  real<lower=0> sigma[J]; // s.e. of effect estimates 
  real<lower=0> tau;
}
parameters {
  real mu; 
  real eta[J];
}
transformed parameters {
  real theta[J];
  for (j in 1:J)
    theta[j] = mu + tau * eta[j];
}
model {
  target += normal_lpdf(eta | 0, 1);
  target += normal_lpdf(y | theta, sigma);
}
generated quantities{
  real y_rep[J];
  vector[J] log_lik;
  for (j in 1:J){
     y_rep[j] = normal_rng(theta[j], sigma[j]);
     log_lik[j] = normal_lpdf(y[j]| theta[j], sigma[j]);
  }
}
