library(xtable)
library(rstan)
library(loo)




######################################
## 1) Predictive information criteria
######################################

## Eight schools

y <- c(28,8,-3,7,-1,1,18,12)
sigma <- c(15,10,16,11,9,11,10,18)
J <- 8

data <- list(y = y, sigma=sigma, J = J)
fit_1 <- stan("8schools.stan", 
            data = data, iter=200,
            cores = 4, chains =4)

fit_2 <- stan("8schools_invgamma.stan", 
              data = data, iter=200,
              cores = 4, chains =4)

fit_3 <- stan("8schools_halfcauchy.stan", 
              data = data, iter=200,
              cores = 4, chains =4)

data_fake <- list(y = y, sigma=sigma, J = J, tau =0.001)
fit_4 <- stan("8schools_fake.stan", 
              data = data_fake, iter=200,
              cores = 4, chains =4)



sims_1 <- extract(fit_1)
posterior_1 <- as.matrix(fit_1)

sims_2 <- extract(fit_2)
posterior_2 <- as.matrix(fit_2)

sims_3 <- extract(fit_3)
posterior_3 <- as.matrix(fit_3)

sims_4 <- extract(fit_4)
posterior_4 <- as.matrix(fit_4)

# Compute LOO
log_lik_1 <- extract_log_lik(fit_1)
loo_1 <- loo(log_lik_1)
print(loo_1)

log_lik_2 <- extract_log_lik(fit_2)
loo_2 <- loo(log_lik_2)
print(loo_2)

log_lik_3 <- extract_log_lik(fit_3)
loo_3 <- loo(log_lik_3)
print(loo_3)

log_lik_4 <- extract_log_lik(fit_4)
loo_4 <- loo(log_lik_4)
print(loo_4)

loo_diff <- compare(loo_1, loo_2, loo_3)

#  compute waic

waic_1 <- waic(log_lik_1)
waic_2 <- waic(log_lik_2)
waic_3 <- waic(log_lik_3)

waic_diff <- compare(waic_1, waic_2, waic_3)




