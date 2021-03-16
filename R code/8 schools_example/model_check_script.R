library(rstan)
library(ggplot2)
library(rstanarm)
library(bayesplot)
library(dplyr)
library(lubridate)

########################
## Eight schools
########################


y <- c(28,8,-3,7,-1,1,18,12)
sigma <- c(15,10,16,11,9,11,10,18)
J <- 8

data <- list(y = y, sigma=sigma, J = J)
fit <- stan("8schools.stan", data = data, iter=200,
            cores = 4, chains =4)
sims <- extract(fit)
posterior <- as.matrix(fit)

theta_names <- paste0("theta[", 1:8, "]")
mu_names <- expression(mu)
tau_names <- expression(tau)
par_names <- c(mu_names, tau_names, theta_names)

mcmc_intervals(posterior, regex_pars = c("theta"))
pdf(file="8schools_areas.pdf", width=8, height =7)
mcmc_areas(posterior, regex_pars=c("mu", "tau", "theta"))+
  scale_y_discrete(labels = rev((parse(text= par_names))))+
  xaxis_text(on =TRUE, size=22)+
  yaxis_text(on =TRUE, size=22)
dev.off()

# check 1: density

pdf(file="8schools_dens.pdf", width=8, height =7)
ppc_dens_overlay(y, sims$y_rep)+
  xaxis_text(on =TRUE, size=22)+
  legend_text(size=rel(4))
dev.off()

# check 2: ecdf

pdf(file="8schools_ecdf.pdf", width=8, height =7)
ppc_ecdf_overlay(y, sims$y_rep)+
  xaxis_text(on =TRUE, size=22)+
  legend_text(size=rel(4))
dev.off()

# check 3: 

pdf(file="8schools_intervals.pdf", width=8, height =7)
ppc_intervals(y, sims$y_rep)+
  xaxis_text(on =TRUE, size=22)+
  yaxis_text(on =TRUE, size=22)+
  labs(x="Schools")+
  theme(axis.title.x = element_text( size=22))+
  legend_text(size=rel(4))
dev.off()

# check 4: stats

pdf(file="8schools_mean.pdf", width=5, height=5)
ppc_stat(y, sims$y_rep, stat="mean")+
  xaxis_text(on =TRUE, size=22)+
  theme(axis.title.x = element_text( size=22))+
  legend_text(size=rel(1.6))
dev.off()
pdf(file="8schools_sd.pdf", width=5, height=5)
ppc_stat(y, sims$y_rep, stat="sd")+
  xaxis_text(on =TRUE, size=22)+
  theme(axis.title.x = element_text( size=22))+
  legend_text(size=rel(1.6))
dev.off()
pdf(file="8schools_median.pdf", width=5, height=5)
ppc_stat(y, sims$y_rep, stat="median")+
  xaxis_text(on =TRUE, size=22)+
  theme(axis.title.x = element_text( size=22))+
  legend_text(size=rel(1.6))
dev.off()
pdf(file="8schools_max.pdf", width=5, height=5)
ppc_stat(y, sims$y_rep, stat="max")+
  xaxis_text(on =TRUE, size=22)+
  theme(axis.title.x = element_text( size=22))+
  legend_text(size=rel(1.6))
dev.off()

# check 5: bivariate stats

pdf(file="8schools_bivstats.pdf", width =5, height =5)
ppc_stat_2d(y, sims$y_rep)+
  xaxis_text(on =TRUE, size=22)+
  yaxis_text(on =TRUE, size=22)+
  theme(axis.title.x = element_text( size=22))+
  legend_text(size=rel(1.6))
dev.off()
pdf(file="8schools_bivstats2.pdf", width =5, height =5)
ppc_stat_2d(y, sims$y_rep, c("median", "max"))+
  xaxis_text(on =TRUE, size=22)+
  yaxis_text(on =TRUE, size=22)+
  theme(axis.title.x = element_text( size=22))+
  legend_text(size=rel(1.6))
dev.off()

# check 6: binned residuals


ppc_error_binned(y, sims$y_rep[1:8,])


