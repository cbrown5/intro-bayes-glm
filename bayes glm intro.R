#Hi this is the intro GLM course! 


library(brms)
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
theme_set(theme_classic())

pois_glm_sim <- function(params, x, seed = runif(1, 0, 100000)){
  set.seed(seed)
  
  with(params,{
    lambda = exp(alpha + beta*x)
    y = rpois(length(x), lambda)
    data.frame(id = 1:length(y), alpha, beta, x, lambda, y)
  })
  
}

set.seed(55)
N <- 50
params <- data.frame(alpha = 0,
                     beta = 0.15, 
                     theta = 0.15)
sst_range <- c(8, 25)
sst <- runif(N, sst_range[1], sst_range[2])
sstnorm <- scale(sst)
dat <- pois_glm_sim(params, x = sst,
                    seed = 42)

ggplot(dat) +
  aes(x = x, y = y) +
  geom_point()

m1 <- glm(y ~ x + x2, family = "poisson",
          data = dat)
pred_m1 <- predict(m1,
                   se = TRUE)
coef(m1)

dat$pred_glm <- pred_m1$fit
dat$pred_glm_se <- pred_m1$se.fit

g1 <- ggplot(dat) +
  aes(x = x, y = y) +
  geom_point() +
  geom_line(aes(y = exp(pred_glm)), color = "red") +
  geom_ribbon(aes(ymin = exp(pred_glm - pred_glm_se*1.96),
                  ymax = exp(pred_glm + pred_glm_se*1.96)),
              alpha = 0.5,
              fill = "red")

g1

options(mc.cores = parallel::detectCores())

b1 <- brm(y ~ x +x2, family = "poisson",
          data = dat,
          chains = 4,
          iter = 2000,
          thin = 1,
          warmup = 1000,
          cores = 4)

get_prior(y ~ x +x2, family = "poisson",
          data = dat)

summary(b1)
plot(b1)

sims <- extract(b1)

shinystan::launch_shinystan(b1)

conditional_effects(b1)

pred_b1 <- posterior_epred(b1)
dim(pred_b1)

hist(pred_b1[,1])
plot(density(pred_b1[,1]^2))
quantile(pred_b1[,1], c(0.025, 0.5, 0.975))

quantile(pred_b1[,1]*0.5, c(0.025, 0.5, 0.975))
quantile(pred_b1[,1]^2, c(0.025, 0.5, 0.975))

sum(pred_b1[,1]>12)/nrow(pred_b1)

plot(dat$x, apply(pred_b1,2,function(x) sum(x>12)/4000))

dat$pred_brm <- colMeans(pred_b1)
dat$pred_brm_lwr <- apply(pred_b1, 2,
                          quantile, probs = 0.025)
dat$pred_brm_upr <- apply(pred_b1, 2,
                          quantile, probs = 0.975)
g2 <- ggplot(dat) +
  aes(x = x, y = y) +
  geom_point() +
  geom_line(aes(y = pred_brm), color = "blue") +
  geom_ribbon(aes(ymin = pred_brm_lwr,
                  ymax = pred_brm_upr),
              alpha = 0.5,
              fill = "blue")
g2

g1 + g2

a1 <- conditional_effects(b1)
a1$x

library(rstan)
options(mc.cores = parallel::detectCores())

dat_in <- list(N = nrow(dat), 
               y = dat$y,
               x = dat$x)

compiled_model <- stan_model("glm-poisson-sim.stan")

sim_out <- sampling(
  compiled_model, data = dat_in)

prior_sims  <- sim_out %>%
  as.data.frame %>%
  select(contains("y_sim"))

summary_tbl <- apply(prior_sims[1:5,], 1, summary)
summary_tbl
mean(prior_sims[1,])
prior_sims[1:100,1]

exp(0 + 1*max(dat$x))

get_prior(y ~ x, family = "poisson",
          data = dat)

dat$xscale <- scale(dat$x)
hist(dat$xscale)

exp(0 + 1*max(dat$xscale))

dat_in <- list(N = nrow(dat), y = dat$y,
               x = as.numeric(dat$xscale))

sim_out <- sampling(
  compiled_model, data = dat_in)

prior_sims  <- sim_out %>%
  as.data.frame %>%
  select(contains("y_sim"))

summary_tbl <- apply(prior_sims[1:5,], 1, summary)
summary_tbl

sims_dat <- prior_sims[1:15,] %>%
  t() %>%
  data.frame() %>%
  mutate(x = dat_in$x) %>%
  pivot_longer(-x, names_to = "sim",
               values_to = "y")

ggplot(sims_dat) +
  aes(x = x,y = y) +
  geom_point() +
  facet_wrap(~sim, scale = "free")

#set_priors

dat_in <- list(N = nrow(dat),
               y = dat$y,
               x = as.numeric(dat$xscale))
fit1 <- stan(file = "glm-poisson.stan",
             data = dat_in,
             iter = 2000,
             warmup = 1000,
             thin = 1,
             chains = 4,
             refresh = 500)
xout <- rstan::extract(fit1)
names(xout)
quantile(xout$alpha, c(0.025, 0.975))
plot(density(xout$alpha))
sum(xout$beta>0.1)/4000

pois_glm_sim <- function(params, x, seed = runif(1, 0, 100000)){
  set.seed(seed)
  
  with(params,{
    lambda = exp(alpha + beta*x)
    y = rpois(length(x), lambda)
    data.frame(id = 1:length(y), alpha, beta, x, lambda, y)
  })
  
}

listparams <- Map(function(alpha,beta) data.frame(alpha,
                                                  beta),
                  xout$alpha,
                  xout$beta)
length(listparams)
listparams[[1]]
pois_glm_sim(listparams[[1]], x = dat_in$x)
datpred <- lapply(listparams, pois_glm_sim,
                  x = dat$xscale) %>%
  do.call("rbind", .)


ggplot(datpred) +
  aes(x = x, y = lambda) +
  geom_line()


## Random effects models 

pois_glmm_sim <- function(params, x,
                          N_per_group,
                          ngrps,
                          seed = runif(1,0, 10000)){
  
  set.seed(seed)
  
  #total sample size
  N <- N_per_group * ngrps
  
  with(params, {
    #random effects
    z <- rnorm(ngrps, 0, sigma)
    Z <- rep(z, each = N_per_group)
    
    #poisson mean
    lambda <- exp(alpha + beta*x + Z)
    y <- rpois(N, lambda)
    data.frame(id = 1:length(y),
               x, lambda, y,
               #just a trick to convert Z into integer site IDs:
               siteID = as.numeric(factor(Z)))
  })
  
}

ngrps <- 12
N_per_group <- 5
x <- runif(ngrps, sst_range[1], sst_range[2])
x <- scale(x)
x <- rep(x, each = N_per_group)

params <- list(alpha = 2.5,
               beta = 0.75,
               sigma = 0.3)

set.seed(77)
dat2 <- pois_glmm_sim(params, x,
                      N_per_group, ngrps,
                      77)

ggplot(dat2) +
  aes(x = x, y = y, color = factor(siteID)) +
  geom_point()


dat_in <- list(N = nrow(dat2), y = dat2$y,
               x = dat2$x,
               igroup = dat2$siteID,
               K = length(unique(dat2$siteID)))

fitRE <- stan(file = "glm-poisson-random-effects.stan",
              data = dat_in,
              iter = 2000,
              warmup = 1000,
              thin = 1,
              chains =  4,
              refresh = 500)

fit_summary <- summary(fitRE, pars = c("alpha", "beta", "sigma"),
                       probs = c(0.033, 0.5, 0.975))
fit_summary$summary

b1ME <- brm(y ~ x + (1|siteID), family = "poisson",
            data = dat2,
            chains = 4,
            iter = 2000,
            thin = 1,
            warmup = 1000,
            cores = 4)
plot(b1ME)
summary(b1ME)

conditional_effects(b1ME, 
                    re_formula = NULL) 


ranef(b1ME)
b1z <- ranef(b1ME, groups = "siteID")$siteID %>%
  data.frame() %>%
  mutate(i = 1:n())

ggplot(b1z) +
  aes(x = i) +
  geom_point(aes(y = Estimate.Intercept)) +
  geom_linerange(aes(ymin = Q2.5.Intercept,
                     ymax = Q97.5.Intercept))

qqnorm(b1z$Estimate.Intercept)
qqline(b1z$Estimate.Intercept, lty = 2)

hist(rgamma(1000000, 1, 0.001))

nprec <- 100000
prec <- seq(0.05, 1000, length.out = nprec) #precisions
sd <- 1/sqrt(prec)
U <- 1
alpha <- 0.025
lambda <- -log(alpha)/U #rate for exponential on sd
pr_prec <- inla.pc.dprec(prec, U, alpha) #Density for the precision. 
pr_sd <- dexp(sd, rate = lambda) #Density for sd


b1_priors <- get_prior(y ~ x + (x|siteID), family = "poisson",
                       data = dat2)
View(b1_priors)
prior_sigma <- set_prior("gamma(10, 0.001)",
                         class = "sd", coef = "Intercept",
                         group = "siteID")
make_stancode(y ~ x + (1|siteID), family = "poisson",
              data = dat2,
              prior = prior_sigma,
              save_model = "dumb-prior.stan")

b1ME_gamma001 <- brm(y ~ x + (1|siteID), family = "poisson",
                     data = dat2,
                     chains = 4,
                     iter = 2000,
                     thin = 1,
                     warmup = 1,
                     cores = 4,
                     prior = prior_sigma)

summary(b1ME_gamma001)

plot(b1ME_gamma001)



tighter_prior <-
  set_prior("normal(-5,0.1)", class = "b", coef = "x")
b2 <- brm(y ~ x, family = "poisson",
          data = dat,
          chains = 4,
          iter = 2000,
          thin = 1,
          warmup = 1000,
          cores = 4,
          prior = tighter_prior)
summary(b2)
conditional_effects(b2)
conditional_effects(b1)

pred_b2 <- posterior_epred(b2)
dat$pred_brm2 <- colMeans(pred_b2)
dat$pred_brm_lwr2 <- apply(pred_b2, 2,
                           quantile, probs = 0.025)
dat$pred_brm_upr2 <- apply(pred_b2, 2,
                           quantile, probs = 0.975)

ggplot(dat) +
  aes(x = x, y = y) +
  geom_point() +
  geom_line(aes(y = pred_brm), color = "blue") +
  geom_ribbon(aes(ymin = pred_brm_lwr,
                  ymax = pred_brm_upr),
              alpha = 0.5,
              fill = "blue") +
  geom_line(aes(y = pred_brm2), color = "green") +
  geom_ribbon(aes(ymin = pred_brm_lwr2,
                  ymax = pred_brm_upr2),
              alpha = 0.5,
              fill = "green")


pois_gamm_sim <- function(params, x,
                          N_per_group,
                          ngrps,
                          seed = runif(1,0, 10000)){
  
  set.seed(seed)
  
  #total sample size
  N <- N_per_group * ngrps
  
  with(params, {
    #random effects
    xs <- splines::ns(x,3)
    z <- rnorm(ngrps, 0, sigma)
    Z <- rep(z, each = N_per_group)
    
    #poisson mean
    lambda <- exp(alpha + beta1*xs[,1] + beta2*xs[,2] + beta3*xs[,3] + Z)
    y <- rpois(N, lambda)
    data.frame(id = 1:length(y),
               x, lambda, y,
               #just a trick to convert Z into integer site IDs:
               siteID = as.numeric(factor(Z)))
  })
  
}


params <- list(alpha = 1,
               beta1 = -1,
               beta2 = 3,
               beta3 = -1,
               sigma = 0.2)

set.seed(77)
dat3 <- pois_gamm_sim(params, x,
                      N_per_group, ngrps,
                      77)

ggplot(dat3) +
  aes(x = x, y = y, color = factor(siteID)) +
  geom_point()

b1gam1 <- brm(y ~ s(x) + (x|siteID), family = "poisson",
              data = dat3,
              chains = 4,
              iter = 2000,
              thin = 1,
              warmup = 1000,
              cores = 4)
summary(b1gam1)
plot(b1gam1)

conditional_effects(b1gam1)

