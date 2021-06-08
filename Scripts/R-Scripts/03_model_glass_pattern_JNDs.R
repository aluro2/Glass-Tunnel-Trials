
# Load packages and data --------------------------------------------------
library(tidyverse)
library(brms)
library(modelr)
library(ggthemr)

MatchedData <-
  read_rds("Data/MatchedData.rds")

# Model flight avoidance vs. pattern JNDs ---------------------------------
model <-
  brm(
    cont | trials(total) ~ (visual_contrast*pat_width) + first_surf + (1|season_yr_cond),
    data = MatchedData,
    family = binomial(link = "logit"),
    prior = c(prior(normal(0, 0.25), class = Intercept, coef = ""),
              prior(cauchy(0, 10), class = sd),
              prior(normal(0, 3), class = b)),
    iter = 2000,
    inits = "random",
    chains = 4,
    cores = 4,
    thin = 1,
    seed = 15,
    control = list(adapt_delta = 0.95),
    sample_prior = "yes",
    save_all_pars = TRUE
  )

write_rds(model, "Results/full-model.rds")


# Simple visual contrast model --------------------------------------------

model_vis_contrast <-
  brm(
    bf(cont | trials(total) ~ visual_contrast + (1|season_yr_cond) +(1|manufacturer)),
    data = MatchedData,
    family = binomial(link = "logit"),
    prior = c(prior(normal(0, 0.25), class = Intercept, coef = ""),
              prior(cauchy(0, 10), class = sd),
              prior(normal(0, 3), class = b)),
    iter = 2000,
    inits = "random",
    chains = 4,
    cores = 4,
    thin = 1,
    seed = 15,
    control = list(adapt_delta = 0.95),
    sample_prior = "yes",
    save_all_pars = TRUE
  )

write_rds(model_vis_contrast, "Results/model-vis-contrast.rds")


# TEST --------------------------------------------------------------------

get_variables(model)

source("https://sebastiansauer.github.io/Rcode/logit2prob.R")

prob2logit <-
  function(x){
    log(x/(1-x))
  }

# Beta-binomial model -----------------------------------------------------
{
  beta_binomial2 <-
    custom_family(
      "beta_binomial2", dpars = c("mu", "phi"),
      links = c("logit", "log"), lb = c(NA, 0),
      type = "int", vars = "trials[n]"
    )

  stan_funs <-
  "real beta_binomial2_lpmf(int y, real mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
  int beta_binomial2_rng(real mu, real phi, int T) {
    return beta_binomial_rng(T, mu * phi, (1 - mu) * phi);
  }"

  stanvars <-
    stanvar(scode = stan_funs, block = "functions")
}

model_beta <-
  brm(
    cont | trials(total) ~ (visual_contrast*pat_width)*first_surf,
    data = MatchedData,
    family = beta_binomial2,
    prior = c(prior(normal(0, 2), class = Intercept),
              prior(exponential(1), class = phi)),
    stanvars = stanvars,
    iter = 2000,
    inits = "random",
    chains = 4,
    cores = 4,
    thin = 1,
    seed = 15,
    sample_prior = "yes", save_all_pars = TRUE
  )

expose_functions(model_beta, vectorize = T)

log_lik_beta_binomial2 <- function(i, prep) {
  mu <- prep$dpars$mu[, i]
  phi <- prep$dpars$phi
  trials <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  beta_binomial2_lpmf(y, mu, phi, trials)
}

posterior_predict_beta_binomial2 <- function(i, prep, ...) {
  mu <- prep$dpars$mu[, i]
  phi <- prep$dpars$phi
  trials <- prep$data$vint1[i]
  beta_binomial2_rng(mu, phi, trials)
}

posterior_epred_beta_binomial2 <- function(prep) {
  mu <- prep$dpars$mu
  trials <- prep$data$vint1
  trials <- matrix(trials, nrow = nrow(mu), ncol = ncol(mu), byrow = TRUE)
  mu * trials
}

conditional_effects(model_beta, conditions = data.frame(size = 1))
