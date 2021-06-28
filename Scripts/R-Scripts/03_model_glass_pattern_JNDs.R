
# Load packages and data --------------------------------------------------
library(tidyverse)
library(brms)
library(modelr)
library(ggthemr)

MatchedData <-
  read_rds("Data/MatchedData.rds")

# Run model(s) ------------------------------------------------------------

run_brms <- function(model){
  model <-
    brm(
      model,
      data = MatchedData,
      family = binomial(link = "logit"),
      prior = c(prior(normal(0, 0.25), class = Intercept, coef = ""),
                prior(cauchy(0, 10), class = sd),
                prior(normal(0, 3), class = b)),
      iter = 5000,
      inits = "random",
      chains = 4,
      cores = 4,
      thin = 1,
      seed = 15,
      control = list(adapt_delta = 0.95),
      sample_prior = "yes",
      save_all_pars = TRUE
    )

  return(model)
}

model_vis_contrast <-
  run_brms(
    model = bf(cont | trials(total) ~ visual_contrast + (1|manufacturer))
  )

write_rds(model_vis_contrast, "Results/model-vis-contrast.rds")

model_pattern_width <-
  run_brms(
    model = bf(cont | trials(total) ~ pat_width + (1|manufacturer))
  )

summmodel_first_surf <-
  run_brms(
    model = bf(cont | trials(total) ~ first_surf + (1|manufacturer))
  )

model_full <-
  run_brms(
    model = bf(cont | trials(total) ~ visual_contrast + pat_width + first_surf + (1|manufacturer))
  )

write_rds(model, "Results/full-model.rds")

# TEST --------------------------------------------------------------------

get_variables(model)

source("https://sebastiansauer.github.io/Rcode/logit2prob.R")

prob2logit <-
  function(x){
    log(x/(1-x))
  }



