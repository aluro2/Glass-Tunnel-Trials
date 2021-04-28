
# Load packages and data --------------------------------------------------
library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)
library(ggthemr)

MatchedData <-
  read_rds("Data/MatchedData.rds") %>%
  filter(!total == 0) %>%
  # Remove outliers, likely bad reflectance measurement
  filter(!(first_surf == 2 & visual_contrast > 5))

# Model flight avoidance vs. pattern JNDs ---------------------------------
model <-
  brm(
    cont | trials(total) ~ (visual_contrast*pat_width)*first_surf,
    data = MatchedData,
    family = binomial(link = "logit"),
    iter = 2000,
    inits = "random",
    chains = 4,
    cores = 4,
    thin = 1,
    seed = 15,
    sample_prior = "yes", save_all_pars = TRUE
  )

write_rds(model, "Results/full-model.rds")


# TEST --------------------------------------------------------------------

get_variables(model)

source("https://sebastiansauer.github.io/Rcode/logit2prob.R")

plot_data <-
  model$data %>%
    filter(first_surf %in% c(1,2),
           !total == 0) %>%
    group_by(first_surf) %>%
    data_grid(visual_contrast = seq_range(visual_contrast, n = 101),
              total = seq(min(total),max(total), 1)) %>%
    add_fitted_draws(model, n = 20, scale = "linear") %>%
    mutate(.value = logit2prob(.value))

plot_data %>%
  ggplot(aes(x = visual_contrast, y = .value, color = first_surf)) +
    geom_line(aes(y = .value, group = paste(first_surf, .draw)))


model %>%
  gather_draws(`b_visual_contrast:first_surf1`, `b_visual_contrast:first_surf2`) %>%
  median_hdi()


# Plot scores vs JNDs -----------------------------------------------------


MatchedData %>%
  ggplot(aes(x = visual_contrast, y = score, size = pat_width)) +
  geom_point() +
  geom_smooth(method = "lm", size = 1) +
  facet_wrap(first_surf ~.)

model <-
  MatchedData %>%
  filter(first_surf == 1) %>%
  lm(score ~ visual_contrast, data = .)

summary(model)


