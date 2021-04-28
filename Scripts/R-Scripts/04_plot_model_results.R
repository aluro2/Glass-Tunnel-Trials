
# Load packages and model data --------------------------------------------
library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)
library(ggthemr)


model <-
  read_rds("Results/full-model.rds")


MatchedData <-
  read_rds("Data/MatchedData.rds") %>%
  filter(!total == 0)

# Plot model results ------------------------------------------------------

plot_data <-
  model$data %>%
  group_by(first_surf) %>%
  add_fitted_draws(model, n = 100, scale = "response") %>%
  mutate(avoid_prob = .value/total)

# plot_data <-
#   model$data %>%
#   data_grid(visual_contrast = seq_range(visual_contrast, n = 50),
#             total = seq(min(total),max(total), 1),
#             pat_width = seq_range(pat_width, n = 50),
#             first_surf) %>%
#   add_fitted_draws(model, n = 100, scale = "response") %>%
#   mutate(avoid_prob = .value/total)

ggthemr::ggthemr_reset()

ggthemr::ggthemr('solarized')

plot_contrast_and_patwidth <-
  ggplot() +
  geom_vline(xintercept = -2, lwd = 1.5, color = "gray", alpha = 0.5) +
  geom_label(
    data = filter(MatchedData, !is.na(first_surf), !is.na(pat_width)) ,
    aes(
      label = matchname.y,
      x = visual_contrast,
      y = score,
      color = first_surf,
      size = 3
    )
  ) +
  scale_size() +
  geom_line(
    data = plot_data,
    aes(
      x = visual_contrast,
      y = avoid_prob,
      group = paste(first_surf, .draw),
      color = first_surf
    ),
    stat = "smooth",
    method = "lm",
    se = FALSE,
    alpha = 0.1
  ) +
  geom_line(
    data = plot_data,
    aes(
      x = visual_contrast,
      y = avoid_prob,
      group = first_surf,
      color = first_surf
    ),
    stat = "smooth",
    method = "lm",
    se = FALSE,
    alpha = 1,
    size = 1.5
  ) +
  scale_colour_ggthemr_d() +
  guides(
    color = guide_legend(title = "Glass Pattern \n Surface Number")#,
    #size = guide_legend(title = "Glass Pattern Width (mm)")
  ) +
  labs(
    x = "Avian Visual Contrast (JND)",
    y = "Proportion of flights \n toward control glass"
  ) +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, 0.10)) +
  facet_wrap(. ~ first_surf) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

ggsave("Figures/01_contrast_and_patwidth_dropped_outliers.pdf",
       plot_contrast_and_patwidth,
       width = 16,
       height = 8,
       units = "in",
       dpi = 400)
