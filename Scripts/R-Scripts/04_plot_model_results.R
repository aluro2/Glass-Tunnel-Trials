
# Load packages and model data --------------------------------------------
library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)
library(ggthemr)


model <-
  read_rds("Results/full-model.rds")


MatchedData <-
  read_rds("Data/MatchedData.rds")

# Plot model results ------------------------------------------------------

plot_data <-
  model$data %>%
  group_by(first_surf, visual_contrast) %>%
  add_fitted_draws(model, n = 100, scale = "response") %>%
  mutate(avoid_prob = .value/total)

plot_data <-
  model$data %>%
  data_grid(visual_contrast = seq(0, 10, 0.2),
            total = 65,
            pat_width = seq_range(pat_width, n = 20),
            first_surf = c(1,2)) %>%
  group_by(first_surf, visual_contrast) %>%
  add_fitted_draws(model, n = 500, scale = "response") %>%
  mutate(avoid_prob = .value/total) %>%
  mutate(median_value = median(avoid_prob))

ggplot() +
  geom_smooth(data = plot_data,
            aes(x = visual_contrast,
                y = median_value,
                color = first_surf,
                group = first_surf))

ggthemr::ggthemr_reset()

ggthemr::ggthemr('solarized')

plot_contrast_and_patwidth <-
  ggplot() +
  geom_vline(xintercept = 0, lwd = 1.5, color = "gray", alpha = 0.5) +
  geom_point(
    data = model$data,
    aes(
      x = visual_contrast,
      y = (cont/total),
      color = as.factor(first_surf),
      size = pat_width,
      group = as.factor(first_surf)
    )
  ) +
  stat_smooth(data = plot_data,
              aes(y = avoid_prob,
                  x = visual_contrast,
                  group = paste(.draw, first_surf),
                  color = as.factor(first_surf)),
              method = "lm",
              geom = "line", size = 0.5,
              alpha = 0.01,
              se = FALSE) +
  stat_smooth(data = plot_data,
              aes(y = median_value,
                  x = visual_contrast,
                  color = as.factor(first_surf)),
              method = "lm",
              geom = "line", size = 0.5,
              alpha = 1,
              se = FALSE) +

  # stat_smooth(
  #   data = plot_data,
  #   aes(
  #     x = visual_contrast,
  #     y = avoid_prob,
  #     group = first_surf,
  #     color = first_surf
  #   ),
  #   geom = "line",
  #   se = FALSE,
  #   alpha = 1,
  #   size = 1.5
  # ) +
  scale_colour_ggthemr_d() +
  guides(
    color = guide_legend(title = "Glass Pattern \n Surface Number"),
    size = guide_legend(title = "Glass Pattern Width (mm)")
  ) +
  labs(
    x = "Avian Visual Contrast (JND)",
    y = "Proportion of flights \n toward control glass"
  ) +
  #scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, 0.10)) +
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
