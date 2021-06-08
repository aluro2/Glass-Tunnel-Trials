
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
  data_grid(visual_contrast = seq(0, 13, 0.2),
            total = 65,
            pat_width = seq_range(pat_width, n = 20),
            first_surf = c("1", ">1"),
            season_yr_cond) %>%
  group_by(first_surf, visual_contrast) %>%
  add_predicted_draws(model, n = 20, scale = "response", allow_new_levels = T) %>%
  mutate(avoid_prob = .prediction/total) %>%
  mutate(median_value = median(avoid_prob))

ggthemr::ggthemr_reset()

ggthemr::ggthemr('solarized')

plot_contrast_and_patwidth <-
  ggplot() +
  stat_lineribbon(data =  plot_data,
                  aes(x = visual_contrast - 3,
                      y = avoid_prob),
                  alpha  = 0.8) +
  scale_fill_brewer(palette = "Greys") +
  geom_vline(xintercept = 0, lwd = 1.5, color = "gray", alpha = 0.5) +
  #geom_hline(yintercept = 0.75, lwd = 1.5, color = "gray", alpha = 0.5) +
  # stat_smooth(
  #   data = plot_data,
  #   aes(
  #     x = visual_contrast,
  #     y = avoid_prob,
  #     group = .draw
  #   ),
  #   se = FALSE,
  #   method = "lm",
  #   alpha = 0.05,
  #   color = "gray",
  #   size = 0.5
  # ) +
  geom_point(
    data = model$data,
    aes(
      x = visual_contrast - 3,
      y = (cont/total),
      color = as.factor(first_surf),
      size = pat_width,
      group = as.factor(first_surf)
    )
  ) +
  # geom_smooth(
  #   data = plot_data,
  #   aes(
  #     x = visual_contrast,
  #     y = avoid_prob,
  #   ),
  #   se = FALSE,
  #   alpha = 1,
  #   color = "black",
  #   size = 1.5
  # ) +
  scale_colour_ggthemr_d() +
  guides(
    color = guide_legend(title = "Glass pattern \n surface number"),
    size = guide_legend(title = "Glass pattern width (cm)"),
    fill = guide_legend(title = "Model prediction probability interval")
  ) +
  labs(
    x = "Avian visual contrast (JND)",
    y = "Probabilty of flight avoidance"
  ) +
  scale_y_continuous(breaks = seq(0.1, 1, 0.10)) +
  expand_limits(y = 1) +
  scale_x_continuous(limits = c(-3, 10),
                     breaks = seq(-3, 10, 1)) +
  annotate("text",
           x = -1.5,
           y = 0.99,
           label = expression(paste(italic("BirdVis "), "PASS/FAIL threshold")),
           color = "gray",
           alpha = 1) +
  annotate("text",
           x = 5,
           y = 1,
           label = "The probability of flight avoidance increases by 7% \n for every 1 unit increase in visual contrast of the pattern against the glass sample.") +
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

# Plot pat-width ----------------------------------------------------------


ggplot() +
  geom_smooth(data = plot_data,
             aes(x = pat_width,
                 y = avoid_prob,
                 group = as.factor(first_surf)))



# Test --------------------------------------------------------------------

plot_data <-
  model_vis_contrast$data %>%
  data_grid(visual_contrast = seq(0, 13, 0.2),
            total = 65) %>%
  group_by(visual_contrast) %>%
  add_predicted_draws(model_vis_contrast, n = 20, scale = "response", allow_new_levels = T) %>%
  mutate(avoid_prob = .prediction/total) %>%
  mutate(median_value = median(avoid_prob))

ggthemr::ggthemr_reset()

ggthemr::ggthemr('solarized')

plot_contrast <-
  ggplot() +
  stat_lineribbon(data =  plot_data,
                  aes(x = visual_contrast,
                      y = avoid_prob),
                  alpha  = 0.8) +
  scale_fill_brewer(palette = "Greys") +
  geom_vline(xintercept = 0, lwd = 1.5, color = "gray", alpha = 0.5) +
geom_point(
  data = model_vis_contrast$data,
  aes(
    x = visual_contrast,
    y = (cont/total)
  )
) +
scale_colour_ggthemr_d() +
  guides(
    color = guide_legend(title = "Glass pattern \n surface number"),
    size = guide_legend(title = "Glass pattern width (cm)"),
    fill = guide_legend(title = "Model prediction probability interval")
  ) +
  labs(
    x = "Avian visual contrast (JND)",
    y = "Probabilty of flight avoidance"
  ) +
  scale_y_continuous(breaks = seq(0.1, 1, 0.10)) +
  expand_limits(y = 1) +
  scale_x_continuous(limits = c(0, 13),
                     breaks = seq(0, 13, 1)) +
  annotate("text",
           x = -1.5,
           y = 0.99,
           label = expression(paste(italic("BirdVis "), "PASS/FAIL threshold")),
           color = "gray",
           alpha = 1) +
  annotate("text",
           x = 5,
           y = 1,
           label = "The probability of flight avoidance increases by 7% \n for every 1 unit increase in visual contrast of the pattern against the glass sample.") +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

# ggsave("Figures/01_contrast_and_patwidth_dropped_outliers.pdf",
#        plot_contrast_and_patwidth,
#        width = 16,
#        height = 8,
#        units = "in",
#        dpi = 400)
