# Load packages and model data --------------------------------------------
library(tidyverse)
library(brms)
library(tidybayes)
library(modelr)
library(ggthemr)
library(gtsummary)

MatchedData <-
  read_rds("Data/MatchedData.rds") %>%
  filter(!total == 0)


# Plot pattern surface vs. visual contrast --------------------------------

ggthemr::ggthemr_reset()

ggthemr::ggthemr('solarized')

plot_contrast_and_patsurface <-
  MatchedData %>%
  filter(., !is.na(first_surf), !is.na(pat_width)) %>%
  ggplot(aes(x = first_surf, y = visual_contrast)) +
  geom_hline(yintercept = 0, size  = 1.5, color = "gray", alpha = 0.5) +
  geom_jitter(aes(color = first_surf)) +
  geom_boxplot(aes(fill = first_surf, color = first_surf),
               alpha = 0.5) +
  scale_colour_ggthemr_d() +
  labs(
    y = "Avian Visual Contrast (JND)",
    x = "Glass Pattern \n Surface Number"
  ) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )



ggsave("Figures/02_surface_number_vs_contrast.pdf",
       plot_contrast_and_patsurface,
       width = 10,
       height = 7,
       units = "in",
       dpi = 400)

# Plot pattern surface vs. score --------------------------------

ggthemr::ggthemr_reset()

ggthemr::ggthemr('solarized')

plot_contrast_and_score <-
  MatchedData %>%
  filter(., !is.na(first_surf), !is.na(pat_width)) %>%
  ggplot(aes(x = first_surf, y = score)) +
  geom_hline(yintercept = 0.5, size  = 1.5, color = "gray", alpha = 0.5) +
  geom_jitter(aes(color = first_surf)) +
  geom_boxplot(aes(fill = first_surf, color = first_surf),
               alpha = 0.5) +
  scale_colour_ggthemr_d() +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, 0.25)) +
  labs(
    y = "Proportion of flights \n toward control glass",
    x = "Glass Pattern \n Surface Number"
  ) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave("Figures/03_surface_number_vs_score.pdf",
       plot_contrast_and_score,
       width = 10,
       height = 7,
       units = "in",
       dpi = 400)


# Summary Table -----------------------------------------------------------
  MatchedData %>%
    filter(., !is.na(first_surf), !is.na(pat_width)) %>%
    select(score, visual_contrast, first_surf) %>%
    drop_na() %>%
    mutate(first_surf = recode(first_surf,
                                "1" = "Surface 1",
                               "2" = "Surface 2",
                               ">2" = "> Surface 2")) %>%
    rename("Proportion of flights toward control glass" = score,
           "Avian visual contrast" = visual_contrast,
           "Glass Pattern Surface Number" = first_surf) %>%
  gtsummary::tbl_summary(
    by = "Glass Pattern Surface Number",
    missing = "no"
  ) %>%
  gtsummary::add_p() %>%
  gtsummary::bold_labels()
