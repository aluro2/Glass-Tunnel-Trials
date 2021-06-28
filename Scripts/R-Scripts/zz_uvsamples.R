

# Load packages and import data -------------------------------------------

library(tidyverse)
library(pavo)
library(fuzzyjoin)

uvsamples <-
  subset(read_rds("Data/glass-reflectance-spectra.rds"),
         c("arnold", "mcgrory", "guardian", "glasspro", "walker")) %>%
    subset(., "on") %>%
  left_join(.,
            subset(read_rds("Data/glass-transmission-spectra.rds"),
                   c("arnold", "mcgrory", "guardian", "glasspro", "walker")) %>%
                subset(., "on"),
            by = "wl")

MatchedData <-
  read_rds("Data/MatchedData.rds")

#  Plot with gglpot2 ------------------------------------------------------

uvsamplesTunnel <-
  unique(plot_data$name) %>%
    tibble(matchname.y = .) %>%
    fuzzyjoin::fuzzy_inner_join(MatchedData, by = "matchname.y", match_fun = str_detect) %>%
    distinct(., matchname.y.y, .keep_all = T)

uvsamplesTunnel %>%
  group_by(manufacturer) %>%
  summarise(mean_contrast = mean(visual_contrast),
            mean_score = mean(score))

plot_data <-
  uvsamples %>%
    pivot_longer(cols = -wl) %>%
    mutate(manufacturer = str_extract(name, "[^_]+"),
           type = ifelse(str_detect(name, "trans"), "transmission", "reflectance"))

plot_data %>%
  ggplot(aes(x = wl, y = value, group = name, linetype = type)) +
  geom_path(alpha = 0.3) +
  guides(color = FALSE) +
  facet_grid(type ~ manufacturer) +
  labs(y = "Percent Reflectance/Transmission",
       x = "Wavelength (nm)",
       linetype = "Spectral Measurement",
       title = "Glass Measurements by Manufacturer") +
  theme_bw() +
  theme(text = element_text(family = "Noto Sans"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14, face = "bold"),
        panel.grid.minor = element_blank())

ggsave("~/Downloads/uvsamples.pdf",
       height = 8,
       width = 12,
       units = "in",
       dpi = 400,
       device=cairo_pdf)


# Plot all samples individually -------------------------------------------

names(uvsamples)[2:length(names(uvsamples))] %>%
  str_extract(., "[^_]+")

plot_data %>%
  ggplot(aes(x = wl, y = value, group = name, linetype = type)) +
  geom_path(alpha = 0.3) +
  guides(color = FALSE) +
  facet_wrap(type ~ name)
