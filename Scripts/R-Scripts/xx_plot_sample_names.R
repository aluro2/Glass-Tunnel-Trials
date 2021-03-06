# Load packages and data --------------------------------------------------
library(tidyverse)

MatchedData <-
  read_rds("Data/MatchedData.rds")

ggplot(MatchedData,
       aes(x = visual_contrast,
           y = score,
           label = paste(matchname.y, pat_width, sep = "_"))) +
  geom_text(hjust = 0, nudge_x = 0.05) +
  geom_point(alpha = 0.9)

