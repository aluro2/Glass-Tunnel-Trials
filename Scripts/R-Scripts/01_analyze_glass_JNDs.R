
# Load packages -----------------------------------------------------------
library(tidyverse)
library(pavo)

# Load data, get sample IDs -----------------------------------------------

GlassSpecs <-
    read_rds("Data/glass-reflectance-spectra.rds")

# Fix sample name errors
{
  names(GlassSpecs)<- gsub("walker_1_211", "walker_1211", names(GlassSpecs))
  names(GlassSpecs)<- gsub("walker_2_211", "walker_2211", names(GlassSpecs))
  names(GlassSpecs)<- gsub("eastman_number_0033_bh", "eastman_0033bh", names(GlassSpecs))
    }

SampleID <-
  names(GlassSpecs)[2:length(GlassSpecs)] %>%
    stringr::str_extract(., "[^_]*_[^_]*") %>%
    sort()

# Problem with eastman sample naming
#names(GlassSpecs)[str_detect(names(GlassSpecs), "eastman")]

GlassPatternJNDs <-
 GlassSpecs %>%
   vismodel(.,
            visual = "avg.uv",
            illum = "D65",
            achromatic = "bt.dc",
            relative = F) %>%
   pavo::coldist(.,
                 noise = "neural",
                 achromatic = TRUE,
                 subset = c("on", "off"))

MatchedJNDs <-
  GlassPatternJNDs %>%
    mutate(product1 = str_extract(patch1, "[^_]*_[^_]*"),
           product2 = str_extract(patch2, "[^_]*_[^_]*")) %>%
    filter(product1==product2) %>%
    select(!product2) %>%
    select(product1, everything()) %>%
    rename(matchname = product1) %>%
    arrange(patch1) %>%
  # Average any duplicates (only difference is white/mirror standard used for refl. measurement)
  group_by(matchname) %>%
  summarise(
    dS = mean(dS),
    dL = mean(dL)
  )

write_csv(MatchedJNDs, "Data/MatchedJNDs.csv")
