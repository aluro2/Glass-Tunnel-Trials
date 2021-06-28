
# Load packages -----------------------------------------------------------
library(tidyverse)
library(pavo)
library(future)
library(furrr)

# Load data, get sample IDs -----------------------------------------------

ReflSpecs <-
    read_rds("Data/glass-reflectance-spectra.rds")

TransSpecs <-
  read_rds("Data/glass-transmission-spectra.rds")

GlassSpecs <-
  list(refl = read_rds("Data/glass-reflectance-spectra.rds"),
       trans = read_rds("Data/glass-transmission-spectra.rds"))

# Fix sample name errors
{
  names(GlassSpecs$refl)<- gsub("walker_1_211", "walker_1211", names(GlassSpecs$refl))
  names(GlassSpecs$refl)<- gsub("walker_2_211", "walker_2211", names(GlassSpecs$refl))
  names(GlassSpecs$refl)<- gsub("eastman_number_0033_bh", "eastman_0033bh", names(GlassSpecs$refl))
  names(GlassSpecs$refl)<- gsub("eastman333bh", "eastman_0333bh", names(GlassSpecs$refl))
    }

SampleID <-
  GlassSpecs %>%
  map(~names(.x)[2:length(names(.x))]) %>%
  map(~stringr::str_extract(.x, "[^_]*_[^_]*")) %>%
  map(~sort(.x))

# Problem with eastman sample naming
#names(GlassSpecs)[str_detect(names(GlassSpecs), "eastman")]

plan(multisession, workers = 2, .cleanup = TRUE)

GlassPatternJNDs <-
 GlassSpecs %>%
  furrr::future_map(
   ~vismodel(.x,
            visual = "avg.uv",
            illum = "D65",
            achromatic = "bt.dc",
            relative = F) %>%
   pavo::coldist(.,
                 noise = "neural",
                 achromatic = TRUE,
                 subset = c("on", "off")),
   .progress = T
  )

MatchedJNDs <-
  GlassPatternJNDs %>%
    purrr::map(
    ~mutate(.x,
           product1 = str_extract(patch1, "[^_]*_[^_]*"),
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
)

ReducedJNDs <-
  MatchedJNDs %>%
    reduce(left_join, "matchname") %>%
    rename(dS_refl = dS.x,
           dL_refl = dL.x,
           dS_trans = dS.y,
           dL_trans = dL.y)

write_csv(ReducedJNDs, "Data/MatchedJNDs.csv")
