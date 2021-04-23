
# Load packages -----------------------------------------------------------
library(tidyverse)
library(pavo)

# Load data, get sample IDs -----------------------------------------------

GlassSpecs <-
  read_rds("Data/glass-reflectance-spectra.rds")

SampleID <-
  names(GlassSpecs)[2:length(GlassSpecs)] %>%
    stringr::str_extract(., "^([^_]*_){2}")

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
    rename(matchname = product1)


# Match JNDs with scores --------------------------------------------------
ScoreData <-
  readxl::read_xlsx("Data/Raw-Data/Tunnel-Trial-Data/Glass Measurements and Scores Complete.xlsx",
                    sheet  = 1) %>%
  # Keep only samples with spectral data
  filter(spectraldata == "y") %>%
  janitor::clean_names(case = "snake") %>%
  mutate(matchname = tolower(paste(samplename, samplenumber, sep = "_"))) %>%
  select(matchname, everything())

MatchedData <-
  ScoreData %>%
  fuzzyjoin::fuzzy_inner_join(MatchedJNDs, by = "matchname", match_fun = str_detect) %>%
  select(matchname.x,
         matchname.y,
         samplename,
         samplenumber,
         score,
         dS,
         dL,
         insulated,
         pat_width,
         first_surf) %>%
  mutate(score = as.numeric(score),
         pat_width = as.numeric(pat_width),
         first_surf = as_factor(first_surf)) %>%
  # Get weighted values-- achro dL = 85%, chromatic dS = 15%
  mutate(
    dS15 = dS*0.15,
    dL85 = dL*0.85,
    visual_contrast = ((dS15 + dL85)/ 2) - 3
  )

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

