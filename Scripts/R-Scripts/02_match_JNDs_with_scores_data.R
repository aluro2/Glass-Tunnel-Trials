# Load packages -----------------------------------------------------------
library(tidyverse)
library(pavo)
library(tidyverse)
library(readxl)
library(janitor)
library(hablar)
library(fuzzyjoin)

# Hash-ID function
source("Scripts/R-Scripts/xx_hashID.R")

# Import JND data -----------------------------------------
MatchedJNDs <-
  read_csv("Data/MatchedJNDs.csv")

# Match JNDs with scores --------------------------------------------------
ScoreData <-
  readxl::read_xlsx("Data/Raw-Data/Tunnel-Trial-Data/Glass Measurements and Scores Complete.xlsx",
                    sheet  = 1,
                    na = "N/A") %>%
  # Keep only samples with spectral data
  #filter(spectraldata == "y") %>%
  janitor::clean_names(case = "snake") %>%
  mutate(samplename = recode(samplename, "3M" = "x3m")) %>%
  mutate(samplename = stringr::str_replace_all(samplename, " ", ""),
         samplename = stringr::str_replace_all(samplename, "[.]", ""),
         samplenumber = stringr::str_replace_all(samplenumber, "-", ""),
         samplenumber = stringr::str_replace_all(samplenumber, "_", ""),
         samplenumber = stringr::str_replace_all(samplenumber, " ", "")) %>%
  mutate(matchname = tolower(paste(samplename, samplenumber, sep = "_")),
         random = toupper(random),
         hor = toupper(hor),
         vert = toupper(vert),
         dots = recode(dots, "N?" = "N"),
         first_surf = fct_collapse(first_surf,
                                   "1" = "1",
                                   ">1" = c("2","3", "4", "5", "N")),
         no_glass = as_factor(no_glass),
         season_yr_cond = as_factor(season_yr_cond)

         ) %>%
  select(matchname, everything()) %>%
  select(!notes:length(.)) %>%
  hablar::convert(
    num(score,pat_width,p_width,p_height,thicktopat,glass_thick,total_thick,
        minhd, maxhd,avehd,minvd,maxvd,avevd
        ),
    int(cont, total, test),
    fct(season_yr, insulated, no_glass, uv, first_surf,
        sec_surf, random, hor, vert, dots)
  )

MatchedData <-
  ScoreData %>%
  fuzzyjoin::fuzzy_left_join(MatchedJNDs, by = "matchname", match_fun = str_detect) %>%
  select(matchname.x,
         matchname.y,
         samplename,
         samplenumber,
         score,
         cont,
         total,
         test,
         dS_refl,
         dL_refl,
         insulated,
         pat_width,
         first_surf,
         everything()) %>%
  # Get weighted values-- achro dL = 85%, chromatic dS = 15%
  mutate(
    dS15 = dS_refl*0.15,
    dL85 = dL_refl*0.85,
    # Composite visual contrast
    visual_contrast = ((dS15 + dL85)/ 2),
    first_surf = as.factor(first_surf)
  ) %>%
  # mutate(sampleID = hashed_id(matchname.x, "glass1234")#,
  #        #first_surf = fct_drop(first_surf, ">2")
  #        ) %>%
  select(#-matchname.x,
         #-matchname.y,
         -samplename,
         -samplenumber,
         -dS15,
         -dL85) %>%
  mutate(contrast_threshold = as_factor(
    case_when(visual_contrast <= 1 ~ "<1_JND",
              visual_contrast > 1 & visual_contrast < 3 ~ "1-3_JND",
              visual_contrast >= 3 ~ ">3_JND"))) %>%
  separate(matchname.x,
           into = c("manufacturer", "sampleID")) %>%
  filter(!total == 0) %>%
  # Remove outliers, likely bad reflectance measurement because of wrong white standard or small sample area (too small for spectrometer probe)
  filter(!(first_surf == 2 & visual_contrast > 5)#,
         #!(first_surf ==">2")
  ) %>%
  filter(!(manufacturer == "pilkington" & sampleID == "nsg4"),
         !(manufacturer == "guardian" & sampleID == "gdots"),
         !(manufacturer == "guardian" & sampleID == "guardian"),
         !(manufacturer == "guardian" & sampleID == "gdots40"),
         !sampleID == "guardian50",
         !(matchname.y == "arnold_56" & test == 30)) %>%
  # Use same JND vals for same sample (differences in size/shape/pattern, not color)
  mutate(dS_refl = ifelse(manufacturer == "stlouiszoo", 1.67807688, dS_refl),
         dL_refl = ifelse(manufacturer == "stlouiszoo", 20.06175, dL_refl),
         dS_refl = ifelse(manufacturer == "walker" & sampleID == "1211", 0.71186309, dS_refl),
         dL_refl = ifelse(manufacturer == "walker" & sampleID == "1211", 8.5281456, dL_refl),
         dS_refl = ifelse(matchname.y == "mcgrory_mgbp008", 0.95911613, dS_refl),
         dL_refl = ifelse(matchname.y == "mcgrory_mgbp008", 1.9710791, dL_refl))

# Save matched data -------------------------------------------------------

write_rds(MatchedData, "Data/MatchedData.rds")

print("DONE")
