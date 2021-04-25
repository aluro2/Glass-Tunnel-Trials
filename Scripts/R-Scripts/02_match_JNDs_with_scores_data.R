# Load packages -----------------------------------------------------------
library(tidyverse)
library(pavo)
library(tidyverse)
library(readxl)
library(janitor)
library(hablar)
library(fuzzyjoin)


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
                                   "2" = "2",
                                   ">2" = c("3", "4", "5", "N")
                                   )) %>%
  select(matchname, everything()) %>%
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
         dS,
         dL,
         insulated,
         pat_width,
         first_surf,
         everything()) %>%
  # Get weighted values-- achro dL = 85%, chromatic dS = 15%
  mutate(
    dS15 = dS*0.15,
    dL85 = dL*0.85,
    visual_contrast = ((dS15 + dL85)/ 2) - 3
  )


# Save matched data -------------------------------------------------------

write_rds(MatchedData, "Data/MatchedData.rds")
