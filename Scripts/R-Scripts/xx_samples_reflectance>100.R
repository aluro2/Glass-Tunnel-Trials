# Load packages -----------------------------------------------------------
library(tidyverse)
library(pavo)
library(readxl)
library(tidyselect)
library(future)
library(furrr)

# Import reflectance spectra files ----------------------------------------

# Paths to spec files
SpecPaths <-
  list.files("Data/Raw-Data/Tunnel-Trial-Data/Reflectance",
             pattern = ".xls",
             full.names = TRUE) %>%
  # Drop file with unnamed sample (all spectra named "SPECTRUM...")
  .[-2]

# Import excel files

# Run functions on list items in parallel using multiple CPU cores
future::plan("multicore")

Specs <-
  SpecPaths %>%
  # Import spreadsheets
  furrr::future_map(., ~readxl::read_xlsx(.x, sheet = 1)) %>%
  # Clean names to SCREAMING_SNAKE
  furrr::future_map(., ~janitor::clean_names(.x,case = "snake")) %>%
  # Get rid of generic unnamed specs, light and dark measurements
  furrr::future_map(., ~select(.x,!contains(c("spectrum", "light", "dark")))) %>%
  # Replace non-numeric values with NA
  furrr::future_map(., ~mutate(.x, across(everything(), as.numeric))) %>%
  # Get rid of erroneous rows
  furrr::future_map(., ~drop_na(.x)) %>%
  # Keep consistent wl column name
  furrr::future_map(., ~rename(.x, "wl" = 1)) %>%
  # Convert to rspec, trim to 300-700nm
  furrr::future_map(., ~pavo::as.rspec(.x, whichwl = 1, lim = c(300,700))) %>%
  # Smooth spectra
  furrr::future_map(., ~pavo::procspec(.x, opt = "smooth", span = 0.25)) %>%
  # Make all negative refl values = 0
  furrr::future_map(., ~pavo::procspec(.x, fixneg = "zero"))

# Get names of samples with reflectance values > 100% ---------------------
specs_100 <-
  Specs %>%
    purrr::map(., ~select_if(.x, ~max(.)> 100)) %>%
    purrr::map(., ~names(.x)) %>%
    unlist()  %>%
    unique() %>%
    tibble(matchname = .) %>%
    filter(!matchname == "wl")


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

MatchedData100 <-
  specs_100 %>%
  fuzzyjoin::fuzzy_left_join(ScoreData, by = "matchname", match_fun = str_detect) %>%
  select(matchname.x,
         matchname.y,
         samplename,
         samplenumber,
         score,
         cont,
         total,
         test,
         insulated,
         pat_width,
         first_surf,
         everything()) %>%
  filter(!matchname.y == is.na(matchname.y)) %>%
  rename(`ReflectanceSample>100` = matchname.x,
         `ScoreDataSampleName` = matchname.y)

write_csv(MatchedData100, file = "Data/reflectance-error-samples.csv")
