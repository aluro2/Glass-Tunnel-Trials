# Load packages -----------------------------------------------------------
library(tidyverse)
library(pavo)
library(tidyverse)
library(readxl)
library(janitor)
library(fuzzyjoin)


# Import glass spectra, get names -----------------------------------------

GlassSpecs <-
  read_rds("Data/glass-reflectance-spectra.rds")

SampleNames <-
  names(GlassSpecs)[2:length(GlassSpecs)] %>%
  as_tibble()

# Import scored sample names ----------------------------------------------
ScoreSampleNames <-
  readxl::read_xlsx("Data/Raw-Data/Tunnel-Trial-Data/Glass Measurements and Scores Complete.xlsx",
                    sheet  = 1) %>%
  # Keep only samples with spectral data
  filter(spectraldata == "y") %>%
  janitor::clean_names(case = "snake") %>%
  select(samplename, samplenumber) %>%
  mutate(across(everything(), ~tolower(.x))) %>%
  unite(c(samplename,samplenumber), sep = "_", col = "samplename") %>%
  as_tibble()

# Get closest name matches ------------------------------------------------
FuzzyNames <-
  SampleNames %>%
    fuzzyjoin::fuzzy_left_join(ScoreSampleNames, by = c(1,1), match_fun = str_detect)

write_csv(FuzzyNames, "Data/samplenames-fuzzy-match.csv")