# Load packages -----------------------------------------------------------
library(tidyverse)
library(pavo)
library(readxl)
library(tidyselect)


# Import reflectance spectra files ----------------------------------------

# Paths to spec files
SpecPaths <-
  list.files("Data/Tunnel-Trial-Data/Reflectance/",
             pattern = ".xls",
             full.names = TRUE)

# Import excel files
Specs<-
  SpecPaths %>%
  purrr::map(., ~readxl::read_xlsx(.x, sheet = 1)) %>%
  # Clean names to SCREAMING_SNAKE
  purrr::map(., ~janitor::clean_names(.x,case = "snake")) %>%
  # Get rid of generic unnamed specs
  purrr::map(., ~select(.x,!contains("spectrum"))) %>%
  # Get rid of erroneous rows
  purrr::map(., ~mutate(.x, ~filter(across(where(is.numeric(.x))))))
  # Convert to rspec, trim to 300-700nm
  purrr::map(., ~as.rspec(.x, whichwl = 1, lim = c(300,700)))
