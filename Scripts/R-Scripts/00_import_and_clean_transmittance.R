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
  list.files("Data/Raw-Data/Tunnel-Trial-Data/Transmittance",
             pattern = ".xls",
             full.names = TRUE) %>%
  # Drop file with unnamed sample (all spectra named "SPECTRUM...")
  .[-grep("3-12-19|3-19-19|3-21-19|3-22-19", .)]

# Import excel files

# Run functions on list items in parallel using multiple CPU cores
future::plan("multicore")

Specs <-
  SpecPaths %>%
  # Import spreadsheets
  furrr::future_map(., ~readxl::read_xlsx(.x, sheet = 1) %>% select(-contains("SPECTRUM"))) %>%
  # Remove empty columns
  furrr::future_map(., ~janitor::remove_empty(.x, "cols")) %>%
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
  furrr::future_map(., ~pavo::procspec(.x, fixneg = "zero")) %>%
  # Remove samples if reflectance > 100%
  furrr::future_map(., ~mutate(.x, across(everything(),~ifelse(.x > 100, NA, .x)))) %>%
  furrr::future_map(., ~select_if(.x, ~!any(is.na(.x)))) %>%
  map(., ~tibble(wl = 300:700, .x))

# Get sample names
SampleNames <-
  Specs %>%
  purrr::map(.,
             ~do.call(rbind,
                      # Split at (and keep all text) before last underscore
                      strsplit(names(.x)[2:length(names(.x))], "_(?!.*_)", perl=TRUE))[,1])

# Get average reflectance from sample replicates
{AvgSpecs <- list()
  for( i in 1:length(Specs)){
    samples <- SampleNames[[i]]

    avg_specs <-
      aggspec(Specs[[i]],
              by = samples)

    AvgSpecs[[i]] <- avg_specs
  }
  }

# Combine all reflectance spectra into single df

AllSpecs <-
  AvgSpecs %>%
  map(., ~rename_with(.x, ~paste(.x, "_trans", sep = ""), -contains("wl"))) %>%
  reduce(left_join, "wl")

# Additional specs (collected 6/2021)
AddtlSpecs <-
  getspec("Data/Raw-Data/Tunnel-Trial-Data/Transmittance/additional-spectra-06-2021/",
          ext = "jaz",
          subdir = TRUE) %>%
  procspec(fixneg = "zero",
           opt = "smooth") %>%
  mutate(., across(everything(), ~ifelse(.x > 100, NA, .x))) %>%
  select_if(., ~!any(is.na(.x))) %>%
  mutate(wl = 300:700) %>%
  select(wl, everything())

# Sample names
AddtlSpecsNames <-
  colnames(AddtlSpecs) %>%
  .[2:length(.)] %>%
  sub("_[^_]+$", "", .)

# Sample averages
AddtlSpecsAvg <-
  AddtlSpecs %>%
  aggspec(.,
          by = AddtlSpecsNames)

# Combine with AllSpecs

FullSpecs <-
  AllSpecs %>%
  select(-contains("eastman_3333bh"),
         -contains("3333")) %>%
  left_join(., AddtlSpecsAvg,
            by = "wl") %>%
  select(-contains("_na_"))

# Save specs as RDS -------------------------------------------------------

write_rds(FullSpecs,
          file = "Data/glass-transmission-spectra.rds")
