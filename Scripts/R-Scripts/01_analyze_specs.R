# Load packages -----------------------------------------------------------
library(tidyverse)
library(pavo)


# Import reflectance data -------------------------------------------------

GlassSpecs <-
  readr::read_rds("Data/glass-reflectance-spectra.rds")

plot(GlassSpecs, col = spec2rgb(GlassSpecs), lwd = 3)

SampleNames <-
  ~do.call(rbind,
           # Split at (and keep all text) before last underscore
           strsplit(names(.x)[2:length(names(.x))], "_(?!.*_)", perl=TRUE))[,1])

names(GlassSpecs)[2:length(GlassSpecs)] %>%
  stringr::str_split(., "_") %>%
  do.call(rbind, .)
