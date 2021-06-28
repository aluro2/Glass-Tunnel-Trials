##Import renaming index, clean up dates, keep only columns to be used

library(tidyverse)


names.dat<-
  read_csv("Data/JAZ_file_rename_index.csv")

temp_dir <- tempdir()

rename.index <-
  names.dat %>%
  na.omit(.) %>%
  unite(., Name, sep="_", c(Manufacturer, Sample_Name, Pattern, Standard, Replicate)) %>%
  select(., CardID, Folder, Spec, Name) %>%
  mutate(old_names = paste(temp_dir, CardID,"jaz", Folder, paste(Spec, ".jaz", sep = ""), sep = "/"),
         new_names = paste(temp_dir, CardID,"jaz", Folder, paste(Name, ".jaz", sep = ""), sep = "/"))

##Field data, American robins

file.copy(from = "Data/temp_specs/",
          to = temp_dir,
          recursive = T)

##Function to rename spectra files
file.rename(from = rename.index$old_names,
            to = rename.index$new_names)

# Get files subset by refl/trans
reflectanceData <-
  paste(temp_dir, "temp_specs", sep = "/") %>%
    list.files(full.names = T, recursive = T) %>%
    .[-grep("trans", .)]

transmissionData <-
  paste(temp_dir, "temp_specs", sep = "/") %>%
  list.files(full.names = T, recursive = T) %>%
  .[grep("trans", .)]

# Save files to respective dirs

file.copy(from = reflectanceData,
          to = "Data/Raw-Data/Tunnel-Trial-Data/Reflectance/additional-spectra-06-2021/")

file.copy(from = transmissionData,
          to = "Data/Raw-Data/Tunnel-Trial-Data/Transmittance/additional-spectra-06-2021/")