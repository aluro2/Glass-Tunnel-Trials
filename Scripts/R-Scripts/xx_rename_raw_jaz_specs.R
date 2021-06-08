##Import renaming index, clean up dates, keep only columns to be used

library(tidyverse)


names.dat<-
  read_csv("Data/JAZ_file_rename_index.csv")

temp_dir <- tempdir()

head(names.dat)

rename.index <-
  names.dat %>%
  na.omit(.) %>%
  unite(., Name, sep="_", c(Manufacturer, Sample_Name, Pattern, Standard, Replicate)) %>%
  select(., CardID, Folder, Spec, Name) %>%
  mutate(old_names = paste(temp_dir, CardID,"jaz", Folder, paste(Spec, ".jaz", sep = ""), sep = "/"),
         new_names = paste(temp_dir, CardID,"jaz", Folder, Name, sep = "/"))

##Field data, American robins

file.copy(from = "Data/temp_specs/",
          to = temp_dir,
          recursive = T)

##Function to rename spectra files

file.rename(from = rename.index$old_names,
            to = rename.index$new_names)

#rename.rawspecs<-function(rename.index){
#  old_names<-paste(rename.index$Folder, rename.index$Spec, sep="/")
#  new_names<-paste(rename.index$Folder, rename.index$Name, sep="/")
#  file.rename(from = old_names, to = new_names)
#}

#rename.rawspecs(rename.index = rename.index.2017)


