# Prep mode data and create model syntax
# this model will be with the "bad responses" removed that I found during the sequence analysis

library(tidyverse)
library(here)
library(janitor)
# library(MplusAutomation)

devtools::load_all(helpers = FALSE)

trraw <- PSRCData::trdat #read_rds(here("analysis/data/derived_data/trdat.rds"))
prraw <- PSRCData::prdat

model_name <- "mode_cleaned"

# import clean pids

cleanpids <- read_rds(here("analysis/data/derived_data/clean_pids.rds"))

# filter
prdat <- prraw %>% filter(personid %in% cleanpids)
trdat <- trraw %>% filter(personid %in% cleanpids)

mode <- trdat %>%
  # my function to make dummy variables from one variable in the dataset (`main_mode`)
  make_dummies(main_mode)

rm(list=setdiff(ls(), "mode", "model_name"))

# Create model syntax ##################################################################

# if the mplus folder does not exist in /analysis/, then create it
dir.create(here("analysis/Mplus/"))

create_model_dirs(model_name)

model_path <- paste0("analysis/Mplus/", model_name, "/")
model_template <-  paste0(model_path, "template/")

# my function to write mplus data to file in the right file location:
write_mplus_data(df = mode,
                 wd_for_analysis = here(model_path),
                 filename = paste0(model_name, "-data-mplus-ready.dat"),
                 writeData = "ifmissing",
                 hashfilename = TRUE)


# THEN: MANUALLY write template file ---------------------------

# THEN RUN the following: ----------------

#  This section could become a function on its own
templatefile_path <- paste0(model_template, model_name, "_template.txt")

MplusAutomation::createModels(templatefile = templatefile_path)


# Next pieced of code copies .inp files to the model folder from the template folder,
# BUT ONLY IF THE INPUT FILES DO NOT ALREADY EXIST
# IF THEY DO EXIST, they will not be overwritten. This protects changes made to input files to
# optimize model results

inp_list <- list.files(model_template, pattern = ".inp", full.names = TRUE)

file.copy(from = inp_list, to = model_path,
          recursive = FALSE,
          overwrite = FALSE)
