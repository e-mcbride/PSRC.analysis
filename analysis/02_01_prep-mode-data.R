# Prep mode data and create model syntax

library(tidyverse)
library(here)
library(janitor)
library(MplusAutomation)

devtools::load_all(helpers = FALSE)

library(PSRC.data)
trdat <- PSRC.data::trdat #read_rds(here("analysis/data/derived_data/trdat.rds"))


# people who made no trips are missing from the `travel` data, so I add them in with the following
prdat <- PSRC.data::prdat #read_rds(here("analysis/data/derived_data/prdat.rds"))

trpid <- trdat %>%
  pull(personid) %>%
  unique()

notrav <- prdat %>%
  filter(!(personid %in% trpid)) %>%
  select(personid) %>%
  mutate(hov = 0,
         sov = 0,
         walk = 0,
         bike = 0,
         transit = 0,
         other = 0)


mode <- trdat %>%
  select(personid, main_mode) %>%
  filter(!is.na(main_mode)) %>%
  mutate(vals = 1) %>%
  distinct() %>%
  # group_by(personid) %>%
  pivot_wider(id_cols = personid, names_from = main_mode, values_from = vals, values_fill = 0) %>%
  clean_names() %>%

  # Adding people who did not travel:
    bind_rows(notrav) %>%
  select(personid, sov, hov, transit, everything())


# Create model syntax ##################################################################

#import my function to write mplus data to file in the right file location
devtools::load_all()

# if the mplus folder does not exist in /analysis/, then create it
dir.create(here("analysis/03_Mplus/"))

create_model_dirs <- function(model_name) {
  analysis_relPath <- paste0("analysis/03_Mplus/", model_name, "/")
  template_relPath <- paste0(analysis_relPath, "template/")

  analysis_path <- here(analysis_relPath)
  template_path <- here(template_relPath)
  if(dir.exists(analysis_path)) {
    message("Model root folder already exists")
  }
  dir.create(analysis_path)

  if(dir.exists(template_path)) {
    message("Template folder already exists within model root folder")
  }
  dir.create(template_path)
}


model_name <- "mode"


create_model_dirs(model_name)

model_path <- paste0("analysis/03_Mplus/", model_name, "/")
model_template <-  paste0(model_path, "template/")

# my function to write mplus data to file in the right file location:
write_mplus_data(df = mode,
                 wd_for_analysis = here(model_path),
                 filename = paste0(model_name, "-data-mplus-ready.dat"),
                 writeData = "ifmissing",
                 hashfilename = TRUE)


# create blank template file

templatefile_path <- paste0(model_template, model_name, "_template.txt")

if(file.exists(templatefile_path)) {
  message("File already exists")
} else {
  file.create(templatefile_path)
}


# THEN: manually write template file before running the following: ----------------
#  This section could become a function on its own
MplusAutomation::createModels(templatefile = templatefile_path)


# Copy .inp files to the model folder from the template folder,
# BUT ONLY IF THE INPUT FILES DO NOT ALREADY EXIST
# IF THEY DO EXIST, they will not be overwritten. This protects changes made to input files to
# optimize model results

inp_list <- list.files(model_template, pattern = ".inp", full.names = TRUE)

file.copy(from = inp_list, to = model_path,
          recursive = FALSE,
          overwrite = FALSE)
