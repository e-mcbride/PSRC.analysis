# Prep mode data and create model syntax

library(tidyverse)
library(here)
library(janitor)
library(MplusAutomation)

devtools::load_all(helpers = FALSE)

trdat <- PSRC.data::trdat #read_rds(here("analysis/data/derived_data/trdat.rds"))


# people who made no trips are missing from the `travel` data, so I add them in with the following
prdat <- PSRC.data::prdat #read_rds(here("analysis/data/derived_data/prdat.rds"))

# Function version of above ############################################
mode <- trdat %>%
  add_nontravelers(prdat, main_mode) %>% # my function to add non-travelers
  select(personid, sov, hov, transit, everything())

# Create model syntax ##################################################################

# if the mplus folder does not exist in /analysis/, then create it
dir.create(here("analysis/03_Mplus/"))

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


# THEN: MANUALLY write template file before running the following: ----------------
#  This section could become a function on its own
templatefile_path <- paste0(model_template, model_name, "_template.txt")

MplusAutomation::createModels(templatefile = templatefile_path)


# Copy .inp files to the model folder from the template folder,
# BUT ONLY IF THE INPUT FILES DO NOT ALREADY EXIST
# IF THEY DO EXIST, they will not be overwritten. This protects changes made to input files to
# optimize model results

inp_list <- list.files(model_template, pattern = ".inp", full.names = TRUE)

file.copy(from = inp_list, to = model_path,
          recursive = FALSE,
          overwrite = FALSE)
