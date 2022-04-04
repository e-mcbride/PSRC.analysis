# 03_01 Prep mode data and create model syntax
# this model
## - will be with the "bad responses" that I found during the sequence analysis removed
## - data will contain auxiliary variables/covariates for more complex models.

library(tidyverse)
library(here)
library(janitor)


devtools::load_all(helpers = FALSE) # load functions

trraw <- PSRCData::trdat

# `model_name` will be used to write folder names, import/export, etc.
## change `model_name` if you are running a new model with different parameters
### This will create a new folder with all the syntax
model_name <- "mode_cleaned_aux"

# import post-cleaning list of person IDs to use when filtering out bad records
cleanpids <- read_rds(here("analysis/data/derived_data/clean_pids.rds"))

# remove bad records before analysis
trdat <- trraw %>% filter(personid %in% cleanpids)

# Separate trips by mode for adding to the auxiliary variables:
mode <- trdat %>%
  # Below is my function to make dummy variables from a
  # variable in the data set. Enter ?make_dummies for more info
  make_dummies(mode_full_EM) # `mode_full_EM` variable separates trips by mode

rm(list=setdiff(ls(), c("mode", "model_name", "cleanpids")))


# add auxiliary variables/covariates ###########################################

# compiling auxiliary var/covariate data for use in more complex models:
auxvars <- read_rds(here("analysis/data/derived_data/curated-auxiliary-vars.rds")) %>%

  # remove ppl age under 18:
  filter(agegrp >= "age18_34") %>%

  # un-order ordered factors because `MplusAutomation` package doesn't like it
  mutate(across(where(is.ordered), ~ factor(.x, ordered = FALSE))) %>%

  # remove bad records:
  filter(personid %in% cleanpids)

# Add mode dummy variables to covariate data set
mode_cov <- auxvars %>%
  left_join(mode, by = "personid") %>%
  #
  select(names(mode), everything()) %>%
  # `personid` in numeric format for Mplus to accept it as an ID variable:
  mutate(personid = as.numeric(personid)) %>%
  janitor::clean_names()

# create numeric codes for factor levels in covariates:
mode_cov_num <- mode_cov %>%
  mutate(across(where(is.factor), as.numeric))

# extract names of factor vars, used to make dummy vars in `write_mplus_data()`:
names_fac_vars <- mode_cov %>%
  dplyr::select((where(is.factor))) %>%
  names()

# Create model syntax ##########################################################

# if the Mplus folder does not exist in /analysis/, then create it:
dir.create(here("analysis/Mplus/"))

# create directory structure for running Mplus models using model_name as folder name:
##    Also creates a blank template file for use w/ `MplusAutomation::createModels()`
## Please run ?create_model_dirs for more info
create_model_dirs(model_name)


# write paths to strings for use in writing files:
model_path <- paste0("analysis/Mplus/", model_name, "/")
model_template <-  paste0(model_path, "template/")


# my function to write mplus data to file in the right file location:
  ## (builds on MplusAutomation::prepareMplusData(). Run `?write_mplus_data` for more info)
write_mplus_data(df = mode_cov_num,
                 wd_for_analysis = here(model_path),
                 filename = paste0(model_name, "-data-mplus-ready.dat"),
                 writeData = "ifmissing",
                 hashfilename = TRUE,
                 dummyCode = names_fac_vars)


# THEN: MANUALLY write template file -------------------------------------------

## Template file is used to create a set of input files used by Mplus to run models
##    Please see vignette("vignette", package="MplusAutomation") for info about
##    building the template file for MplusAutomation to use


# THEN run the following: ----------------

# write path of template file as string to input into `MplusAutomation::createModels()`
templatefile_path <- paste0(model_template, model_name, "_template.txt")

MplusAutomation::createModels(templatefile = templatefile_path)


# Next pieced of code copies .inp files to the model folder from the template folder,
# BUT ONLY IF THE INPUT FILES DO NOT ALREADY EXIST
# IF THEY DO EXIST, they will not be overwritten
# This protects manual changes made to input files to optimize model results

inp_list <- list.files(model_template, pattern = ".inp", full.names = TRUE)

file.copy(from = inp_list, to = model_path,
          recursive = FALSE,
          overwrite = FALSE)
