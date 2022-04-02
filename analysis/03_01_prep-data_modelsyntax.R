# XX_01 Prep mode data and create model syntax
# this model
## - will be with the "bad responses" removed that I found during the sequence analysis
## - data will contain auxiliary variables. I will be checking to see if this makes any difference at all in the model.
## - Will SEEDS be different?
## I want to divide up the mode "carpool" into "driveothers" and "passenger"

library(tidyverse)
library(here)
library(janitor)
# library(MplusAutomation)

devtools::load_all(helpers = FALSE)

trraw <- PSRCData::trdat #read_rds(here("analysis/data/derived_data/trdat.rds"))
# prraw <- PSRCData::prdat

model_name <- "mode_cleaned_aux"

# import clean pids

cleanpids <- read_rds(here("analysis/data/derived_data/clean_pids.rds"))

# filter
# prdat <- prraw %>% filter(personid %in% cleanpids)
trdat <- trraw %>% filter(personid %in% cleanpids)



mode <- trdat %>%
  # my function to make dummy variables from one variable in the dataset (`main_mode`)
  make_dummies(mode_full_EM) # different mode var from previously: separates passengers from carpool drivers

rm(list=setdiff(ls(), c("mode", "model_name", "cleanpids")))


# add covariates ##################################################################

auxvars <- read_rds(here("analysis/data/derived_data/curated-auxiliary-vars.rds")) %>%

  # removing under 18 ppl
  filter(agegrp >= "age18_34") %>%
  mutate(across(where(is.ordered), ~ factor(.x, ordered = FALSE))) %>%
  filter(personid %in% cleanpids)

mode_cov <- auxvars %>%
  left_join(mode, by = "personid") %>%
  select(names(mode), everything()) %>%
  mutate(personid = as.numeric(personid)) %>%
  janitor::clean_names()

mode_cov_num <- mode_cov %>%
  mutate(across(where(is.factor), as.numeric))



# Create model syntax ##################################################################

# if the mplus folder does not exist in /analysis/, then create it
dir.create(here("analysis/Mplus/"))

create_model_dirs(model_name)

model_path <- paste0("analysis/Mplus/", model_name, "/")
model_template <-  paste0(model_path, "template/")

# # my function to write mplus data to file in the right file location:
# write_mplus_data(df = mode_cov,
#                  wd_for_analysis = here(model_path),
#                  filename = paste0(model_name, "-data-mplus-ready.dat"),
#                  writeData = "ifmissing",
#                  hashfilename = TRUE,
#                  dummyCode = c())


names_fac_vars <- mode_cov %>%
  dplyr::select((where(is.factor))) %>%
  names()

write_mplus_data(df = mode_cov_num,
                 wd_for_analysis = here(model_path),
                 filename = paste0(model_name, "-data-mplus-ready.dat"),
                 writeData = "ifmissing",
                 hashfilename = TRUE,
                 dummyCode = names_fac_vars)

# THEN: MANUALLY write template file ---------------------------

# THEN RUN the following: ----------------

#  This section could become a function on its own
templatefile_path <- paste0(model_template, model_name, "_template.txt")

MplusAutomation::createModels(templatefile = templatefile_path)


# Next pieced of code copies .inp files to the model folder from the template folder,
# BUT ONLY IF THE INPUT FILES DO NOT ALREADY EXIST
# IF THEY DO EXIST, they will not be overwritten. This protects changes made manually to input files to
# optimize model results

inp_list <- list.files(model_template, pattern = ".inp", full.names = TRUE)

file.copy(from = inp_list, to = model_path,
          recursive = FALSE,
          overwrite = FALSE)
