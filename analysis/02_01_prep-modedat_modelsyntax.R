# Prep mode data and create model syntax
# this model will be with the "bad responses" removed that I found during the sequence analysis

library(tidyverse)
library(here)
library(janitor)
# library(MplusAutomation)

devtools::load_all(helpers = FALSE)

trraw <- PSRC.data::trdat #read_rds(here("analysis/data/derived_data/trdat.rds"))

# people who made no trips are missing from the `travel` data, so I add them in with the following
prraw <- PSRC.data::prdat #read_rds(here("analysis/data/derived_data/prdat.rds"))



# Bring in ids that are NOT in OG trdat =======================================================

tr_pid <- trraw %>%
  dplyr::pull(personid) %>%
  unique()


# 1.  Get pids of all hh members where whole hh was missing from trraw
### Get the hhids that are not present in trraw. These are hhs w/ nobody who traveled
tr_hid <- trraw %>%
  pull(hhid) %>%
  unique()

pr_hid <- prraw %>%
  pull(hhid) %>%
  unique()

hhs_notr_hid <- pr_hid[!(pr_hid %in% tr_hid)] # hhids


### get pids
hhs_notr_pid <- prraw %>%
  filter(hhid %in% hhs_notr_hid) %>%
  pull(personid) %>%
  unique()
# ^ these can be safely added to the travel data


# 2. Get the records where somebody from a hh in trdat ("clean" dataset) didn't travel.
clean_hids <- read_rds(here("analysis/data/derived_data/clean_hids.rds"))

# get pids of ppl who didn't record travel
pr_notr_pid  <- prraw %>%
  filter(!(personid %in% tr_pid)) %>%
  filter(hhid %in% clean_hids) %>%
  pull(personid) %>%
  unique()

# 3. Combine people whose entire hh is not present in trdat + ppl who do come from hhs with travelers

notr_pid_combo <- c( hhs_notr_pid, pr_notr_pid) %>% unique()

rm(list = setdiff(ls(), c("prraw", "trraw", "notr_pid_combo")))

#  ===============================================================

# import clean pids

cleanpids <- read_rds(here("analysis/data/derived_data/clean_pids.rds")) %>%
  # add the new pid list to these
  c(notr_pid_combo) %>%
  unique()


# Save person- and hh-lvl datasets (or ids) that has the new final list of ppl and hhs
write_rds(cleanpids, here("analysis/data/derived_data/clean_pids_notr.rds"))

# filter
prdat <- prraw %>% filter(personid %in% cleanpids)
trdat <- trraw %>% filter(personid %in% cleanpids)


mode <- trdat %>%
  # my function to make dummy variables from one variable in the dataset (`main_mode`)
  make_dummies(main_mode) %>%

  # my fn to add the non-travelers to the dummy dataset
  add_nontravelers(prraw = prraw, notr_pids = notr_pid_combo)

rm(list=setdiff(ls(), "mode"))

# Create model syntax ##################################################################

# if the mplus folder does not exist in /analysis/, then create it
dir.create(here("analysis/Mplus/"))

model_name <- "mode_cleaned_notr"

create_model_dirs(model_name)


model_path <- paste0("analysis/Mplus/", model_name, "/")
model_template <-  paste0(model_path, "template/")

# my function to write mplus data to file in the right file location:
write_mplus_data(df = mode,
                 wd_for_analysis = here(model_path),
                 filename = paste0(model_name, "-data-mplus-ready.dat"),
                 writeData = "ifmissing",
                 hashfilename = TRUE)


dir.exists(here(model_path))


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
