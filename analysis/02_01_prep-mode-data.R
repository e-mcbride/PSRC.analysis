# nubmer of times used each mode on diary day

library(tidyverse)
library(here)
library(janitor)

trdat <- read_rds(here("analysis/data/derived_data/trdat.rds"))


# people who made no trips are missing from the `travel` data, so I add them in with the following
prdat <- read_rds(here("analysis/data/derived_data/prdat.rds"))

trpid <- trdat %>%
  pull(personid) %>%
  unique()

notrav <- prdat %>%
  filter(!(person_id %in% trpid)) %>%
  select(personid = person_id) %>%
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
    bind_rows(notrav)



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

  #
  # if(!dir.exists(analysis_path)){
  #   dir.create(analysis_path)
  # }
  # if(!dir.exists(template_path)){
  # dir.create(template_path)
  # }
  # message("Both directories already exist")

}

# create_model_dirs("HEYO")


model_name <- "mode"
model_template <- paste0("analysis/03_Mplus/", model_name, "-template/")

# model_name <- "WHY"
# analysis_path <- paste0("analysis/03_Mplus/", model_name, "/")
# analysis_path
#
# template_path <- paste0(analysis_path, "template/")
# template_path
#
# template_path <- paste0("analysis/03_Mplus/", mode_n, "-template/")
#
#
# dir.create(here(template_path))
# dir.exists(here(template_path))


write_mplus_data(df = mode,
                 wd_for_analysis = here(template_path),
                 filename = paste0(mode_n, "-data-mplus-ready.dat"),
                 writeData = "ifmmissing",
                 hashfilename = TRUE)
