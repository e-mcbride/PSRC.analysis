# 03_02 Run models in Mplus

library(here)
library(MplusAutomation)


# This short script interfaces between Mplus and R to run models
# Separated into its own script to reduce chances of running accidentally
##  If run accidentally, it will just take a long time to complete
##  (won't overwrite input files, so model results should be the same)

model_name <- "mode_cleaned_aux"
model_path <- paste0("analysis/Mplus/", model_name, "/")

MplusAutomation::runModels(
  here(model_path),
  recursive=FALSE)

