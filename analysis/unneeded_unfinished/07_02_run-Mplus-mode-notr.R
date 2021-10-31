# 07_02 Run models

library(here)
library(MplusAutomation)

model_name <- "mode_cleaned_notr"
model_path <- paste0("analysis/Mplus/", model_name, "/")

MplusAutomation::runModels(
  here(model_path),
  recursive=FALSE)
