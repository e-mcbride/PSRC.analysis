# 02_02 Run models

library(here)
library(MplusAutomation)

model_name <- "mode_cleaned"
model_path <- paste0("analysis/03_Mplus/", model_name, "/")

MplusAutomation::runModels(
  here(model_path),
  recursive=FALSE)
