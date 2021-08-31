# 04_01 Run models

library(here)
library(MplusAutomation)

MplusAutomation::runModels(
  here("analysis/03_Mplus/mode/"),
       recursive=FALSE)


