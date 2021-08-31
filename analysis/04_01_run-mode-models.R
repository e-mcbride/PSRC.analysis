# 04_01 Run models

library(here)
library(MplusAutomation)

# NOTE: before running the following, all .inp files in "analysis/03_Mplus/mode/template"
#       must be manually copied to "analysis/03_Mplus/mode/". This is to protect any changes
#       made to the .inp files from being overwritten by 02_01 script when the template
#       file is used to build the initial .inp files.
MplusAutomation::runModels(
  here("analysis/03_Mplus/mode/"),
       recursive=FALSE)


