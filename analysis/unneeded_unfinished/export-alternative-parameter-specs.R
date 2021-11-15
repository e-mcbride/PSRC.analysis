# model with auxiliary: figures
library(tidyverse)
# library(MplusAutomation)


c6_models <-
  MplusAutomation::readModels(here::here("analysis/Mplus/mode_cleaned_aux/final-model-work-c6/"),
             recursive = FALSE
  ) %>%
  set_names(~ .x %>%
                     str_replace(".out", replacement = "") %>%
                     str_split("_cleaned_") %>%
                     map(2) %>%
                     str_replace("aux_03_", "m")
  )

# read in my function `alt_param` to extract the alternative parameterizations from a `mplus.model`:
source(here::here("analysis/90_01_fxn_extract-mplus-alternative-parameterizations.R"))

# Model 3A ##########################################################


## female * sequences ===================================
# c6_models$m03a_fem.seq.interactions %>%
#   alt_param(refClass = 4, modelID = "M3A_fem_sq_rmcarless_nadlt") %>%
#   write.table("clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)

# m3a_ap4 <- m3a %>%
#   alt_param(refClass = 4, modelID = "M3A_fem_sq_rmcarless_nadlt")

## female * complexity

to_table_process <- function(mplusModel, model.ID) {
  mplusModel %>%
    alt_param(refClass = 4, modelID = model.ID) %>%
    # mutate(var_id = if_else(var_id == "NUFVHS", "NUFVHSLIC", var_id)) %>%
    write.table("clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)
}

c6_models$m03a_fem.cmplx.interactions %>%
  to_table_process(model.ID = "M3A_fem_cmplx_nuflic2")

# Model 0 ##########################################################
c6_models$m00_baseline %>%
  to_table_process(model.ID = "M0_baseline")


# Model 1 ##########################################################
c6_models$m01_ses %>%
  totable_process(model.ID = "M1_SES")

# Model 2 ##########################################################
c6_models$m02_sesatts %>%
  to_table_process("M2_SES_atts")
