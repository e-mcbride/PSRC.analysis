# model with auxiliary: figures
library(tidyverse)
# library(MplusAutomation)

source(here::here("analysis/90_01_fxn_extract-mplus-alternative-parameterizations.R"))

to_table_process <- function(mplusModel, model.ID) {
  mplusModel %>%
    alt_param(refClass = 4, modelID = model.ID) %>%
    # mutate(var_id = if_else(var_id == "NUFVHS", "NUFVHSLIC", var_id)) %>%
    write.table("clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)
}

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
# Model 3A ##########################################################


## female * sequences ===================================
c6_models$m03a_fem.seq.interactions %>%
  alt_param(refClass = 4, modelID = "M3A_fem_sq_rmcarless_nadlt") %>%
  write.table("clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)

# m3a_ap4 <- m3a %>%
#   alt_param(refClass = 4, modelID = "M3A_fem_sq_rmcarless_nadlt")

## female * complexity ==========================================

c6_models$m03a_fem.cmplx.interactions %>%
  to_table_process(model.ID = "M3A_fem_cmplx")

# Model 0 ##########################################################
c6_models$m00_baseline %>%
  to_table_process(model.ID = "M0_baseline2")

## trying different reference class to see what happens
c6_models$m00_baseline %>%
  alt_param(refClass = 1, modelID = "M0_baseline2_refc1") %>%
  write.table("clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)




# Model 1 ##########################################################
c6_models$m01_ses %>%
  to_table_process(model.ID = "M1_SES_schincyng")

m01_cor <- c6_models$m01_ses$sampstat$correlations

m01_hi_cor <- ((abs(m01_cor) > 0.09) * m01_cor)
View(m01_hi_cor)

# x <- (m01_cor[(abs(m01_cor) > 0.1)])


# Model 2 ##########################################################
c6_models$m02_sesatts %>%
  to_table_process("M2_SES_atts")

m02_cor <- c6_models$m02_sesatts$sampstat$correlations

m02_hi_cor <- ((abs(m02_cor) > 0.09) * m02_cor)
View(m02_hi_cor)

m02_hi_cor %>% write.table("clipboard", sep = "\t")

# Goodness of Fit #########################################


GoF <- c6_models %>%
  map(~.x$summaries %>% select(LL, AIC, BIC, aBIC, Entropy))


# Class Sizes #############################################

c6_models$m00_baseline$class_counts$mostLikely

# get counts from data
lca6sav <- MplusAutomation::readModels(here::here("analysis/Mplus/mode_cleaned_aux/6-class_lca_mode_cleaned_aux.out"), what = "savedata")$savedata

rmnas <- lca6sav %>%
  pivot_longer(cols = -PERSONID) %>%
  filter(value == 9999) %>%
  pull(PERSONID) %>% unique()


lca6sav %>%
  filter(!(PERSONID %in% rmnas)) %>%
  group_by(C) %>%
  summarise(n())
