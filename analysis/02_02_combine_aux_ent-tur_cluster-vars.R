# Building the final auxiliary variable dataset
# joining SES, entropy/turbulence, time allocation, cluster id
# paring down to the variables I will use in my analysis

library(dplyr)
library(readr)

# Building complete dataset of auxiliary variable options

sesvars <- read_rds(here::here("analysis/data/derived_data/pid_SES.rds"))

pl.en_tu_com <- read_rds(here::here("analysis/data/derived_data/ent-tur-com_place-seq.rds")) %>%
  rename(personid = pid)

seq_vars <- read_rds(here::here("analysis/data/derived_data/pid_timealloc-6clust.rds")) %>%
  mutate(clustno = ordered(clustno, c(1,2,3,4,5,6)))

all_aux_vars <-  sesvars %>%
  left_join(pl.en_tu_com, by = "personid") %>%
  left_join(seq_vars, by = "personid")


curated_auxvars <- all_aux_vars %>%
  select(personid,
         # Intention #####################################################################
         ## Attitude =====================================================================
         ### Preferences --------------------------------------------
         starts_with("wbt_"),

         ### Values and Beliefs
         starts_with("HH_res_factors"),

         ## Social Factors ===============================================================
         relationship,

         # Habit #########################################################################
         starts_with("mode_freq"), # do they have habits besides car use

         # Context/Facilitating Conditions ###############################################
         ## Socioeconomic Status =========================================================
         agegrp,
         race_category,
         gender,
         schooltype,
         worker,
         starts_with("HH_Age"),
         HH_inc_lvl,

         ## Access to Modes ==============================================================
         license,

         ## Spatiotemporal Structures ====================================================
         C,
         clustno
  ) %>%
  janitor::clean_names() %>%
  rename(fqtran = mode_freq_1,
         fqbike = mode_freq_2,
         fqwalk = mode_freq_3,
         fqcsha = mode_freq_4,
         fqrsha = mode_freq_5,
         cmplxty = c,
         race = race_category) %>%
  rename_with(~ gsub("hh_age", "n", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("hh_res_factors_", "res", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("wbt_transitmore_", "usetr", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("wbt_bikemore_", "usebk", .x, fixed = TRUE)) %>%
  rename_with(gsub, hh_inc_lvl, pattern = "_", replacement = "", fixed = TRUE) %>%
  rename_with(stringr::str_trunc, -personid, width = 6, side = "right", ellipsis = "")


write_rds(curated_auxvars, here::here("analysis/data/derived_data/curated-auxiliary-vars.rds"))


