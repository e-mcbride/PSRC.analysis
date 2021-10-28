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
  rename(cmplxty = c, race = race_category) %>%
  rename_with(~ gsub("hh_age", "n", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("hh_res_factors_", "res", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("wbt_transitmore_", "usetr", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("wbt_bikemore_", "usebk", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("mode_freq_", "moden", .x, fixed = TRUE)) %>%
  rename_with(gsub, hh_inc_lvl, pattern = "_", replacement = "", fixed = TRUE) %>%
  rename_with(stringr::str_trunc, -personid, width = 6, side = "right", ellipsis = "")


write_rds(curated_auxvars, here::here("analysis/data/derived_data/curated-auxiliary-vars.rds"))


# ERASE
# Building cluster variables and modifying SES variables, then combining
# library(tidyverse)
#
#
#
#
#
# pl.seq.5min <- read_rds(here::here("analysis/data/derived_data/pl_seq_5min.rds"))
#
# # minutes by state
# minbystate <- pl.seq.5min %>%
#   as_tibble(rownames = "personid") %>%
#   gather(-personid, key = "time", value = "place") %>%
#   group_by(personid, place) %>%
#   count(name = "minutes") %>%
#   ungroup() %>%
#   spread(key = place, value = "minutes", fill = 0) %>%
#   rename(min_NA = `*`, min_G = "Gshop", min_H = "Home", min_O = "Other", min_S = "School", min_T = "Travel", min_W = "Work")
#
# sixclust <- read_rds(here::here("analysis/data/derived_data/six-cluster-by-pids.rds")) %>%
#   rename(clustnum = c6)
#
# frag_vars <- minbystate %>%
#   left_join(sixclust, by = "personid")
#
# write_rds(frag_vars, here::here("analysis/data/derived_data/frag_6clust_by_pid.rds"))


#
# ses_en_tu <- read_rds(here::here("analysis/data/derived_data/auxiliary-entropy-turbulence.rds"))
#
# ses_frag_trav <- ses_en_tu %>%
#   left_join(frag_vars, by = "personid")
#
# write_rds(ses_frag_trav, here::here("analysis/data/derived_data/auxiliary_ent-tur_traveldat.rds"))
#
# # make dataset with factors converted to numeric
# factors2numeric <- ses_frag_trav %>%
#   mutate(across(where(is.factor), ~ as.numeric(.x)))

# data.frame(levels(ses_frag_trav$employment), as.num)
#
# x <- levels(ses_frag_trav$employment) %>%
#   as_tibble()
# aux
