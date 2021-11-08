# car attached persons versus multimode
## * did they use only cars, or did they use multiple modes
library(tidyverse)

# Building complete dataset of auxiliary variable options ===========================================
prdat <- PSRCData::prdat

cleanpids <- read_rds(here::here("analysis/data/derived_data/clean_pids.rds"))

sesvars <- read_rds(here::here("analysis/data/derived_data/pid_SES.rds"))

pl.en_tu_com <- read_rds(here::here("analysis/data/derived_data/ent-tur-com_place-seq.rds")) %>%
  rename(personid = pid)

seq_vars <- read_rds(here::here("analysis/data/derived_data/pid_timealloc-6clust.rds")) %>%
  mutate(clustno = ordered(clustno, c(1,2,3,4,5,6)))

all_aux_vars <-  sesvars %>%
  left_join(pl.en_tu_com, by = "personid") %>%
  left_join(seq_vars, by = "personid")

## Pull only car ppl ==================
### * RM mode_frq_4 bc it's "carshare" which is using a vehicle
### * also removing mode 3 (walking) cuz it kinda messes it up cuz so many ppl "go on walks"
### * excluding "I never do this" and "I do this, but not in the past 30 days"

no_other_modes <- prdat %>% #2862
  filter(age_category != "Under 18 years") %>%
  filter(personid %in% cleanpids) %>%
  filter(str_detect(vehicleused, "vehicle")) %>% # this answers "do they use a vehicle"
  select(personid, starts_with("mode_freq"), -mode_freq_3, -mode_freq_4) %>%
  pivot_longer(-personid, names_to = "travmode", values_to = "freq") %>%
  mutate(doit = as.numeric(freq != "I never do this" &
                             freq != "I do this, but not in the past 30 days")) %>%
  select(-freq) %>%
  group_by(personid) %>%
  summarize(nmodes = sum(doit)) %>%
  filter(nmodes == 0) %>%
  pull(personid)

# building curated set =============================================
curated_auxvars <- all_aux_vars %>%
  # filter(personid %in% cleanpids) %>% #
  # filter(agegrp >="age18_34") %>%
  # mutate(across(where(is.factor), ~ droplevels(.x))) %>% # this and 2 above: include here or next script? Probably in next
  transmute(personid,
            # Intention #####################################################################
            ## Attitude =====================================================================
            ### Preferences --------------------------------------------
            ut_saf = wbt_transitmore_1 >= "Occasionally (1-2 days per week)", # wording these:
            ut_frq = wbt_transitmore_2 >= "Occasionally (1-2 days per week)", # be willing to use tr/bk 1+ times/ week with
            ut_rel = wbt_transitmore_3 >= "Occasionally (1-2 days per week)", # right conditions (?)
            ub_shr = wbt_bikemore_1 >= "Occasionally (1-2 days per week)",
            ub_grn = wbt_bikemore_2 >= "Occasionally (1-2 days per week)",
            ub_lan = wbt_bikemore_3 >= "Occasionally (1-2 days per week)",
            ub_rln = wbt_bikemore_4 >= "Occasionally (1-2 days per week)",
            ub_amn = wbt_bikemore_5 >= "Occasionally (1-2 days per week)",

            ### Values and Beliefs --------------------------------------
            across(starts_with("HH_res_factors"), ~ .x >="Somewhat important"),

            # Habit(kind of) #########################################################################
            carlvr = (personid %in% no_other_modes), # EM!! this puts the NA responders in the FALSE category

            # Context/Facilitating Conditions ###############################################
            ## Socioeconomic Status =========================================================
            agegrp,
            racwht =   (race_category == "White Only"),
            racasn =   (race_category == "Asian"),
            rachis =    (race_category == "Hispanic"),
            racblk =   (race_category == "African American"),
            racoth =
              (race_category != "White Only" &
                 race_category != "Asian" &
                 race_category != "Hispanic" &
                 race_category != "African American"),
            female = (gender == "Female"),
            school = (schooltype != "None (adult)"), # 34 in hs, 689 in upper ed/trade sch
            worker = (worker != "No jobs"),
            #hh chars
            hinclo = (HH_inc_lvl == "Below SSS"),
            n00_04 = (HH_Age00_04 > 0),
            n05_15 = (HH_Age05_15 > 0),
            n16_17 = (HH_Age16_17 > 0),
            nufvhs = HH_nufvhs,
            carlss = HH_carless,

            ## Access to Modes ==============================================================
            license = (license != "No, does not have a license or permit"),

            ## Spatiotemporal Structures ====================================================
            cmplxty = C,
            seq = clustno
  ) %>%
  mutate(across(where(is.logical), ~as.numeric(.x))) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("hh_res_factors_", "res", .x, fixed = TRUE)) %>%
  rename_with(stringr::str_trunc, -personid, width = 6, side = "right", ellipsis = "")

write_rds(curated_auxvars, here::here("analysis/data/derived_data/curated-auxiliary-vars.rds"))


# Rejects  ========================================
# no_other_modes <- prdat %>% #1009
#   filter(age_category != "Under 18 years") %>%
#   filter(personid %in% cleanpids) %>%
#   filter(str_detect(vehicleused, "vehicle")) %>% # this answers "do they use a vehicle"
#   select(personid, starts_with("mode_freq"), -mode_freq_4) %>%
#   pivot_longer(-personid, names_to = "travmode", values_to = "freq") %>%
#   mutate(doit = as.numeric(freq != "I never do this" &
#                              freq != "I do this, but not in the past 30 days")) %>%
#   select(-freq) %>%
#   group_by(personid) %>%
#   summarize(nmodes = sum(doit)) %>%
#   filter(nmodes == 0)

# mutate(multmod = nmodes != 0)

# ## also excluding "1-3 times in the past 30 days"
#
# no_other_modes <- prdat %>% #1659
#   filter(age_category != "Under 18 years") %>%
#   filter(personid %in% cleanpids) %>%
#   filter(str_detect(vehicleused, "vehicle")) %>% # this answers "do they use a vehicle"
#   select(personid, starts_with("mode_freq"), -mode_freq_4) %>%
#   pivot_longer(-personid, names_to = "travmode", values_to = "freq") %>%
#   mutate(doit = as.numeric(freq != "I never do this" &
#                              freq != "I do this, but not in the past 30 days" &
#                              freq != "1-3 times in the past 30 days")) %>%
#   select(-freq) %>%
#   group_by(personid) %>%
#   summarize(nmodes = sum(doit)) %>%
#   filter(nmodes == 0)
#
# ## try removing mode3 (walking) cuz it kinda messes it up cuz so many ppl "go on walks"
#
# excl_walk <- prdat %>% #4324
#   filter(age_category != "Under 18 years") %>%
#   filter(personid %in% cleanpids) %>%
#   filter(str_detect(vehicleused, "vehicle")) %>% # this answers "do they use a vehicle"
#   select(personid, starts_with("mode_freq"), -mode_freq_3, -mode_freq_4) %>%
#   pivot_longer(-personid, names_to = "travmode", values_to = "freq") %>%
#   mutate(doit = as.numeric(freq != "I never do this" &
#                              freq != "I do this, but not in the past 30 days" &
#                              freq != "1-3 times in the past 30 days")) %>%
#   select(-freq) %>%
#   group_by(personid) %>%
#   summarize(nmodes = sum(doit)) %>%
#   filter(nmodes == 0)
#
# # shrink down categories
# mutate(across(starts_with("HH_res_factors"),
#               ~ case_when(
#                 .x %in% "Very unimportant" ~ "Unimportant",
#                 .x %in% "Somewhat unimportant" ~ "Unimportant",
#                 .x %in% "Neither or N/A" ~ "Neither or N/A",
#                 .x %in% "Somewhat important"~ "Important",
#                 .x %in% "Very important" ~ "Important",
#                 TRUE ~ "THERES AN ISSUE"
#               )),
#        across(starts_with("wbt_"),
#               ~case_when(
#
#               ))
# )
