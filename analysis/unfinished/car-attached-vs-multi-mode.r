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
### * RM mode_frq_4 bc it's "carshare" which is vehicle
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
aux_sm <- all_aux_vars %>%
  filter(personid %in% cleanpids) %>%
  filter(agegrp >="age18_34") %>%
  mutate(across(where(is.factor), ~ droplevels(.x))) %>%
  transmute(personid,

            onlycar = (personid %in% no_other_modes), # EM!! this puts the NA responders in the FALSE category

            # residence

            agegrp,
            racwht =   as.numeric(race_category == "White Only"),
            racasn =   as.numeric(race_category == "Asian"),
            rachis =    as.numeric(race_category == "Hispanic"),
            racblk =   as.numeric(race_category == "African American"),
            racoth =
              (race_category != "White Only" &
                 race_category != "Asian" &
                 race_category != "Hispanic" &
                 race_category != "African American"),
            female = as.numeric(gender == "Female"),

            license = as.numeric(license == "Yes, has an intermediate or unrestricted license"),

            school = as.numeric(schooltype != "None (adult)"), # 34 in hs, 689 in upper ed/trade sch
            worker = as.numeric(worker != "No jobs"),

            #hh chars
            hinclo = (HH_inc_lvl == "Below SSS"),
            h00_04 = (HH_Age00_04 > 0),
            h05_15 = (HH_Age05_15 > 0),
            h16_17 = (HH_Age16_17 > 0),


            clustno)


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

