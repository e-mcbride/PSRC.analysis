# do the categorical PCA with the OG data (not shrunken)

library(tidyverse)
library(Gifi)

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

# build og aux data ==================================
ogdat <- all_aux_vars %>%
  select(personid,
         wbt_transitmore_1,
         wbt_transitmore_2,
         wbt_transitmore_3,
         wbt_bikemore_1,
         wbt_bikemore_2,
         wbt_bikemore_3,
         wbt_bikemore_4,
         wbt_bikemore_5,
         starts_with("HH_res_factors"),
         starts_with("mode_freq"),
         agegrp,
         race = race_category,
         gender,
         schooltype,
         worker,
         hinclv = HH_inc_lvl,
         HH_Age00_04,
         HH_Age05_15,
         HH_Age16_17,
         license,
         cmplxty = C,
         clustno) %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("hh_res_factors_", "res", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("wbt_transitmore", "ut", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("wbt_bikemore", "ub", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("mode_freq_", "modef", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("hh_age", "n", .x, fixed = TRUE))%>%
  rename_with(stringr::str_trunc, -personid, width = 6, side = "right", ellipsis = "")

# context vars ===================================
cleanpids <- read_rds(here::here("analysis/data/derived_data/clean_pids.rds"))

lcadat <- MplusAutomation::readModels(here::here("analysis/Mplus/mode_cleaned_aux/6-class_lca_mode_cleaned_aux.out"),
                                      what = "savedata")$savedata %>% janitor::clean_names()

c6 <- lcadat %>%
  mutate(personid = as.character(personid)) %>%
  select(personid, c) %>%
  mutate(c = as.factor(c))

aux_c6 <- ogdat %>%
  filter(agegrp >= "age18_34") %>%
  filter(personid %in% cleanpids) %>%
  left_join(c6, by = "personid")

rm(list = setdiff(ls(), c("ogdat", "aux_c6")))

context <- aux_c6 %>%
  select(personid, agegrp, race, gender, worker, school, hinclv,
         n00_04, n05_15, n16_17, licens, c) %>%
  mutate(across(where(is.factor), droplevels)) %>%
  column_to_rownames("personid") %>%
  filter_all(~!is.na(.))


# run `princals()` on og data ========================================

cxt.ordered <- context %>%
  map_lgl(~ (is.ordered(.x)))

cxt.ordered
catpca <- context %>%
  # make n kids binary
  # mutate(across(starts_with("n"), ~ as.numeric(.x > 1))) %>%
  # select(-c) %>%
  select(-c, -starts_with("n")) %>%
  princals(ndim = 5, ordinal = FALSE)# cxt.ordered)

summary(catpca)

plot(catpca$objectscores, xlab = "dim1", ylab = "dim2", col = "RED", cex = .5)
plot(catpca, plot.type = "biplot", cex.scores = 0.5)
plot(catpca, plot.type = "loadplot")
plot(catpca, plot.type = "screeplot")
plot(catpca, plot.type = "transplot")



