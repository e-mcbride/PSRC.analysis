# Building cluster variables and modifying SES variables, then combining
library(tidyverse)

# clusterward <- read_rds(here::here("analysis/data/derived_data/cluster-ward_5min.rds"))
#
# # final clustering decision ================================
# cluster <- cutree(clusterward, k = 6) %>%
#   factor(labels=paste("Type", 1:6))
#
# # level_key <- list(`Type 1` = "Home Day",
# #                   `Type 2` = "School Day",
# #                   `Type 3` = "Typical Work Day",
# #                   `Type 4` = "Errands Type 1",
# #                   `Type 5` = "Mostly Out of Home",
# #                   `Type 6` = "Errands Type 2",
# #                   `Type 7` = "Non-typical Work Day",
# #                   `Type 8` = "Leave Home",
# #                   `Type 9` = "Traveling")
#



pl.seq.5min <- read_rds(here::here("analysis/data/derived_data/pl_seq_5min.rds"))

# minutes by state
minbystate <- pl.seq.5min %>%
  as_tibble(rownames = "personid") %>%
  gather(-personid, key = "time", value = "place") %>%
  group_by(personid, place) %>%
  count(name = "minutes") %>%
  ungroup() %>%
  spread(key = place, value = "minutes", fill = 0) %>%
  rename(min_NA = `*`, min_G = "Gshop", min_H = "Home", min_O = "Other", min_S = "School", min_T = "Travel", min_W = "Work")

sixclust <- read_rds(here::here("analysis/data/derived_data/six-cluster-by-pids.rds")) %>%
  rename(clustnum = c6)

frag_vars <- minbystate %>%
  left_join(sixclust, by = "personid")

aux_en_tu <- read_rds(here::here("analysis/data/derived_data/auxiliary-entropy-turbulence.rds"))

aux_frag_trav <- aux_en_tu %>%
  left_join(frag_vars, by = "personid")

write_rds(aux_frag_trav, here::here("analysis/data/derived_data/auxiliary_ent-tur_traveldat.rds"))

# make dataset with factors converted to numeric
factors2numeric <- aux_frag_trav %>%
  mutate(across(where(is.factor), ~ as.numeric(.x)))

# data.frame(levels(aux_frag_trav$employment), as.num)
#
# x <- levels(aux_frag_trav$employment) %>%
#   as_tibble()
# aux
