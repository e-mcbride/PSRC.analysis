# Building cluster variables and modifying SES variables, then combining
library(tidyverse)

clusterward <- read_rds(here::here("analysis/data/derived_data/cluster-ward_5min.rds"))

# final clustering decision ================================
cluster <- cutree(clusterward, k = 6) %>%
  factor(labels=paste("Type", 1:6))

level_key <- list(`Type 1` = "Home Day",
                  `Type 2` = "School Day",
                  `Type 3` = "Typical Work Day",
                  `Type 4` = "Errands Type 1",
                  `Type 5` = "Mostly Out of Home",
                  `Type 6` = "Errands Type 2",
                  `Type 7` = "Non-typical Work Day",
                  `Type 8` = "Leave Home",
                  `Type 9` = "Traveling")


# minutes by state
minbystate <- pl.seq.5min %>%
  as_tibble(rownames = "personid") %>%
  gather(-personid, key = "time", value = "place") %>%
  group_by(personid, place) %>%
  count(name = "minutes") %>%
  ungroup() %>%
  spread(key = place, value = "minutes", fill = 0) %>%
  rename(min_NA = `*`, min_G = "Gshop", min_H = "Home", min_O = "Other", min_S = "School", min_T = "Travel", min_W = "Work")
