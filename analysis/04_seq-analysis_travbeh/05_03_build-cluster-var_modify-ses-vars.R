# Building the data with the desired number of clusters to examine

library(tidyverse)

grp_en_tu <- read_rds(here::here("analysis/data/derived_data/grouping-variables.rds"))

clusterward <- read_rds(here::here("analysis/data/derived_data/cluster-ward_5min.rds"))

# final clustering decision ================================
cluster <- cutree(clusterward, k = 9) %>%
  factor(labels=paste("Type", 1:9))

level_key <- list(`Type 1` = "Home Day",
                  `Type 2` = "School Day",
                  `Type 3` = "Typical Work Day",
                  `Type 4` = "Errands Type 1",
                  `Type 5` = "Mostly Out of Home",
                  `Type 6` = "Errands Type 2",
                  `Type 7` = "Non-typical Work Day",
                  `Type 8` = "Leave Home",
                  `Type 9` = "Traveling")

# Testing different number of clusters by changng everywehre it says `10` to change n clusters



