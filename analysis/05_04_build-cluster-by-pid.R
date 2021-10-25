# cluster-timealloc_by-pid
library(tidyverse)

cluster_id <- read_rds(here::here("analysis/data/derived_data/pid_all-clusters.rds"))


# six cluster decision ==========================================================
level_key <- list(type1 = "Home Day",
                  type2 = "Typical Work Day",
                  type3 = "School Day",
                  type4 = "Other Outings Day",
                  type5 = "Atypical Work Day",
                  type6 = "Traveling")

sixclust <- cluster_id %>% select(personid, c6 = nclust6) %>%
  mutate(c6str = factor(c6, labels = paste0("type", 1:6))) %>%
  mutate(namedcluster = recode_factor(c6str, !!!level_key)) %>%
  select(-c6str) %>%
  rename(clustno = c6)


# minutes by state ########################################################################

pl.seq.5min <- read_rds(here::here("analysis/data/derived_data/pl_seq_5min.rds"))

minbystate <- pl.seq.5min %>%
  as_tibble(rownames = "personid") %>%
  gather(-personid, key = "time", value = "place") %>%
  group_by(personid, place) %>%
  count(name = "minutes") %>%
  ungroup() %>%
  spread(key = place, value = "minutes", fill = 0) %>%
  rename(min_NA = `*`, min_G = "Gshop", min_H = "Home", min_O = "Other", min_S = "School", min_T = "Travel", min_W = "Work")

seq_vars <- minbystate %>%
  left_join(sixclust, by = "personid")

write_rds(seq_vars, here::here("analysis/data/derived_data/pid_timealloc-6clust.rds"))



# write_rds(sixclust, here::here("analysis/data/derived_data/six-cluster-by-pids.rds"))

# unused, but preserved for now ########################################
# cluster <- cutree(clusterward, 6)%>%
#   factor(labels=paste0("type", 1:6))
#
# alldata <- cbind(ses_en_tu, cluster) %>%
#   mutate(namedcluster = recode_factor(cluster, !!!level_key))

# seqplot <- seqfplot(pl.seq.5min, group = cl6)
#
# seqplot
#
# seqmtplot(pl.seq.5min, group = cl6)
