library(dplyr)
library(readr)
cluster_id <- read_rds(here::here("analysis/data/derived_data/all-clusters-by-pid.rds"))


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
  select(-c6str)

write_rds(sixclust, here::here("analysis/data/derived_data/six-cluster-by-pids.rds"))

# unused, but preserved for now ########################################
# cluster <- cutree(clusterward, 6)%>%
#   factor(labels=paste0("type", 1:6))
#
# alldata <- cbind(aux_en_tu, cluster) %>%
#   mutate(namedcluster = recode_factor(cluster, !!!level_key))

# seqplot <- seqfplot(pl.seq.5min, group = cl6)
#
# seqplot
#
# seqmtplot(pl.seq.5min, group = cl6)
