# Building the data with the desired number of clusters to examine

library(tidyverse)
library(TraMineR)

clusterward <- read_rds(here::here("analysis/data/derived_data/cluster-ward_5min.rds"))
clust_pids <- clusterward$order.lab # pull the pids of the ppl who made it to the clustering

grp_en_tu <- read_rds(here::here("analysis/data/derived_data/grouping-variables.rds")) %>%
  filter(personid %in% clust_pids) # filter so the ppl included in clustering are only ones here

pl.seq <- read_rds(here::here("analysis/data/derived_data/pl_seq.rds"))

column.5min <- seq(from = 1, to = 1440, by = 5)
pl.seq.5min <- pl.seq[, column.5min]
rm(pl.seq)

# reduce so it's only every 5 minutes



# plot(clusterward, which.plots = 2, rotate = TRUE)
plot(clusterward)


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



# Testing different number of clusters by changing everywhere it says `10` to change n clusters ==============

# allclusters <- clusterward %>% map(1:10, ~cutree(.x))

allclusters <- cutree(clusterward, 1:10)

cl6 <- factor(cutree(clusterward, 6), labels = paste0("type", 1:6))

table(cl6)


# cutree(clusterward, k = 10)

# cutree_list <- function(agnes_obj, max_k = 5) {
#   clust_list <- list()
#   for(i in 1:max_k) {
#     clust_list[[i]] <- cutree(clusterward, i)
#   }
#   return(clust_list)
# }

allclusters <- cutree_list(clusterward, max_k = 10) %>%
  as_tibble(.name_repair = make.names)

seqplot <- seqfplot(pl.seq.5min, group = cl6)

seqplot
#
# seqmtplot(pl.seq.5min, group = cl6)

# alldata <- cbind(grp_en_tu, allclusters)
