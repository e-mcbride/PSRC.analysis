# Building the data with the desired number of clusters to examine

library(tidyverse)
library(TraMineR)

clusterward <- read_rds(here::here("analysis/data/derived_data/cluster-ward_5min.rds"))
clust_pids <- clusterward$order.lab # pull the pids of the ppl who made it to the clustering

grp_en_tu <- read_rds(here::here("analysis/data/derived_data/grouping-entropy-turbulence.rds")) %>%
  filter(personid %in% clust_pids) # filter so the ppl included in clustering are only ones here

pl.seq <- read_rds(here::here("analysis/data/derived_data/pl_seq.rds"))

##' Rename the columns for the minute of the day (avoids issues with plotting)
colnames(pl.seq) <- seq(1,1440) #paste0("y", seq(1,1440))


column.5min <- seq(from = 1, to = 1440, by = 5)
pl.seq.5min <- pl.seq[, column.5min]
rm(pl.seq)


# plot(clusterward, which.plots = 2, rotate = TRUE)
# plot(clusterward)


# build all cluster sizes that are potentially useable =========================
allclusters <- cutree(clusterward, 1:10)

colnames(allclusters) <- paste0("nclust", colnames(allclusters))

alldata <- cbind(grp_en_tu, allclusters)

cluster_id <- alldata %>% select(personid, starts_with("nclust"))



# try plotting k=2

# join cluster ids to the sequences by pid. make data "long" for ggplot


seq_clust_long <- pl.seq.5min %>%
  as_tibble(rownames = "personid") %>%
  left_join(cluster_id, by = "personid") %>%
  gather(key = "minute", value = "state", -personid, -nclust1, -nclust2, -nclust3, -nclust4,-nclust5,-nclust6,-nclust7,-nclust8, -nclust9, -nclust10) %>%
  mutate(minute = as.numeric(minute)) %>%
           arrange(personid, minute)

## 4. Make x axis labels (times)
# timelabs <- c(seq(3,21,3) %>% str_pad(width = 2,side = "left", pad = "0") %>% paste0(":00"), "00:00", "02:59") # 9 labels

timelabs <- c(seq(3,23,4) %>% str_pad(width = 2,side = "left", pad = "0") %>% paste0(":00"), "02:55")


plot_by_cluster <- function(ggobj, nclust) {
  ggobj +
    geom_area(stat = "bin", binwidth = 1, position = "fill", na.rm = TRUE) +
    scale_x_continuous("Time of Day",
                       breaks = seq(1, 1620, by = 240),
                       minor_breaks = seq(1, 1441, by = 60),
                       labels = timelabs) +
    scale_y_continuous(name = "Frequency") +
    scale_fill_brewer(palette = "Accent", name = "State") +
    facet_wrap(vars({{nclust}}), scale = "free_x", dir="v")
}


seq_clust_gg <- seq_clust_long %>%
  ggplot(aes(x = minute, fill = state))

plot_c2 <- plot_by_cluster(ggobj = seq_clust_gg, nclust = nclust2)
plot_c2


plot_c3 <- plot_by_cluster(seq_clust_gg, nclust3)
plot_c3

plot_c4 <- plot_by_cluster(seq_clust_gg, nclust4)
plot_c4

plot_c5 <- plot_by_cluster(seq_clust_gg, nclust5)
plot_c5


plot_c6 <- plot_by_cluster(seq_clust_gg, nclust6)
plot_c6

plot_c7 <- plot_by_cluster(seq_clust_gg, nclust7)
plot_c7

plot_c8 <- plot_by_cluster(seq_clust_gg, nclust8)
plot_c8

ggsave(filename = here::here("analysis/figures/plot_2clust.png"), plot_c2)
ggsave(filename = here::here("analysis/figures/plot_3clust.png"), plot_c3)
ggsave(filename = here::here("analysis/figures/plot_4clust.png"), plot_c4)
ggsave(filename = here::here("analysis/figures/plot_5clust.png"), plot_c5)
ggsave(filename = here::here("analysis/figures/plot_6clust.png"), plot_c6)
ggsave(filename = here::here("analysis/figures/plot_7clust.png"), plot_c7)
ggsave(filename = here::here("analysis/figures/plot_8clust.png"), plot_c8)


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

# cluster <- cutree(clusterward, 6)%>%
#   factor(labels=paste0("type", 1:6))
#
# alldata <- cbind(grp_en_tu, cluster) %>%
#   mutate(namedcluster = recode_factor(cluster, !!!level_key))

# seqplot <- seqfplot(pl.seq.5min, group = cl6)
#
# seqplot
#
# seqmtplot(pl.seq.5min, group = cl6)


