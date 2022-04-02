# Deciding the number of clusters to use, creating figures for all those

library(tidyverse)


clusterward <- read_rds(here::here("analysis/data/derived_data/cluster-ward_5min.rds"))
clust_pids <- clusterward$order.lab # pull the pids of the ppl who made it to the clustering

pid_df <- PSRCData::prdat %>%
  filter(personid %in% clust_pids) %>%
  select(personid)

# identical(pid_df$personid, clusterward$order.lab)
#
# who <- pid_df$personid == clusterward$order.lab
#
# length(pid_df$personid)
# length(clusterward$order.lab)

# labelrows <- matrix(clusterward$order.lab, ncol = 1)

# ses_en_tu <- read_rds(here::here("analysis/data/derived_data/auxiliary-entropy-turbulence.rds")) %>%
#   filter(personid %in% clust_pids) # filter so the ppl included in clustering are only ones here

pl.seq <- read_rds(here::here("analysis/data/derived_data/pl_seq.rds"))

##' Rename the columns for the minute of the day (avoids issues with plotting)
colnames(pl.seq) <- seq(1,1440) #paste0("y", seq(1,1440))


column.5min <- seq(from = 1, to = 1440, by = 5)
pl.seq.5min <- pl.seq[, column.5min]
rm(pl.seq)


# library(cluster)
# plot(clusterward, which.plots = 2, rotate = TRUE)
# plot(clusterward)


# build all cluster sizes that are potentially useable =========================
allclusters <- cutree(clusterward, 1:9)


colnames(allclusters) <- paste0("nclust", colnames(allclusters))

alldata <- cbind(pid_df, allclusters)

#
# alldata <- cbind(labelrows, allclusters) %>% as_tibble() %>% #IMPORTANT TO COME BACK TO LATER
#   rename(personid = V1)

cluster_id <- alldata %>% select(personid, starts_with("nclust"))


# sq6_count <- cluster_id %>%
#   group_by(nclust6) %>%
#   summarise(n = n()) %>%
#   mutate(paste0(nclust6, ": ", n))
#
# cluster_id %>%
#   group_by(nclust7) %>%
#   summarise(n())
#
# cluster_id %>%
#   group_by(nclust8) %>%
#   summarise(n())



write_rds(pl.seq.5min, here::here("analysis/data/derived_data/pl_seq_5min.rds"))
write_rds(cluster_id, here::here("analysis/data/derived_data/pid_all-clusters.rds"))



sq_counts <- cluster_id %>%
  pivot_longer(!personid) %>%
  group_by(name, value) %>%
  summarise(n = n()) %>%
  mutate(clust_label = as.factor(paste0("Cluster ", value, " (", n, " observations)"))) %>%
  select(!n)

cluster_named <- cluster_id %>%
  pivot_longer(!personid) %>%
  left_join(sq_counts, by = c("name", "value")) %>%
  select(!value) %>%
  pivot_wider(names_from = name, values_from = clust_label)

# plotting, deciding n clusters ############################################
# join cluster ids to the sequences by pid. make data "long" for ggplot

seq_clust_long <- pl.seq.5min %>%
  as_tibble(rownames = "personid") %>%
  left_join(cluster_named, by = "personid") %>%
  gather(key = "minute", value = "state", -personid, -nclust1, -nclust2, -nclust3, -nclust4,-nclust5,-nclust6,-nclust7,-nclust8, -nclust9) %>%
  mutate(
    across(starts_with("nclust"), droplevels),
    across(starts_with("nclust"), ~ factor(.x, levels = c((unique(.x)))))
  ) %>%
  mutate(minute = as.numeric(minute),
         state = factor(state,
                        levels = c("Home", "School", "Work", "Gshop", "Other", "Travel"),
                        labels = c("Home", "School", "Work", "Grocery", "Other", "Traveling") )) %>%
           arrange(personid, minute)



## 4. Make x axis labels (times) ----------------------------------
# timelabs <- c(seq(3,21,3) %>% str_pad(width = 2,side = "left", pad = "0") %>% paste0(":00"), "00:00", "02:59") # 9 labels

timelabs <- c(seq(3,23,4) %>% str_pad(width = 2,side = "left", pad = "0") %>% paste0(":00"), "02:55")


## ------------------------------------
# sq_counts <- cluster_id %>%
#   pivot_longer(!personid) %>%
#   group_by(name, value) %>%
#   summarise(n = n()) %>%
#   mutate(clust_label = as.factor(paste0("Cluster ", value, " (", n, " observations)")))
#
# sq_counts(filter())

plot_by_cluster <- function(ggobj, nclust) {
  ggobj +
    geom_area(stat = "bin", binwidth = 1, position = "fill", na.rm = TRUE) +
    scale_x_continuous("Time of Day",
                       breaks = seq(1, 1620, by = 240),
                       minor_breaks = seq(1, 1441, by = 60),
                       labels = timelabs) +
    scale_y_continuous(name = "Frequency") +
    scale_fill_brewer(palette = "Accent", name = "State") +
    facet_wrap(vars({{nclust}}), scale = "free_x")
}


seq_clust_gg <- seq_clust_long %>%
  ggplot(aes(x = minute, fill = state))

# sq2_counts <- sq_counts %>%
#   filter(name == "nclust2")

plot_c2 <- plot_by_cluster(ggobj = seq_clust_gg, nclust = nclust2)
plot_c2


plot_c3 <- plot_by_cluster(seq_clust_gg, nclust3)
plot_c3

plot_c4 <- plot_by_cluster(seq_clust_gg, nclust4)
plot_c4

plot_c5 <- plot_by_cluster(seq_clust_gg, nclust5)
plot_c5

# bb <- sq_counts %>%
#   filter(name == "nclust6") %>%
#   mutate(clust_label =
#            droplevels(clust_label) %>%
#            relevel(ref = "Cluster 1 (3115 observations)")
#   )
#
# levels(bb$clust_label)

plot_c6 <- plot_by_cluster(seq_clust_gg, nclust6)
plot_c6

plot_c7 <- plot_by_cluster(seq_clust_gg, nclust7)
plot_c7

plot_c8 <- plot_by_cluster(seq_clust_gg, nclust8)
plot_c8
#
# ggsave(filename = here::here("analysis/figures/plot_2clust.png"), plot_c2)
# ggsave(filename = here::here("analysis/figures/plot_3clust.png"), plot_c3)
# ggsave(filename = here::here("analysis/figures/plot_4clust.png"), plot_c4)
# ggsave(filename = here::here("analysis/figures/plot_5clust.png"), plot_c5)
# ggsave(filename = here::here("analysis/figures/plot_6clust.png"), plot_c6)
ggsave(filename = here::here("analysis/figures/plot_7clust.png"), plot_c7)
ggsave(filename = here::here("analysis/figures/plot_8clust.png"), plot_c8)

# =================================== DECISION MADE: 6 clusters ===========================================


# cl6_labs <- sq_counts %>%
#   filter(name == "nclust6") %>%
#   mutate(clust_label =
#            droplevels(clust_label) %>%
#            relevel(ref = "Cluster 1 (3115 observations)")
#   ) %>%
#   select(name, value, cl6_label = clust_label) %>%
#   pivot_wider(names_from = name, values_from = value)
#
# levels(cl6_labs$cl6_label)
seq_clust_long$nclust6 %>% levels()

sixclust_final <- seq_clust_long %>%
  select(!c(nclust1, nclust2, nclust3, nclust4, nclust5, nclust7, nclust8, nclust9)) %>%
  mutate(nclust6 = factor(nclust6, labels = c("Home Day (3115 observations)",
                                               "Typical Work Day (4457 observations)",
                                               "School Day (964 observations)",
                                               "Errands Day (1045 observations)",
                                               "Atypical Work Day (260 observations)",
                                               "Travel Day (418 observations)")))

# c("Home Day (3115 observations)",
#   "Typical Work Day (4457 observations)",
#   "School Day (964 observations)",
#   "Errands Day (1045 observations)",
#   "Atypical Work Day (260 observations)",
#   "Travel Day (418 observations)")

# %>%
#   mutate(nclust6 =
#            droplevels(nclust6) %>%
#            relevel(ref = "Cluster 1 (3115 observations)"))



sixclust_gg <- sixclust_final %>%
  ggplot(aes(x = minute, fill = state))


final_plotc6 <- sixclust_gg +
  geom_area(stat = "bin", binwidth = 1, position = "fill", na.rm = TRUE) +
  scale_x_continuous("Time of Day",
                     breaks = seq(1, 1620, by = 240),
                     minor_breaks = seq(1, 1441, by = 60),
                     labels = timelabs) +
  scale_y_continuous(name = "Frequency") +
  scale_fill_brewer(palette = "Accent", name = "State") +
  facet_wrap(vars(nclust6), scale = "free_x", ncol = 2) +
  theme_bw() +
  theme(
    text = element_text(size=8),
    axis.text = element_text(size=6.5),
    legend.text = element_text(size=7),
    strip.text = element_text(size=8),
    # panel.border = element_rect(size = 0.2),
    axis.ticks = element_line(size = 0.4)
  )

final_plotc6

ggsave(filename = here::here("analysis/figures/final_plot_6clust.png"), plot = final_plotc6,
       units = "in",
       width = 6.5
       )


# seven cluster ========================================================
sevclust_final <- seq_clust_long %>%
  select(!c(nclust1, nclust2, nclust3, nclust4, nclust5, nclust6, nclust8, nclust9))

sevclust_gg <- sevclust_final %>%
  ggplot(aes(x = minute, fill = state))

final_plotc7 <- sevclust_gg +
  geom_area(stat = "bin", binwidth = 1, position = "fill", na.rm = TRUE) +
  scale_x_continuous("Time of Day",
                     breaks = seq(1, 1620, by = 240),
                     minor_breaks = seq(1, 1441, by = 60),
                     labels = timelabs) +
  scale_y_continuous(name = "Frequency") +
  scale_fill_brewer(palette = "Accent", name = "State") +
  facet_wrap(vars(nclust7), scale = "free_x", ncol = 2) +
  theme_bw() +
  theme(
    text = element_text(size=8),
    axis.text = element_text(size=6.5),
    legend.text = element_text(size=7),
    strip.text = element_text(size=8),
    # panel.border = element_rect(size = 0.2),
    axis.ticks = element_line(size = 0.4)
  )

final_plotc7

ggsave(filename = here::here("analysis/figures/final_plot_7clust.png"), plot = final_plotc7,
       units = "in",
       width = 6.5
)

# eight cluster ====================================================
eigclust_final <- seq_clust_long %>%
  select(!c(nclust1, nclust2, nclust3, nclust4, nclust5, nclust6, nclust7, nclust9))

eigclust_gg <- eigclust_final %>%
  ggplot(aes(x = minute, fill = state))


final_plotc8 <- eigclust_gg +
  geom_area(stat = "bin", binwidth = 1, position = "fill", na.rm = TRUE) +
  scale_x_continuous("Time of Day",
                     breaks = seq(1, 1620, by = 240),
                     minor_breaks = seq(1, 1441, by = 60),
                     labels = timelabs) +
  scale_y_continuous(name = "Frequency") +
  scale_fill_brewer(palette = "Accent", name = "State") +
  facet_wrap(vars(nclust8), scale = "free_x",ncol = 2) +
  theme_bw() +
  theme(
    text = element_text(size=8),
    axis.text = element_text(size=6.5),
    legend.text = element_text(size=7),
    strip.text = element_text(size=8),
    # panel.border = element_rect(size = 0.2),
    axis.ticks = element_line(size = 0.4)
  )

final_plotc8

ggsave(filename = here::here("analysis/figures/final_plot_8clust.png"), plot = final_plotc8,
       units = "in",
       width = 6.5
)
