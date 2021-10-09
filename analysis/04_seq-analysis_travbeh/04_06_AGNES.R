# AGNES

library(tidyverse)
library(here)
library(cluster)

pl_om <- read_rds(here("analysis/data/derived_data/sequences-optimal-matching.rds"))

startagnes <- Sys.time()
clusterward <- cluster::agnes(pl_om, diss = TRUE, method = "ward")
endagnes <- Sys.time()

endagnes - startagnes
# 45 minutes

write_rds(clusterward, here("analysis/data/derived_data/cluster-ward_5min.rds"))
