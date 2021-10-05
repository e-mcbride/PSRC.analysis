# optimal matching (OM)

library(tidyverse); library(here);library(TraMineR)
pl.seq <- read_rds(here("analysis/data/derived_data/pl_seq.rds"))

column.5min <- seq(from = 1, to = 1440, by = 5)
pl.seq.5min <- pl.seq[, column.5min]
rm(pl.seq)

costs <- TraMineR::seqsubm(pl.seq.5min, method = "TRATE", with.missing = TRUE) # OLD NOTE: took about 60 seconds
round(costs, 2)

pl_om <- TraMineR::seqdist(pl.seq.5min, method = "OM", indel = 3, sm = costs, with.missing = TRUE)
# OLD NOTE: with 5000 households (12704 people), it took 1.135 days (about 27 hrs)
# NEW NOTE: started: 10/4/2021 22:10
write_rds(pl_om, here("analysis/data/derived_data/sequences-optimal-matching.rds"))
