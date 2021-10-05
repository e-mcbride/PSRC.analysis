# optimal matching (OM)

library(tidyverse); library(here);library(TraMineR)
pl.seq <- read_rds(here("analysis/data/derived_data/pl_seq.rds"))

costs <- TraMineR::seqsubm(pl.seq, method = "TRATE", with.missing = TRUE) # OLD NOTE: took about 60 seconds
round(costs, 2)

pl_om <- TraMineR::seqdist(pl.seq, method = "OM", indel = 3, sm = costs, with.missing = TRUE)
# OLD NOTE: with 5000 households (12704 people), it took 1.135 days (about 27 hrs)
# NEW NOTE: started: 10/4/2021 22:10
write_rds(pl_om, here("analysis/data/derived_data/sequences-optimal-matching.rds"))
