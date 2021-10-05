# optimal matching (OM)

library(tidyverse); library(here);library(TraMineR)
read_rds(here("analysis/data/derived_data/pl_seq.rds"))

costs <- TraMineR::seqsubm(seq_samp05, method = "TRATE")#, with.missing = TRUE) # took about 60 seconds
round(couts, 2)

pl_om <- TraMineR::seqdist(seq_samp05, method = "OM", indel = 3, sm = couts)#, with.missing = TRUE)
# with 5000 households (12704 people), it took 1.135 days (about 27 hrs)

write_rds(pl_om, here("data", "sequences-optimal-matching.rds"))
