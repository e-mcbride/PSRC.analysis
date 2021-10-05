# 03 Calculating entropy and turbulence

library(tidyverse); library(TraMineR); library(here)
pl.seq <- read_rds(here("analysis/data/derived_data/pl_seq.rds"))

## Sequence Entropy and Turbulence


print(pl.seq[1:10,], "SPS")
seqient(pl.seq[1:10, ])
seqici(pl.seq[1:10, ])




pl.en_tu_com <- data.frame(seqient(pl.seq, norm = F, with.missing = T),
                           seqST(pl.seq),
                           seqici(pl.seq, with.missing = T)) %>%
  rownames_to_column(var = "pid")


# write_csv(pl.en_tu_com, "data/ent-tur-com_place-seq.csv")
write_rds(pl.en_tu_com, "analysis/data/derived_data/ent-tur-com_place-seq.rds")


summary(pl.en_tu_com$C)
hist(pl.en_tu_com$C)
summarytools::dfSummary(pl.en_tu_com)
