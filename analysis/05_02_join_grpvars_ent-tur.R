# join grouping variables with entropy and turbulence

library(dplyr)

pl.en_tu_com <- readr::read_rds(here::here("analysis/data/derived_data/ent-tur-com_place-seq.rds"))

grpvars <- readr::read_rds(here::here("analysis/data/derived_data/grouping-variables.rds"))

grp_en_tu <-  grpvars %>%
  left_join(pl.en_tu_com, by = c("personid" = "pid"))

readr::write_rds(grp_en_tu, here::here("analysis/data/derived_data/grouping-entropy-turbulence.rds"))
