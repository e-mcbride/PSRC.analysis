# join grouping variables with entropy and turbulence

library(dplyr)

pl.en_tu_com <- readr::read_rds(here::here("analysis/data/derived_data/ent-tur-com_place-seq.rds"))

grpvars <- readr::read_rds(here::here("analysis/data/derived_data/grouping-variables.rds"))


grp_en_tu <- pl.en_tu_com %>%
  left_join(grpvars, by = c("pid" = "personid"))
