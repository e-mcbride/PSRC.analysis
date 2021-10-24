# join grouping variables with entropy and turbulence

library(dplyr)

pl.en_tu_com <- readr::read_rds(here::here("analysis/data/derived_data/ent-tur-com_place-seq.rds"))

auxvars <- readr::read_rds(here::here("analysis/data/derived_data/auxiliary-variables.rds"))

aux_en_tu <-  auxvars %>%
  left_join(pl.en_tu_com, by = c("personid" = "pid"))

readr::write_rds(aux_en_tu, here::here("analysis/data/derived_data/auxiliary-entropy-turbulence.rds"))
