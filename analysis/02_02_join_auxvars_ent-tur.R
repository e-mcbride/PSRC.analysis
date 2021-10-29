# join grouping variables with entropy and turbulence

# library(dplyr)
#
# pl.en_tu_com <- readr::read_rds(here::here("analysis/data/derived_data/ent-tur-com_place-seq.rds"))
#
# sesvars <- readr::read_rds(here::here("analysis/data/derived_data/pid_SES.rds"))
#
# ses_en_tu <-  sesvars %>%
#   left_join(pl.en_tu_com, by = c("personid" = "pid"))
#
# readr::write_rds(ses_en_tu, here::here("analysis/data/derived_data/auxiliary-entropy-turbulence.rds"))
