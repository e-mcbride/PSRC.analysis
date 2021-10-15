# Models splitting employed and not employed

library(tidyverse)
library(here)
library(janitor)

prdat <- read_rds(here("analysis/data/derived_data/prdat.rds"))

# worker pids and non-worker pids ====================

worker_pids <- prdat %>%
  filter(worker == "1+ job(s) (including part-time)") %>%
  pull(person_id)


nonworker_pids <- prdat %>%
  filter(worker == "No jobs") %>%
  pull(person_id)


# TEST do the pid lists above include all possible PIDs?

all_pids <- prdat %>%
  pull(person_id) %>%
  sort()

w_nw_pids <- c(worker_pids, nonworker_pids) %>%
  sort()

all(all_pids == w_nw_pids)

# YES

rm(all_pids, w_nw_pids)


# Split travel data ==========================

wrk_trdat


