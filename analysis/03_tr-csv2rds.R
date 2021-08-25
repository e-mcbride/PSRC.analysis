# take raw trip (tr) data, build factors, write rds
library(tidyverse)
library(here)

raw_tr <- read_csv(here("analysis/data/raw_data/2017-2019-pr2-5-Trip.csv"),
                   guess_max = 63000)


trdat <- raw_tr


write_rds(trdat, here("analysis/data/derived_data/trdat.rds"))
