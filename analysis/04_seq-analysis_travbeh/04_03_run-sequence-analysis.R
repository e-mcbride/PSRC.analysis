# Run sequence analysis
library(tidyverse)
library(TraMineR)

place_dat <- read_rds(here::here("analysis/data/derived_data/place_dat.rds"))

# =====

pl.labels <- seqstatl(place_dat$place_type)

# function to get time of day labels for future graphs (added to `cnames` argument of `seqdef`)
min2TOD = function(dayminutes) {
  TOD = c()
  for (i in 1:length(dayminutes)) {
    add3 = (dayminutes[i]/60) + 3

    if(add3 < 24){
      tod_hr = (floor(add3)) %>% str_pad(width = 2, side = "left", pad = "0")
    } else {
      tod_hr = (floor(add3) - 24) %>% str_pad(width = 2, side = "left", pad = "0")
    }

    tod_min = ((add3 - floor(add3))*60) %>% round() %>%
      str_pad(width = 2, side = "left", pad = "0")

    TOD[i] = tod_hr %>% str_c(tod_min, sep = ":")
  }
  return(TOD)
}

# from Elissa
time <- min2TOD(seq(0,1439))

# Running sequence analysis:
pl.seq <- place_dat %>% data.frame() %>%
  seqdef(var = c("personid", "dep_ma3am", "arr_ma3am", "place_type"),
         informat = "SPELL", labels = pl.labels, process = FALSE, cnames = time, xtstep= 180)

print(pl.seq[1000:1015, ], format = "SPS")

print(pl.seq[1:15, ], format = "SPS")

write_rds(pl.seq, here::here("analysis/data/derived_data/pl_seq.rds"))
