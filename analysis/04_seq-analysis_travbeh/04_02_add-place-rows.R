# 2. get the places that are in between the trips
library(tidyverse)
library(lubridate)

tr_datetimes <- read_rds('analysis/data/derived_data/tr_datetimes.Rds')

trips <- tr_datetimes %>%
  filter(daynum == 1) %>% #EM: COULD smash together daynum and pid to use all of the days `pdayid`
  mutate(place_type = "T",
         oplace_type =
           case_when(
             origin_purpose == "Went home"  ~ "Home",
             origin_purpose == "Went to school/daycare (e.g., daycare, K-12, college)"  ~ "School",
             origin_purpose == "Went to primary workplace" ~ "Work",
             origin_purpose == "Dropped off/picked up someone (e.g., son at a friend's house, spouse at bus stop)"  ~ "Other",
             origin_purpose == "Went grocery shopping" ~ "Gshop",
             TRUE ~ "Other"
           ),
         dplace_type =
           case_when(
             dest_purpose == "Went home"  ~ "Home",
             dest_purpose == "Went to school/daycare (e.g., daycare, K-12, college)"  ~ "School",
             dest_purpose == "Went to primary workplace" ~ "Work",
             dest_purpose == "Dropped off/picked up someone (e.g., son at a friend's house, spouse at bus stop)"  ~ "Other",
             dest_purpose == "Went grocery shopping" ~ "Gshop",
             TRUE ~ "Other"
           ),

         pltype = oplace_type,
         activity_type = oplace_type
  )

# ========
# source(here::here("EM_02_run-before-02.R")) # EM: run to get the value/label table so the 2019 data can be used
#
# trips <- trips %>%
#   mutate(pltype = oplace_type,
#          activity_type = oplace_type)


pl_nest <- trips %>%
  mutate(tripnum = as.character(tripnum)) %>%
  select(personid, tripnum, dep_ma3am, arr_ma3am, pltype, place_type, oplace_type, dplace_type, activity_type) %>%
  group_by(personid) %>%
  nest()

plnest_mini <- pl_nest %>%
  filter(personid == "1710000501" | personid == "1710000502" | personid == "1710017901")
# slice(1:21)

# Function below makes the new rows with activity place in them and uses a rowbind to combine them with the original trip rows. =====
# based on Elissa's work with CHTS

PlaceRows <- function(x) {

  plno = x[["tripnum"]]
  dep_time = x[["dep_ma3am"]]
  arr_time = x[["arr_ma3am"]]
  destination =x[["dplace_type"]]
  PLANO = NA
  dep_ma3am = NA
  arr_ma3am = NA
  place_type = NA_character_
  pltype = NA_character_
  activity_type = NA_character_

  for(i in 1:length(plno)){
    if(length(dep_time[i-1]!=0)){
      PLANO[i] = paste(plno[i-1], "A", sep = "") #change test
      dep_ma3am[i] = arr_time[i-1]
      arr_ma3am[i] = dep_time[i]
      pltype[i] = pltype[i-1]
      place_type[i] = destination[i-1]
      activity_type[i] = "Activity"

    }
  }

  placedf = tibble(PLANO, dep_ma3am, arr_ma3am, pltype, place_type, destination, activity_type) %>%
    filter(!is.na(PLANO))

  combodf <- placedf %>% bind_rows(x) %>% arrange(starttime)


  return(combodf)
}

place_added_mini <- plnest_mini %>%
  mutate(place = map(data, PlaceRows))
,
         newrows = map(place, ~ .x["arr_ma3am"]))
           # map(place, ~ add_row(.x, dep_ma3am = 1, arr_ma3am, .before = 1))


pl_dat_mini <- place_added_mini %>%
  select(-data) %>%
  unnest(place) %>%
  rename(starttime = dep_ma3am, endtime = arr_ma3am) %>%
  arrange(personid, starttime)


# running function above to add the new set of rows that contain travel
place_added <- pl_nest %>% mutate(place = map(data, PlaceRows))

place_dat <- place_added %>%
  mutate(newrows = map(place, ~ add_row(.x, dep_ma3am = 1, .before = 1)),
         newrows = map(newrows, ~ add_row(.x, .after = nrow(.x))))

%>%
  select(-data) %>%
  unnest(place) %>%
  rename(starttime = dep_ma3am, endtime = arr_ma3am) %>%
  arrange(personid, starttime)


# Add 1st and last places =====
## first row:
  # start time = 1 (not 0 to appease)
  # end time = start time when first(tripnum) == 1
  # place_type = oplace_type of first(tripnum) == 1

# last row:
  # start time = end time of last trip
  # end time = 1440
  # place_type = dplace_type of last trip

function(df) {
  index_r1 <-  which(tripnum == 1)

firstpl <- place_dat %>%
  group_by(personid) %>%
  mutate(which(tripnum == 1))

}

firstpl <- place_dat %>%
  group_by(personid) %>%
  mutate(if_else(tripnum == 1,

  )
    which(tripnum == 1))

# =====
#
# activ_dat <- place_dat %>%
#   mutate(starttime =  if_else(condition = (place_type == "T"), true =  dep_ma3am, false = arr_ma3am),
#          endtime =    if_else(condition = (place_type == "T"), true =  arr_ma3am, false = dep_ma3am))
#
# sorted <- activ_dat[
#   with(activ_dat, order(personid, starttime)),]
# sorted$duration <- sorted$endtime-sorted$starttime
