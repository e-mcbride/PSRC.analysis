# 2. get the places that are in between the trips
library(tidyverse)
library(lubridate)

tr_datetimes <- read_rds('analysis/data/derived_data/tr_datetimes.Rds')

trips <- tr_datetimes %>%
  filter(daynum == 1) %>% #EM: COULD smash together daynum and pid to use all of the days `pdayid`
  mutate(place_type = "Travel",
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

         pltype = "T",
         activity_type = "Travel"
  )

# ========
# source(here::here("EM_02_run-before-02.R")) # EM: run to get the value/label table so the 2019 data can be used
#
# trips <- trips %>%
#   mutate(pltype = oplace_type,
#          activity_type = oplace_type)


pl_nest <- trips %>%
  mutate(tripnum = as.character(tripnum)) %>%
  select(hhid, personid, trip_id, tripnum, dep_ma3am, arr_ma3am, pltype, place_type, oplace_type, dplace_type, activity_type) %>%
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

  combodf <- placedf %>% bind_rows(x) %>% arrange(dep_ma3am)


  return(combodf)
}

place_added_mini <- plnest_mini %>%
  mutate(place = map(data, PlaceRows),
         # newrows = map(place, ~ .x[1, "dep_ma3am"]))
         newrows = map(place, ~ add_row(.x,
                                        dep_ma3am = 1,
                                        arr_ma3am = .x[[1, "dep_ma3am"]],
                                        place_type = .x[[1, "oplace_type"]],
                                        .before = 1)),
         newrows = map(newrows, ~ add_row(.x,
                                        dep_ma3am = .x[[nrow(.x), "arr_ma3am"]],
                                        arr_ma3am = 1440,
                                        place_type = .x[[nrow(.x), "dplace_type"]],
                                        .after = nrow(.x)))
         )


pl_dat_mini <- place_added_mini %>%
  select(-data, -place) %>%
  unnest(newrows) %>%
  arrange(personid, dep_ma3am)


# running function above to add the new set of rows that contain travel
place_added <- pl_nest %>% mutate(place = map(data, PlaceRows))

# Add 1st and last places =====
## first row:
# start time = 1 (not 0 to appease)
# end time = start time when first(tripnum) == 1
# place_type = oplace_type of first(tripnum) == 1

# last row:
# start time = end time of last trip
# end time = 1440
# place_type = dplace_type of last trip

place_dat <- place_added %>%

  mutate(newrows = map(place, ~ add_row(.x,
                                        dep_ma3am = 1,
                                        arr_ma3am = .x[[1, "dep_ma3am"]],
                                        place_type = .x[[1, "oplace_type"]],
                                        .before = 1)),
         newrows = map(newrows, ~ add_row(.x,
                                          dep_ma3am = .x[[nrow(.x), "arr_ma3am"]],
                                          arr_ma3am = 1440,
                                          place_type = .x[[nrow(.x), "dplace_type"]],
                                          .after = nrow(.x))
         )) %>%
  select(-data, -place) %>%
  unnest(newrows) %>%
  arrange(personid, dep_ma3am)

# trips %>%
#   filter(arr_ma3am < dep_ma3am) %>%
#   select(dep_ma3am, arr_ma3am, depart_time_hhmm, arrival_time_hhmm, dep_datetime, arr_datetime, start_datetime, end_datetime, date_extracted) %>%
#   View()

place_dat %>%
  filter(arr_ma3am < dep_ma3am) %>%
  arrange(personid, dep_ma3am) %>%
  View("end < start")

pids_endvstart <- place_dat %>%
  filter(arr_ma3am < dep_ma3am) %>%
  pull(personid) %>%
  unique()



place_dat %>%
  filter(personid %in% pids_endvstart) %>%
  # arrange(personid, dep_ma3am) %>%
  View("end < start allrecs")

trips %>%
  filter(personid %in% pids_endvstart) %>%
  View("wholrecs")

trips %>%
  filter(dep_ma3am == arr_ma3am) %>%
  select(dep_ma3am, arr_ma3am, dep_datetime, arr_datetime, depart_time_hhmm, arrival_time_hhmm) %>%
  View()

# make an if/else for if departtime == lead arrtime
# make an if/else for if departtime == lead arrtime. look at dep_ma3am == arr_ma3am ppl


write_rds(place_dat, here::here("analysis/data/derived_data/place_dat.rds"))



