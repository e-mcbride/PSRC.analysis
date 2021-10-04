# 2. get the places that are in between the trips
library(tidyverse)

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

# source(here::here("EM_02_run-before-02.R")) # EM: run to get the value/label table so the 2019 data can be used
#
# trips <- trips %>%
#   mutate(pltype = oplace_type,
#          activity_type = oplace_type)


pl_nest <- trips %>%
  mutate(tripnum = as.character(tripnum)) %>%
  select(personid, tripnum, dep_ma3am, arr_ma3am, pltype, place_type, dplace_type, activity_type) %>%
  group_by(personid) %>%
  nest()



# Function below makes the new rows with activity place in them and uses a rowbind to combine them with the original trip rows. =====
# based on Elissa's work with CHTS

ActivRows <- function(x) {
  plno = x[["tripnum"]]
  dep_time = x[["dep_ma3am"]]
  arr_time = x[["arr_ma3am"]]
  destination =x[["dplace_type"]]
  PLANO = NA
  dep_ma3am = NA
  arr_ma3am = NA
  place_type = NA
  pltype = NA
  activity_type = NA

  for(i in 1:length(plno)){
    if(length(dep_time[i-1]!=0)){
      PLANO[i] = paste(plno[i-1], "A", sep = "") #change test
      dep_ma3am[i] = dep_time[i]
      arr_ma3am[i] = arr_time[i-1]
      pltype[i] = pltype[i-1]
      place_type[i] = destination[i-1]
      activity_type[i] = "Activity"

    }
  }

  activdf = tibble(PLANO, dep_ma3am, arr_ma3am, pltype, place_type, destination, activity_type) %>%
    filter(!is.na(PLANO))

  combodf <- activdf %>% bind_rows(x)

  return(combodf)
}

testsubj <- pl_nest %>%
  filter(personid == "1710000501" | personid == "1710000502")
  # filter(personid == "1710017901")

testfxn <- testsubj %>%
  mutate(place_rows = map(data, ActivRows))

testfxunnest <- testfxn %>%
  select(-data) %>%
  unnest(cols = place_rows)

# oneperson <- pl_nest$data[[1]]
# oneperson[["tripnum"]] %>% length()
# length(oneperson$dep_ma3am[0])

badperson <- pl_nest %>% filter(personid == "1710017901") %>% unnest()


# WHAT IF: mutated a new column in nested data frame
## make it the same as the first column (copied dataset) that way we can manipulate it and maintain the original
## Then: IF length() > 1, THEN dplyr::add_row() with the values defined above. ELSE: keep the

  # arrange(personid, tripnum, PLANO)

# running function above to add the new set of rows that contain travel
activ_added <- pl_nest %>% mutate(activ = map(data, ActivRows))
