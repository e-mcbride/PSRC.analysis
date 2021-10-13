library(dplyr)
library(lubridate)

library(PSRC.data)
trdat <- PSRC.data::trdat %>%
  mutate(depart_time_hhmm = hms::as_hms(depart_time_hhmm),
         arrival_time_hhmm = hms::as_hms(arrival_time_hhmm))


# class(trdat_raw$depart_time_hhmm)
# class(trdat_raw$arrival_time_hhmm)
#
# # class(trdat_raw$depart_time_hhmm) == "difftime"
#
# trdat <- trdat_raw %>%
#   transmute(depart_time_hhmm, depclassdifftime = hms::as_hms(depart_time_hhmm))

# FIRST: remove the hhs that have messed up time/date combos that aren't fixable ========================
rollovers <- trdat %>%
  filter(depart_time_hhmm > arrival_time_hhmm) %>%
  select(personid, traveldate, rollovers = tripnum) %>%
  distinct()

badtrips_labeled <- trdat %>%
  select(personid, daynum, tripnum, traveldate, depart_time_hhmm, arrival_time_hhmm, dest_purpose,
         # mode_1, mode_2, mode_3, mode_4,
         trip_id) %>%
  # get: last trip number of the day
  group_by(personid, traveldate) %>%
  mutate(lasttripnum = max(tripnum)) %>%
  ungroup() %>%
  #join rollovers numbers from step 1
  left_join(rollovers, by = c("personid", "traveldate")) %>%

  # T/F rollover day
  mutate(tr_after_rover = lasttripnum > rollovers)
# arrival_time_hhmm > lead(depart_time_hhmm))

hid_some_badtrips <- trdat %>%
  # get: last trip number of the day
  group_by(personid, traveldate) %>%
  mutate(lasttripnum = max(tripnum)) %>%
  ungroup() %>%
  #join rollovers numbers from step 1
  left_join(rollovers, by = c("personid", "traveldate")) %>%

  # T/F rollover day
  mutate(tr_after_rover = lasttripnum > rollovers) %>%
  filter(tr_after_rover) %>%
  pull(hhid) %>%
  unique()

hhids_unfixable <- trdat %>%
  mutate(arr_sec = as.period(arrival_time_hhmm) %>% period_to_seconds(),
         gr8r_3am = arr_sec > 10800) %>%
  filter(depart_time_hhmm > arrival_time_hhmm) %>%
  filter(arr_sec >= 46800) %>%
  pull(hhid)


# the following should be removed because they don't have any possibility of being salvaged.
## For the messed up ppl in these hhs, no idea when the start/end time is for their first trips of the day
hids_dep_arrNA <- trdat %>%
  filter(is.na(depart_time_hhmm) & is.na(arrival_time_hhmm)) %>%
  pull(hhid) %>%
  unique()

# manually identified to be problematic
hid_manual <- c("17101379", "17148555", "17151187", # times after noon were incorrectly coded (e.g. 1:00 instead of 13:00)
                "17108877", "17139169", "17141482" # all kinds of messed up stuff in these records
                )

hid_badtrips <- c(hid_some_badtrips, hhids_unfixable, hids_dep_arrNA, hid_manual) %>% unique()



# WHEN daynum is the same, but traveldate is +1, THEN the trip was made as part of the day before
## BUT this probably would only apply if a trip STARTS after midnight
### HOW DO I FIND trips that start on normal day and then END after midnight?
#### IF depart_time > arrival_time, full datetime is +1 to the date

## take the depart_time, add the date to it using lubridate. This should always be correct ===============
#### IF depart_time > arrival_time, full datetime is +1 to the arrival_time date

tr <- trdat %>%
  # remove incomplete records
  filter(svy_complete %in% "Complete") %>%
  # remove the bad trips identified in step before this
  filter(!(hhid %in% hid_badtrips))

# %>%
#   # shrink dataset to necessary vars (for easier visual assessment. commented out now that assessment is done)
#   select(hhid, personid, daynum, tripnum, traveldate, depart_time_hhmm, arrival_time_hhmm, dest_purpose,
#          # mode_1, mode_2, mode_3, mode_4,
#          trip_id)

# clear unnecessary vars
# rm(rollovers)


# This set of pids where arrival was after 3am on travel day. I will set arrival to 3am. =======
time0259 <- hms::as_hms(10740)
time0300 <- hms::as_hms(10800)
time0301 <- hms::as_hms(10860)

## it won't mess with other trips that day, and these trips actually truly ended the next day
fixed_arrtimes <- tr %>%
  mutate(arr_sec = as.period(arrival_time_hhmm) %>% period_to_seconds(),
         gr8r_3am = arr_sec > 10800) %>%
  filter(depart_time_hhmm > arrival_time_hhmm) %>%
  mutate(arrt2 =
           if_else(gr8r_3am, time0300, arrival_time_hhmm)
  ) %>%
  select(trip_id, arrt2)



newtimes <- tr %>%
  # join and fix the arrival time hhmm var so that times are set to 3am
  left_join(fixed_arrtimes, by = "trip_id") %>%
  mutate(arrival_time_hhmm = if_else(is.na(arrt2), arrival_time_hhmm, arrt2)) %>%
  group_by(personid, daynum) %>% mutate(maxdaytrnum = max(tripnum)) %>% ungroup() %>%

  # convert the arr_time and dep_time vars to hms, convert traveldate to lubridate (?) format
  mutate(arrival_time_hhmm = if_else(is.na(arrival_time_hhmm) & maxdaytrnum == tripnum, time0300, arrival_time_hhmm),
         depart_time_hhmm = if_else((tripnum == 1 & is.na(depart_time_hhmm)), time0300, depart_time_hhmm), # if tripnum is 1 and depart time is NA, set depart time to 03:00
         startdate = traveldate %>% mdy_hms() %>% date(),

         enddate = if_else(depart_time_hhmm > arrival_time_hhmm, (startdate + 1), startdate),


         dep_datetime_chr = paste(startdate, depart_time_hhmm),
         dep_datetime = ymd_hms(dep_datetime_chr),
         arr_datetime_chr = paste(enddate, arrival_time_hhmm),
         arr_datetime = ymd_hms(arr_datetime_chr)
  ) %>%
  # do something like "start time" which will always be traveldate of first trip on each day (min(tripno)) paste 3am, so we can subtract
  group_by(personid, daynum) %>%
  mutate(
    # daytrip1 = min(tripnum),
         diarystartdate = first(startdate),
         diaryenddate = diarystartdate + days(1)) %>%
  ungroup() %>%
  # make minutes after 3am vars `ma3am`
  mutate(start_datetime = paste(diarystartdate, time0300) %>% ymd_hms(),

         arr_datetime = if_else((dep_datetime - start_datetime) %>% as.period() %>% as.numeric("minutes") < 0 ,  #has to come first cuz problem fixed in next line
                                arr_datetime + days(1),
                                arr_datetime),

         dep_datetime = if_else((dep_datetime - start_datetime) %>% as.period() %>% as.numeric("minutes") < 0 ,
                                dep_datetime + days(1),
                                dep_datetime),

         end_datetime = paste(diaryenddate, time0300) %>% ymd_hms(),

         dep_ma3am = (dep_datetime - start_datetime) %>% as.period() %>% as.numeric("minutes"),
         arr_ma3am = (arr_datetime - start_datetime) %>% as.period() %>% as.numeric("minutes"),
         dep_ma3am = if_else(dep_ma3am == 0, 1, dep_ma3am),

         arr_ma3am = if_else(arr_ma3am > 1440, 1440, arr_ma3am),
         arr_ma3am = if_else(arr_ma3am < 1, 1, arr_ma3am)
         ) %>%

  # select(personid, trip_id, daynum, diarystartdate, tripnum, startdate, enddate, dep_datetime, arr_datetime, start_datetime)
  select(trip_id, dep_datetime, arr_datetime, dep_ma3am, arr_ma3am, start_datetime, end_datetime)

tr_datetimes <- tr %>% left_join(newtimes, by = "trip_id")


tr_datetimes %>%
  filter(arr_ma3am < dep_ma3am) %>%
  select(starts_with("dep"), starts_with("arr")) %>%
  View("ends < starts")
# # checking arrival and departure time fixes
# tr_datetimes %>%
#   filter(daynum == 1) %>%
#   select(personid, daynum, tripnum, dep_ma3am, arr_ma3am, dep_datetime, arr_datetime, start_datetime, traveldate) %>%
#   group_by(personid) %>%
#   mutate(mindep = min(dep_ma3am),
#          maxdep = max(dep_ma3am),
#          minarr = min(arr_ma3am),
#          maxarr = max(arr_ma3am),
# ) %>%
#   ungroup() %>%
#   View()


# # check the end date variable
# newtimes %>% filter(startdate != enddate) %>% View() # looks good!

clean_pids <- tr_datetimes %>% pull(personid) %>% unique()
readr::write_rds(clean_pids, here::here("analysis/data/derived_data/clean_pids.rds"))

clean_hids <- tr_datetimes %>% pull(hhid) %>% unique()
readr::write_rds(clean_hids, here::here("analysis/data/derived_data/clean_hids.rds"))

readr::write_rds(tr_datetimes, "analysis/data/derived_data/tr_datetimes.Rds")
