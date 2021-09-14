# take raw trip (tr) data, build factors, write rds
library(tidyverse)
library(here)

raw_tr <- read_csv(here("analysis/data/raw_data/2017-2019-pr2-5-Trip.csv"),
                   guess_max = 63000)


trdat <- raw_tr %>%
  # get the columns mode_1 thru mode_4
  mutate(
    across(mode_1:mode_4,
           ~ factor(.x,
             c("Walk (or jog/wheelchair)",
             "Bicycle or e-bike (rSurvey only)",
             "Household vehicle 1",
             "Household vehicle 2",
             "Household vehicle 3",
             "Household vehicle 4",
             "Household vehicle 5",
             "Household vehicle 6",
             "Household vehicle 7",
             "Household vehicle 8",
             "Household vehicle 9",
             "Household vehicle 10",
             "Other vehicle in household",
             "Rental car",
             "Carshare service (e.g., Turo, Zipcar, ReachNow)",
             "Vanpool",
             "Other non-household vehicle",
             "Bus (public transit)",
             "School bus",
             "Private bus or shuttle",
             "Paratransit",
             "Other bus (rMove only)",
             "Airplane or helicopter",
             "Ferry or water taxi",
             "Car from work",
             "Friend/colleague's car",
             "Taxi (e.g., Yellow Cab)",
             "Other hired service (Uber, Lyft, or other smartphone-app car service)",
             "Commuter rail (Sounder, Amtrak)",
             "Other rail (e.g., streetcar)",
             "Other motorcycle/moped/scooter",
             "Urban Rail (e.g., Link light rail, monorail)",
             "Other motorcycle/moped",
             "Scooter or e-scooter (e.g., Lime, Bird, Razor)",
             "Bicycle owned by my household (rMove only)",
             "Borrowed bicycle (e.g., from a friend) (rMove only)",
             "Bike-share bicycle (rMove only)",
             "Other rented bicycle (rMove only)",
             "Other mode (e.g., skateboard, kayak, motorhome, etc.)")
           )
    ),
    mode_egr = factor(mode_egr,
                      c("Walked or jogged",
                        "Rode a bike",
                        "Drove and parked a car (e.g., a vehicle in my household)",
                        "Drove and parked a carshare vehicle (e.g., ZipCar, Car2Go)",
                        "Got dropped off",
                        "Took a taxi (e.g., Yellow Cab)",
                        "Took ride-share/other hired car service (e.g., Lyft, Uber)",
                        "Other")),
    mode_acc = factor(mode_acc,
                      c("Walked or jogged",
                        "Rode a bike",
                        "Drove and parked a car (e.g., a vehicle in my household)",
                        "Drove and parked a carshare vehicle (e.g., ZipCar, Car2Go)",
                        "Got dropped off",
                        "Took a taxi (e.g., Yellow Cab)",
                        "Took ride-share/other hired car service (e.g., Lyft, Uber)",
                        "Other")),

    driver = factor(driver,
                    c("Driver",
                      "Passenger",
                      "Both (switched drivers during trip)")),

    # There are NAs in the modes that shouldn't be. this fixes it:
    main_mode =
      if_else(
        str_detect(mode_1, regex("Bicycle", ignore_case = TRUE)),
        "Bike",
        main_mode),
    main_mode =
      if_else(
        !is.na(mode_1) & is.na(main_mode),
        "Other",
        main_mode
      ),
    mode_simple =
             if_else(is.na(mode_simple) & !is.na(main_mode),
                     main_mode,
                     mode_simple
             ),

    # Now, must make a variable that includes driving alone, passenger, and drive others separate
    # Make a new variable called `mode_full_EM` - indicates it's a variable made by me

    mode_full_EM =
             case_when(
               main_mode == "HOV" & driver == "Driver"  ~ "DrOth",
               main_mode == "HOV" & driver == "Passenger" ~ "Pass",
               TRUE                                        ~ main_mode
             )

  )



write_rds(trdat, here("analysis/data/derived_data/trdat.rds"))
