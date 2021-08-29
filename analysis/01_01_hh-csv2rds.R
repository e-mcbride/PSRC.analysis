# take raw hh data, build factors, write rds
library(tidyverse)
library(here)


raw_hh <- read_csv(here("analysis/data/raw_data/2017-2019-pr2-1-Household.csv"),
                   guess_max = 3500) #`guess_max` increases max # rows looked at to determine col type

hhdat <- raw_hh %>%
  mutate(hhincome_broad = ordered(hhincome_broad,
                                  c("Prefer not to answer",
                                    "Under $25,000",
                                    "$25,000-$49,999",
                                    "$50,000-$74,999",
                                    "$75,000-$99,999",
                                    "$100,000 or more")),
         hhincome_detailed = ordered(hhincome_detailed,
                                     c("Prefer not to answer",
                                       "Under $10,000",
                                       "$10,000-$24,999",
                                       "$25,000-$34,999",
                                       "$35,000-$49,999",
                                       "$50,000-$74,999",
                                       "$75,000-$99,999",
                                       "$100,000-$149,999",
                                       "$150,000-$199,999",
                                       "$200,000-$249,999",
                                       "$250,000 or more")),
         hhincome_followup = ordered(hhincome_followup,
                                     c("Prefer not to answer",
                                       "Under $25,000",
                                       "$25,000-$49,999",
                                       "$50,000-$74,999",
                                       "$75,000-$99,999",
                                       "$100,000 or more")),  # leaving out "Missing:skip logic" makes it NA
         lifecycle = ordered(lifecycle,
                             c("Household includes children under 5",
                               "Household includes children age 5-17",
                               "Household size = 1, Householder under age 35",
                               "Household size = 1, Householder age 35 - 64",
                               "Household size = 1, Householder age 65+",
                               "Household size > 1, Householder under age 35",
                               "Household size > 1, Householder age 35 - 64",
                               "Household size > 1, Householder age 65+")),
         dayofweek = ordered(dayofweek,
                             c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         rent_own = ordered(rent_own,
                            c("Own/paying mortgage",
                              "Rent",
                              "Provided by job or military",
                              "Other",
                              "Prefer not to answer")),
         res_type = ordered(res_type,
                            c("Single-family house (detached house)",
                              "Townhouse (attached house)",
                              "Building with 3 or fewer apartments/condos",
                              "Building with 4 or more apartments/condos",
                              "Mobile home/trailer",
                              "Dorm or institutional housing",
                              "Other (including boat, RV, van, etc.)"))
         )

write_rds(hhdat, here("analysis/data/derived_data/hhdat.rds"))
