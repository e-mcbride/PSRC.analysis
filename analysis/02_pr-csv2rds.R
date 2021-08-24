# take raw pr data, build factors, write rds
library(tidyverse)
library(here)

raw_pr <- read_csv(here("analysis/data/raw_data/2017-2019-pr2-2-Person.csv"),
                   guess_max = 7000) #`guess_max` increases max # rows looked at to determine col type

prdat <- raw_pr %>%
  mutate(age = ordered(age,
                       c("Under 5 years old",
                         "5-11 years",
                         "12-15 years",
                         "16-17 years",
                         "18-24 years",
                         "25-34 years",
                         "35-44 years",
                         "45-54 years",
                         "55-64 years",
                         "65-74 years",
                         "75-84 years",
                         "85 or years older")),
         age_category = ordered(age_category,
                                c("Under 18 years",
                                  "18-64 years",
                                  "65 years+")),
         gender = ordered(gender,
                          c("Male",
                            "Female",
                            "Another",
                            "Prefer not to answer")),
         employment = ordered(employment,
                              c("Employed full time (35+ hours/week, paid)",
                                "Employed part time (fewer than 35 hours/week, paid)",
                                "Self-employed",
                                "Unpaid volunteer or intern",
                                "Homemaker",
                                "Retired",
                                "Not currently employed")),
         jobs_count = ordered(jobs_count,
                              c("0 jobs",
                                "1 job",
                                "2 jobs",
                                "3 jobs",
                                "4 jobs",
                                "5 or more jobs")),
         education = ordered(education,
                             c("Less than high school",
                               "High school graduate",
                               "Some college",
                               "Vocational/technical training",
                               "Associates degree",
                               "Bachelor degree",
                               "Graduate/post-graduate degree")),
         student = ordered(student,
                           c("No, not a student",
                             "Part-time student",
                             "Full-time student")),
         schooltype = ordered(schooltype,
                              c("Daycare",
                                "Preschool",
                                "K-12 public school",
                                "K-12 private school",
                                "K-12 home school (full-time or part-time)",
                                "College, graduate, or professional school",
                                "Vocational/technical school",
                                "Other",
                                "None"))
         )





