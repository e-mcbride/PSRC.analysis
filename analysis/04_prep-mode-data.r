# nubmer of times used each mode on diary day

library(tidyverse)
library(here)
library(janitor)

trdat <- read_rds(here("analysis/data/derived_data/trdat.rds"))


# people who made no trips are missing from the `travel` data, so I add them in with the following
prdat <- read_rds(here("analysis/data/derived_data/prdat.rds"))

trpid <- trdat %>% 
  pull(personid) %>% 
  unique()

notrav <- prdat %>% 
  filter(!(person_id %in% trpid)) %>% 
  select(personid = person_id) %>% 
  mutate(hov = 0,
         sov = 0,
         walk = 0,
         bike = 0,
         transit = 0,
         other = 0)


mode <- trdat %>% 
  select(personid, main_mode) %>% 
  filter(!is.na(main_mode)) %>% 
  mutate(vals = 1) %>% 
  distinct() %>% 
  # group_by(personid) %>%
  pivot_wider(id_cols = personid, names_from = main_mode, values_from = vals, values_fill = 0) %>%
  clean_names() %>% 
  
  # Adding people who did not travel:
    bind_rows(notrav)



