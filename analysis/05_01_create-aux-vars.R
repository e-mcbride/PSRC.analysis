# Building auxiliary variables

# we want all these in the same table
## to use them for analyzing their relationships to the model results

library(tidyverse)
library(here)
library(tidyxl)
library(unpivotr)
# NOTES ##################################################################################
# will need to get day of week from trdat!



# 1. building SES auxiliary variables
hhdat <- PSRCData::hhdat
prraw <- PSRCData::prdat

# ## when at least 2 race categories are selected, what is race_category?
#
# prraw %>% pull(race_category) %>% unique()
# race <- prraw %>%
#   select(starts_with("race")) %>%
#   # mutate(across(starts_with("race") & !contains("race_category"),
#   #               ~ factor(.x, c("Not Selected", "Selected")))
#   #        )
#   mutate(across(starts_with("race") & !contains("race_category"),
#                 ~ if_else(.x == "Selected", TRUE, FALSE))
#          ) %>%
#   mutate(total_cats = (race_afam + race_aiak + race_asian + race_hapi + race_hisp + race_white + race_other)) %>%
#   arrange(desc(total_cats))
#
#
# # FROM THIS I LEARNED: if more than 1 race cat is selected, if 2 are selected and 1 is white, then the
# # non-white race is the one that they are labeled as. Otherwise, they are categorized as "Other"

# Person level to household level ########################################################################

## Agegroups -------------------------------------------------------------------------------------------
prraw$age %>% unique()
prraw$age_category %>% unique()

hh_Agegrp_count <- prraw %>%
  # mutate(AgeGrp = case_when(
  #   age == "Under 5 years old" ~ "Age00-04",
  #   age >= "5-11 years" & age < "18-24 years" ~ "Age05-17",
  #   age >= "18-24 years" ~"Age18-99"
  # )) %>%
  group_by(hhid) %>%
  summarise(Age00_04 = sum(age == "Under 5 years old", na.rm = TRUE),
            Age05_15 = sum(age >= "5-11 years" & age < "16-17 years", na.rm = TRUE),
            Age16_17 = sum(age == "16-17 years", na.rm = TRUE),
            Age18_99 = sum(age >= "18-24 years", na.rm = TRUE)
            )




## Income ------------------------------------------------------------------------------------------------------------

# first: numeric versions of income values
incomevals <- hhdat %>%
  select(hhincome_detailed) %>%
  unique() %>%
  mutate(renameinc = str_replace(hhincome_detailed, "Under ", "$0-"),
         renameinc = str_replace(renameinc, "or more", "-$999,999"),

         renameinc = if_else(renameinc == "Prefer not to answer", NA_character_, renameinc),
         renameinc = str_remove_all(renameinc,c("\\$|\\,"))) %>%
  tidyr::separate(col = renameinc, into = c("inc_lo", "inc_hi"), sep = "-", remove = TRUE) %>%
  mutate(inc_lo = as.numeric(inc_lo),
         inc_hi = as.numeric(inc_hi)
         ) %>%
  arrange(inc_lo)

incomevals_broad <- hhdat %>%
  select(hhincome_broad) %>%
  unique() %>%
  mutate(rninc_broad = str_replace(hhincome_broad, "Under ", "$0-"),
         rninc_broad = str_replace(rninc_broad, "or more", "-$999,999"),
         rninc_broad = if_else(rninc_broad == "Prefer not to answer", NA_character_, rninc_broad),
         rninc_broad = str_remove_all(rninc_broad,c("\\$|\\,"))
  ) %>%
  tidyr::separate(col = rninc_broad, into = c("inc_lo_b", "inc_hi_b"), sep = "-", remove = TRUE) %>%
  mutate(inc_lo_b = as.numeric(inc_lo_b),
         inc_hi_b = as.numeric(inc_hi_b)
  ) %>%
  arrange(inc_lo_b)


# For the poverty lines: Using the Washington State self-sufficiency standard of 2017
# LINK TO PAGE: http://www.selfsufficiencystandard.org/washington
# 2017 DOCUMENT: http://selfsufficiencystandard.org/sites/default/files/selfsuff/docs/WA2017_SSS.pdf

# infant, preschooler, school-age, teenager, adult defined ages from document above:
# infants: 0-2 yrs
# presch: 3-5 yrs
# school: 6-12 yrs
# teen: 13-18 yrs

# Infants: under 5 years old and not in preschool
# Preschoolers: under 5 years old and in preschool
# school-age: 5-11 yrs
# teen: 12-15 yrs & 16-17 yrs
# adult: >= 18-24 yrs
famcode <- prraw %>%
  group_by(hhid) %>%
  summarise(infant = sum(age == "Under 5 years old" & schooltype != "Preschool", na.rm = TRUE),
            preschool = sum(age == "Under 5 years old" & schooltype == "Preschool", na.rm = TRUE),
            schoolage = sum(age == "5-11 years", na.rm = TRUE),
            teen = sum(age >= "12-15 years" & age < "18-24 years", na.rm = TRUE),
            adult = sum(age >= "18-24 years", na.rm = TRUE)
  ) %>%
  mutate(adl = paste0("a", adult),
         inf = paste0("i", infant),
         pre = paste0("p", preschool),
         sch = paste0("s", schoolage),
         tee = paste0("t", teen),

         famcode = paste0(adl, inf, pre, sch, tee)
         ) %>%
  select(hhid, famcode)

# download poverty table of 2017, pull sheet named "SSS"
raw <- "WA2017_all_families.xlsb"

RawPath <- here::here(paste0("analysis/data/raw_data/", raw))

SelfSuffDataURL <- "http://selfsufficiencystandard.org/sites/default/files/selfsuff/docs/WA2017_all_families.xlsb"

if(!file.exists(RawPath)) {
    download.file(SelfSuffDataURL, RawPath, mode = "wb")
}
rm(SelfSuffDataURL, raw, RawPath)

# read in table using tidyxl

# UNFORTUNATELY a manual step is necessary.
# 1. Open `WA2017_all_families.xlsb` you just downloaded
# 2. Save As type .xlsx

raw.xlsx <- "WA2017_all_families.xlsx"
raw.xlsx.path <- here::here(paste0("analysis/data/raw_data/", raw.xlsx))
file.exists(raw.xlsx.path)
# tidyxl::xlsx_sheet_names(raw.xlsx.path)

sss_cells <- tidyxl::xlsx_cells(raw.xlsx.path, sheets = "SSS")

# clean up table using unpivotr
sss_clean <- sss_cells %>%
  filter(row > 8, # removing unnecessary rows at the top
         col != 2, # clearing column 2 that is unnecessary
         !(row == 9 & col == 1), # clear the name "famcode" from the top of the column of counties
         !is_blank) %>%
  select(row, col, data_type, character, numeric) %>%
  behead("up", famcode) %>%
  behead("left", county) %>%
  rename(sss = numeric) %>%
  select(-row, -col, -character)

# fix counties with multiple numbers (average them)

## 1. which counties are included in PSTP
pstp.cnt.regex <- hhdat %>% pull(sample_county) %>% unique() %>% str_c(collapse="|")
### King, Pierce, Kitsap, Snohomish

## 2. pull those counties from sss_clean, see how many different values there are for each county
countysel <- sss_clean %>%
  filter(grepl(pstp.cnt.regex, county)) %>%
  pull(county) %>%
  unique()

## 3. average the amounts for each famcode category

sss_cntyAvgs <- sss_clean %>%
  filter(county %in% countysel) %>%
  # create new var with shortened county names
  mutate(cnty = str_extract(county, "[:alpha:]+")) %>%
  # use new var as grouping var plus the famcode, get the mean of each group
  group_by(cnty, famcode) %>%
  summarise(county_mean_sss = mean(sss)) %>%
  rename(county = cnty)

# link money amount to each hh based on their county and their famcode
# hh_sss <- hhdat %>%
#   select(hhid, sample_county) %>%
#   left_join(famcode, by = "hhid") %>%
#   left_join(sss_cntyAvgs, by = c("sample_county" = "county", "famcode"))

# is their income above, below, or around this number

hh_incvars <- hhdat %>%
  select(hhid, sample_county, hhincome_detailed, hhincome_broad, hhincome_followup) %>%
  left_join(incomevals, by = "hhincome_detailed") %>%
  left_join(famcode, by = "hhid") %>%
  left_join(sss_cntyAvgs, by = c("sample_county" = "county", "famcode")) %>%
  mutate(inc_lvl_det =
           case_when(
             # inc_hi >= county_mean_sss ~ "Above SSS",
             inc_lo >= county_mean_sss & inc_hi >= county_mean_sss ~ "Above SSS",
             inc_lo <= county_mean_sss & inc_hi >= county_mean_sss ~ "Around SSS",
             inc_lo <= county_mean_sss & inc_hi <= county_mean_sss ~ "Below SSS"
             # inc_lo >= county_mean_sss & inc_hi <= county_mean_sss ~ "Around SSS",
             # inc_lo < county_mean_sss ~ "Below SSS"
           )
  ) %>%

  # for those who prefer not to answer the "detailed" category:
  left_join(incomevals_broad, by = "hhincome_broad") %>%
  mutate(inc_lvl =
           case_when(
             !is.na(inc_lvl_det) ~ inc_lvl_det,
             inc_lo_b >= county_mean_sss & inc_hi_b >= county_mean_sss ~ "Above SSS",
             inc_lo_b <= county_mean_sss & inc_hi_b >= county_mean_sss ~ "Around SSS",
             inc_lo_b <= county_mean_sss & inc_hi_b <= county_mean_sss ~ "Below SSS"
             # is.na(inc_lv_det) & !(hhincome_broad %in% "Prefer not to answer")
           )
  ) %>%
  mutate(inc_lvl =
           ordered(inc_lvl, levels = c("Below SSS", "Around SSS", "Above SSS"))
  ) %>%
  select(hhid, famcode, inc_lvl)

hh_incvars %>% group_by(inc_lvl) %>% summarise(n = n())

# Final Vars ##################################################################################

## hh vars =============================================================================


hhvars <- hhdat %>%
  select(hhid, hhsize, lifecycle, numworkers, numadults, numchildren, lifecycle, hhincome_broad, hhincome_detailed,
         starts_with("res_factors")
         ) %>%
  left_join(hh_Agegrp_count, by = "hhid") %>%
  left_join(hh_incvars, by = "hhid") %>%
  # rename(across(-starts_with("hh"), ~ paste0("HH_", .) ))
  rename_at(.vars = vars(-starts_with("hh")), list(~paste0("HH_", .)))


## person vars ==========================================================================

prsel <- prraw %>%
  mutate(agegrp =
           case_when( # under 5, 5-15, 16-17, 18-34, 35-64,65+
             age == "Under 5 years old" ~ "age00_04",
             age <= "12-15 years"       ~ "age05_15",
             age <= "16-17 years"       ~ "age16_17",
             age <= "25-34 years"       ~ "age18_34",
             age <= "55-64 years"       ~ "age35_64",
             age > "55-64 years"        ~ "age65_99"
           ),
         agegrp = ordered(agegrp, c("age00_04",
                                    "age05_15",
                                    "age16_17",
                                    "age18_34",
                                    "age35_64",
                                    "age65_99"))) %>%
  mutate(
    schooltype = case_when(
      schooltype == "None"                                      ~ "None (baby)",
      schooltype <="K-12 home school (full-time or part-time)"  ~ "Daycare, preschool, K-12",
      schooltype >= "College, graduate, or professional school" ~ "Upper education",
      is.na(schooltype)                                         ~ "None (adult)"),
    schooltype = as.factor(schooltype)
  ) %>%  # schooltype = none (baby), k-12, upper education, none (adult)

  select(personid, hhid, pernum, age, agegrp, gender, employment, worker, student, schooltype, education, license,
         starts_with("race"), race_category,
         starts_with("wbt_"),
         relationship,
         starts_with("mode_freq"))

# prsel %>% select(starts_with("schooltype")) %>% View()

# # check `agegrp` var
# prsel %>% group_by(age) %>% summarise(n = n())
# prsel %>% group_by(agegrp) %>% summarise(n = n())


## joining hhvars to person-lvl ==============================================================
sesvars <- prsel %>%
  left_join(hhvars, by = "hhid")



write_rds(sesvars, here("analysis/data/derived_data/pid_SES.rds"))
