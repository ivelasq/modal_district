# "Modal District" Analysis
# Inspired by analysis by Ben Casselman, Kenny Malone, Liza Yeager and Darian Woods.
# and the code behind Planet Money Episode 936: The Modal American

library(tidyverse)


# DOWNLOAD AND PARSE DATA -------------------------------------------------

# Data used is from the Common Core of Data 2016-17 district-level file
# https://nces.ed.gov/ccd/pubagency.asp


ccd <-
  read_csv(here::here("data", "raw", "ccd_lea_052_1617_l_2a_11212017_csv 2.csv"))

# We want to look at race/ethnicity composition by district,
# so we filter by TOTAL INDICATOR == "Category Set A - By Race/Ethnicity; Sex; Grade"
# We only want race/ethnicity, so we aggregate the number for each race by district
# Then calculate the proportion by race for each district
# Then we have to make the data wide in order to create the bin "Majority White/Majority Non-White"

ccd_wide <-
  ccd %>% 
  filter(TOTAL_INDICATOR == "Category Set A - By Race/Ethnicity; Sex; Grade") %>% 
  select(LEAID, RACE_ETHNICITY, STUDENT_COUNT) %>% 
  group_by(LEAID, RACE_ETHNICITY) %>% 
  summarise(STUDENT_COUNT = sum(STUDENT_COUNT, na.rm = T)) %>% 
  mutate(prop = STUDENT_COUNT/sum(STUDENT_COUNT)) %>%
  mutate_at(vars(prop), ~replace(., is.nan(.), NA)) %>% # dividing by zeros created some NaN
  # I am assuming we don't want districts who did not report in our dataset
  select(-STUDENT_COUNT) %>% 
  spread(RACE_ETHNICITY, prop, -1, fill = NA) %>% 
  mutate(demographics = case_when(White < .5 ~ "Majority White",
                                  TRUE ~ "Majority Non-White"))

# RUN ANALYSIS ------------------------------------------------------------

grouper <- function(data, ...) {
  data %>% 
    group_by(...) %>% 
    tally() %>% 
    arrange(desc(n))
}

modal <- 
  ccd_wide %>% 
  grouper(demographics)
