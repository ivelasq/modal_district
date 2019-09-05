# "Modal District" Analysis
# Inspired by analysis by Ben Casselman, Kenny Malone, Liza Yeager and Darian Woods.
# and the code behind Planet Money Episode 936: The Modal American

library(tidyverse)
library(readxl)
library(janitor)

# DOWNLOAD AND PARSE DATA -------------------------------------------------

# Data used is from the California Department of Data
# 2018-2019 files
# https://www.cde.ca.gov/ds/sd/sd/#e

# enrollment file
ca_enroll <-
  read_delim(here::here("data", "raw", "filesenr.asp.txt"), delim = "\t") %>% 
  clean_names()

# frpm file
ca_frpm <-
  read_excel(here::here("data", "raw", "frpm1819.xlsx"), sheet = 2)

# el file
ca_el <-
  read_delim(here::here("data", "raw", "fileselsch.txt"), delim = "\t")

# We want to look at race/ethnicity composition by district,
# We only want race/ethnicity, so we aggregate the number for each race by district
# Then calculate the proportion by race for each district
# Then we have to make the data wide in order to create the demographics bin

ca_enroll_district <-
  ca_enroll %>% 
  mutate(ethnic_name = case_when(ethnic == 0 ~ "Not Reported",
                                 ethnic == 1 ~ "American Indian or Alaska Native",
                                 ethnic == 2 ~ "Asian",
                                 ethnic == 3 ~ "Pacific Islander",
                                 ethnic == 4 ~ "Filipino",
                                 ethnic == 5 ~ "Hispanic",
                                 ethnic == 6 ~ "African American",
                                 ethnic == 7 ~ "White",
                                 ethnic == 9 ~ "Two or More Races")) %>% 
  select(district, ethnic_name, enr_total) %>% 
  group_by(district, ethnic_name) %>% 
  summarise(enr_total = sum(enr_total, na.rm = T)) %>% 
  mutate(prop = enr_total/sum(enr_total)) %>% 
  select(-enr_total) %>% 
  spread(ethnic_name, prop, -1, fill = NA) %>% 
  mutate(demographics = case_when(White < .5 ~ "Majority Non-White",
                                  TRUE ~ "Majority White"))
  

# RUN ANALYSIS ------------------------------------------------------------

grouper <- function(data, ...) {
  data %>% 
    group_by(...) %>% 
    tally() %>% 
    arrange(desc(n))
}

modal <- 
  ca_enroll_district %>% 
  grouper(demographics)

