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

colnames(ca_frpm) <- as.character(unlist(ca_frpm[1,]))
ca_frpm = ca_frpm[-1, ]

ca_frpm <- ca_frpm %>% clean_names()

# el file
ca_el <-
  read_delim(here::here("data", "raw", "fileselsch.txt"), delim = "\t")

# We want to look at race/ethnicity composition by district,
# We only want race/ethnicity, so we aggregate the number for each race by district
# Then calculate the proportion by race for each district
# Then we have to make the data wide in order to create the demographics bin

ca_enroll_district <-
  ca_enroll %>% 
  mutate(district_code = str_sub(cds_code, 3, 7),
         ethnic_name = case_when(ethnic == 0 ~ "Not Reported",
                                 ethnic == 1 ~ "American Indian or Alaska Native",
                                 ethnic == 2 ~ "Asian",
                                 ethnic == 3 ~ "Pacific Islander",
                                 ethnic == 4 ~ "Filipino",
                                 ethnic == 5 ~ "Hispanic",
                                 ethnic == 6 ~ "African American",
                                 ethnic == 7 ~ "White",
                                 ethnic == 9 ~ "Two or More Races")) %>% 
  select(district_code, ethnic_name, enr_total) %>% 
  group_by(district_code, ethnic_name) %>% 
  summarise(enr_total = sum(enr_total, na.rm = T)) %>% 
  mutate(prop = enr_total/sum(enr_total)) %>% 
  select(-enr_total) %>% 
  spread(ethnic_name, prop, -1, fill = NA) %>% 
  mutate(demo_bucket = case_when(White < .5 ~ "Majority Non-White",
                                  TRUE ~ "Majority White"))
  
nrow(ca_enroll_district) # 1034 districts

# We want to create a bucket for FRPM.

ca_frpm_district <-
  ca_frpm %>% 
  select(district_code, enrollment_k_12, frpm_count_k_12) %>% # we need to calculate frpm percentage so that it is a weighted average
  mutate_at(vars(enrollment_k_12, frpm_count_k_12), funs(as.numeric)) %>% 
  group_by(district_code) %>% 
  summarise_all(funs(sum(.))) %>% 
  mutate(district_frpm_pct = frpm_count_k_12/enrollment_k_12,
         frpm_bucket = case_when(district_frpm_pct > .59 ~ "More than 59% FRPM",
                                 TRUE ~ "Less than 59% FRPM"))

summary(ca_frpm_district$district_frpm_pct) # let's see what the data looks like
# I used the median to decide the bucket
sum(is.na(ca_frpm_district$frpm_bucket))
nrow(ca_frpm_district)

# join the datasets

full_district_data <-
  full_join(ca_enroll_district, ca_frpm_district, by = "district_code")

sum(is.na(full_district_data$frpm_bucket))
nrow(full_district_data)

# RUN ANALYSIS ------------------------------------------------------------

grouper <- function(data, ...) {
  data %>% 
    group_by(...) %>% 
    tally() %>% 
    arrange(desc(n))
}

modal <- 
  full_district_data %>% 
  grouper(demo_bucket, frpm_bucket)

