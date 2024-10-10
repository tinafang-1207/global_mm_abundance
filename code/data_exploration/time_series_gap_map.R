
#### clean working environment #####
rm(list = ls())

#### read in package ####
library(tidyverse)

### read in data ###
data <- read.csv("data/clean_data/U.S_Abundance_Clean.csv")

species_key <- read.csv("data/raw_data/species_key.csv", na = "-") %>%
  rename(us_region = `U.S..region`) %>%
  select(-notes)


# read in u.s shapefile
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")

### clean data ###

# clean data for cetaceans

data_clean_cetacean <- data %>%
  filter(catg1 == "Cetaceans") %>%
  select(stock, stock_id, survey_method, abundance_units, year, abundance, us_region, parent_stock_id, catg1, catg2) %>%
  filter(abundance_units == "Total number") %>%
  group_by(stock_id, survey_method, abundance_units) %>%
  summarize(total_years = n()) %>%
  group_by(stock_id) %>%
  filter(total_years == max(total_years)) %>%
  filter(!duplicated(stock_id)) %>%
  rename(max_est = total_years) %>%
  select(-survey_method, -abundance_units) %>%
  mutate(catg_est = case_when(max_est<7~"<7",
                              max_est>=7 & max_est <15 ~"7-15",
                              max_est>= 15 & max_est <30~"15-30",
                              max_est>=30~">30"))


# clean data for pinnipeds

data_clean_pinniped <- data %>%
  filter(catg1 == "Pinnipeds") %>%
  select(stock, stock_id, survey_method, abundance_units, year, abundance, us_region, parent_stock_id, catg1, catg2) %>%
  group_by(stock_id, survey_method, abundance_units) %>%
  summarize(total_years = n()) %>%
  filter(!stock_id %in% c("HBSEAL-CAISLANDS",
                          "HBSEAL-CAMAIN",
                          "NESEAL-CACENTRAL",
                          "NESEAL-CAISLANDS",
                          "NFSEAL-BO",
                          "NFSEAL-GR",
                          "NFSEAL-PL",
                          "STSEALION-CA",
                          "STSEALION-CALISLAND",
                          "STSEALION-CGUAK",
                          "STSEALION-EALISLAND",
                          "STSEALION-EGUAK",
                          "STSEALION-OR",
                          "STSEALION-SEAK",
                          "STSEALION-WA",
                          "STSEALION-WALISLAND",
                          "STSEALION-WGUAK",
                          "HBSEAL-WACOA",
                          "HBSEAL-ORCOA"
                          )) %>%
  group_by(stock_id) %>%
  filter(total_years == max(total_years)) %>%
  filter(!duplicated(stock_id)) %>%
  select(-survey_method, -abundance_units)%>%
  rename(max_est = total_years) %>%
  mutate(catg_est = case_when(max_est<7~"<7",
                              max_est>=7 & max_est <15 ~"7-15",
                              max_est>= 15 & max_est <30~"15-30",
                              max_est>=30~">30"))

  # combine the year numbers of cetacean and pinnipeds
data_clean_total <- bind_rows(data_clean_cetacean, data_clean_pinniped)

# clean species key
species_key_clean <- species_key %>%
  select(stock, stock_id, us_region, parent_stock_id, catg1, catg2) %>%
  filter(is.na(parent_stock_id)) %>%
  filter(!is.na(us_region)) %>%
  




