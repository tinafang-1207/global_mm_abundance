
#### clean working environment #####
rm(list = ls())

#### read in package ####
library(tidyverse)

### read in data ####

data_orig <- read_csv("data/raw_data/U.S_Abundance.csv",na = "-")

species_key <- read_csv("data/raw_data/species_key.csv", na = "-")

# clean aundance time-series data

data_abundance_clean <- data_orig %>%
  rename(source_type = `source_type (digitize/actual)`) %>%
  select(species, stock_id, survey_method, abundance_units, year, abundance, abundance_low, abundance_hi, source_type) %>%
  # remove rows that are blank
  subset(species != "") %>%
  #categorize survey method
  mutate(survey_method = case_when(survey_method %in% c("Species distribution model","Model-Based, Encounter on Shipboard Line-transect Surveys","Population reconstruction model")~"Other model",
                                   survey_method %in% c("Bayesian Trend Analysis", "Bayesian Line-transect", "Aerial Survey, Bayesian Model", "Bayesian trend")~"Bayesian analysis",
                                   survey_method %in% c("Iced-based count, Acoustic","Shore-Based Count","Direct count","Pup counts","Capture-recapture, Field Counts, Trend Analysis","Haul-out counts","Shore-based counts")~"Count",
                                   survey_method %in% c("Ship-Based Line-transect", "Acoustic Point-Transect", "Design-Based, Encounter on Shipboard Line-transect Surveys")~"Line-transect",
                                   survey_method %in% c("Photo Mark Recapture","Photo Mark-Recapture", "Mark Recapture")~"Mark recapture",
                                   survey_method %in% c("Aerial Survey, Ground Survey", "Aerial Survey, Ground Survey, Ship Survey", "Ariel Survey, Ship-Based Survey", "Aerial Survey")~"Aerial survey",
                                   .default = survey_method)) %>%
  # categorize abundance_units
  mutate(abundance_units = case_when(abundance_units %in% c("Total Numbers", "Total Number", "Total number (Male and Female)", "Total Number**", "Total Number (raw)")~"Total number",
                                     abundance_units %in% c("Total Number (Females)**","Total Number (Females)")~"Total number(Females)",
                                     abundance_units %in% c("Total Number (Males)**", "Total Number (Males)")~"Total number(Males)",
                                     abundance_units %in% c("Non-pups", "Non-pup")~"Non-pup",
                                     abundance_units %in% c("Pups","Births")~"Pup")) %>%
  # mutate the class of year
  mutate(year = as.Date(as.character(year), format = "%Y")) %>%
  mutate(year = format(as.Date(year, format="%d/%m/%Y"),"%Y"))

# clean species key time-series data

species_key_clean <- species_key %>%
  rename(us_region = `U.S. region`) %>%
  select(-notes)

# join the cleaned abundance and species key datasets

abundance_overall <- left_join(data_abundance_clean, species_key_clean, by = "stock_id") %>%
  select(stock, stock_id, common_name, scientific_name, survey_method, abundance_units, year, abundance, abundance_low, abundance_hi, us_region, area, area_code, sub_area, parent_stock_id, source_type, catg1, catg2)

# save the cleaned data
write.csv(abundance_overall, file = "data/clean_data/U.S_Abundance_Clean.csv", row.names = FALSE)

