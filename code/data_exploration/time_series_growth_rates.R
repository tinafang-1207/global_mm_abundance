
#### clean working environment #####
rm(list = ls())

#### read in package ####
library(tidyverse)

# set plot directory
plotdir <- "figures"

### read in data ###
data <- read.csv("data/clean_data/U.S_Abundance_Clean.csv")

species_key <- read.csv("data/raw_data/species_key.csv", na = "-") %>%
  rename(us_region = `U.S..region`) %>%
  select(-notes)

# clean data

exp <- data %>%
  filter(common_name == "Blue whale") %>%
  select(-abundance_low, -abundance_hi)
  
  
  
  
  
  
  