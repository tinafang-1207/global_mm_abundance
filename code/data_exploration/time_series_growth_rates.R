
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

growth_rates <- read.csv("data/raw_data/U.S_growth_rates.csv", na = "-")

# clean data

growth_rates_clean <- growth_rates %>%
  select(stock_id, year, abundance, year_diff, abundance_diff, growth_rates_all, growth_rates_annual) %>%
  mutate(abundance = gsub(",","",abundance)) %>%
  mutate(abundance = as.numeric(abundance))

data_all <- left_join(data, growth_rates_clean, by = c("stock_id", "year","abundance"), relationship = "many-to-many") %>%
  select(stock, stock_id, common_name, scientific_name, survey_method, abundance_units, year, abundance, year_diff, abundance_diff, growth_rates_all, growth_rates_annual) %>%
  filter(!is.na(growth_rates_annual))

max_r <- data_all %>%
  group_by(common_name, stock_id, survey_method) %>%
  summarise(max_r = max(growth_rates_annual))

ggplot(max_r, aes(x = max_r)) +
  geom_density() +
  facet_wrap(.~common_name, scales = "free") +
  theme_bw()
  
  
  
  
  