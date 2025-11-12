
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)

### read in data ###
comm_catch <- readxl::read_excel("data/ENP_blue_whale_input/ENP_blue_whale_catch.xlsx")

abundance <- readxl::read_excel("data/ENP_blue_whale_input/ENP_blue_whale_abundance.xlsx")

obs_mort <- readRDS("data/clean_data/2007_2022_injury_mortality_data.Rds")

### clean data ###

# catch

obs_mort_clean <- obs_mort %>%
  filter(species == "Blue whale") %>%
  group_by(year) %>%
  summarize(catch = n())

comm_catch_clean <- comm_catch %>%
  rename(year = Year,
         catch = Catch)
  
catch_total <- bind_rows(comm_catch_clean, obs_mort_clean)

catch_total_full <- tibble (year = full_seq(catch_total$year,1)) %>%
  left_join(catch_total, by = "year") %>%
  mutate(catch = ifelse(year > 1971, replace_na(catch, 0), catch))

# abundance
abd_clean <- abundance %>%
  # derive the cv of estimates
  mutate(cv = standard_error/abundance) %>%
  # derive the sigma of estimates
  mutate(sigma = sqrt(log(1+cv^2))) %>%
  mutate(sigma = round(sigma,2))
  
  
data_total <- left_join(catch_total_full, abd_clean, by = "year") %>%
  filter(year <= 2018) %>%
  select(year, catch, abundance, sigma) %>%
  mutate(abundance = ifelse(is.na(abundance), -999, abundance)) %>%
  mutate(sigma = ifelse(is.na(sigma), 1, sigma))

# check data type
class(data_total$abundance)
class(data_total$catch)
class(data_total$sigma)

write.csv(data_total, "data/ENP_blue_whale_input/ENP_bluewhale_input_df.csv", row.names = FALSE)
  
  
