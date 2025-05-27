
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)

### read in data ###
catch_orig <- readRDS("data/clean_data/1981_2021_bycatch_estimate_ratio_stratified_w_historical.Rds")
abd_orig <- read.csv("data/clean_data/U.S_Abundance_Clean.csv")
obs_mort_orig <- readRDS("data/clean_data/2007_2022_injury_mortality_data.Rds")

# clean catch data
catch <- catch_orig %>%
  filter(comm_name == "Harbor porpoise") %>%
  select(comm_name, strata, year, nbycatch) %>%
  group_by(year, strata) %>%
  summarize(total_bycatch = sum(nbycatch, na.rm = TRUE)) %>%
  mutate(total_bycatch = round(total_bycatch, digits = 0)) %>%
  filter(strata %in% c("San Francisco", "Monterey Bay", "Morro Bay")) %>%
  rename(stock = strata)

obs_mort <- obs_mort_orig %>%
  filter(species == "Harbor porpoise") %>%
  select(year, state, species, stock, interaction_type) %>%
  filter(stock %in% c("San Francisco - Russian River", "Monterey", "Morro Bay", "Northern California - Southern Oregon")) %>%
  group_by(year, stock) %>%
  summarize(other_mort_total = n()) %>%
  mutate(stock = case_when(stock == "San Francisco - Russian River"~"San Francisco",
                           stock == "Monterey"~"Monterey Bay",
                           .default = stock))


catch_total <- left_join(catch, obs_mort, by = c("year", "stock")) %>%
  mutate(other_mort_total = ifelse(is.na(other_mort_total), 0, other_mort_total)) %>%
  mutate(total_catch = total_bycatch + other_mort_total)


abd <- abd_orig %>%
  filter(common_name == "Harbor porpoise") %>%
  select(year, abundance, area) %>%
  rename(stock = area) %>%
  mutate(stock = case_when(stock == "San Francisco/Russian River"~"San Francisco",
                           stock == "Northern California/Southern Oregon"~"Northern California - Southern Oregon",
                           .default = stock))

input_df_final <- merge(abd, catch_total, by = c("year", "stock"), all = TRUE) %>%
  select(year, stock, abundance, total_catch) %>%
  mutate(abundance = ifelse(is.na(abundance), -999, abundance)) %>%
  mutate(total_catch = ifelse(is.na(total_catch), -999, total_catch)) %>%
  mutate(stock = ifelse(stock == "San Francisco","San Francisco - Russian River", stock))

write.csv(input_df_final, "data/clean_data/ca_harbor_porpoise_input_df.csv", row.names = FALSE)



