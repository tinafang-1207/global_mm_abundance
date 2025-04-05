
### clean working environment ###
rm(list = ls())

### read in data ###
catch_orig <- readRDS("data/clean_data/1981_2021_bycatch_estimate_ratio_stratified_w_historical.Rds")
abd_orig <- read.csv("data/clean_data/U.S_Abundance_Clean.csv")
obs_mort_orig <- readRDS("data/clean_data/2007_2022_injury_mortality_data.Rds")

### read in library ###
library(tidyverse)

### format data ###

# bycatch data
catch <- catch_orig %>%
  filter(comm_name == "California sea lion") %>%
  select(comm_name, strata, year, nbycatch) %>%
  group_by(year) %>%
  summarize(total_bycatch = sum(nbycatch, na.rm = TRUE)) %>%
  mutate(total_bycatch = round(total_bycatch, digits = 0))

# other mortality
obs_mort <- obs_mort_orig %>%
  filter(species == "California sea lion") %>%
  select(year, state, species, interaction_type) %>%
  filter(interaction_type!= "CA halibut set gillnet fishery") %>%
  group_by(year) %>%
  summarize(other_mort_total = n())

# combine with bycatch data
catch_total <- left_join(catch, obs_mort, by = "year") %>%
  mutate(other_mort_total = ifelse(is.na(other_mort_total), 0, other_mort_total)) %>%
  mutate(total_catch = total_bycatch + other_mort_total)

# format abundance data
abd <- abd_orig %>%
  filter(common_name == "California sea lion" & abundance_units == "Non-pup") %>%
  select(year, abundance)

# Format final input dataframe
input_df_final <- merge(abd, catch_total, by = "year", all = TRUE) %>%
  select(year, abundance, total_catch) %>%
  mutate(abundance = ifelse(is.na(abundance), -999, abundance)) %>%
  mutate(total_catch = ifelse(is.na(total_catch), -999, total_catch))

### save the dataframe ###
write.csv(input_df_final, "data/clean_data/ca_sealion_input_df.csv", row.names = FALSE)





