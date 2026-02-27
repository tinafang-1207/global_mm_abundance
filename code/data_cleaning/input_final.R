
### read in library ###
library(tidyverse)

### read in data ###

# Abundance
blue_abd <- read.csv("data/confidential/input_data/abundance/ENP_blue_whale_abundance.csv", na.strings = "/")
hbk_abd <- read.csv("data/confidential/input_data/abundance/CA:OR_hbk_abundance_new.csv", na.strings = "/")
gray_abd <- read.csv("data/confidential/input_data/abundance/ENP_gray_abundance.csv", na.strings = "/")
ca_sealion_abd <- read.csv("data/confidential/input_data/abundance/CA_sealion_abundance.csv", na.strings = "/")
ca_seal_abd <- read.csv("data/confidential/input_data/abundance/CA_Harbor_seal_abundance.csv", na.strings = "/")
elephant_seal_abd <- read.csv("data/confidential/input_data/abundance/CA_Northern_elephant_seal_abundance.csv", na.strings = "/")



# Catch

gray_catch <- read.csv("data/confidential/input_data/catch/ENP_gray_catch.csv", na.strings = "/")
hbk_catch <- read.csv("data/confidential/input_data/catch/wc_humpback_catch_new.csv", na.strings = "/")
blue_catch <- read.csv("data/confidential/input_data/catch/ENP_blue_whale_catch.csv", na.strings = "/")
ca_gillnet_catch_est <- readRDS("data/confidential/input_data/catch/1981_2021_bycatch_estimate_ratio_stratified_w_historical.Rds")
mortality_obs <- readRDS("data/confidential/input_data/catch/2007_2022_injury_mortality_data.Rds")
ca_drift_gillnet_catch <- read.csv("data/confidential/input_data/catch/CA_drift_gillnet_bycatch.csv", na.strings = "/")

# Temp-PDO Index
temp <- read.csv("data/confidential/input_data/PDO_temp.csv", na.strings = "/")


############################## Clean data ##################################

# scale temperature
temp_clean <- temp %>% 
  select(Year, average) %>%
  filter(Year < 2025) %>%
  rename(
    year = Year,
    temp_raw = average
  ) %>%
  mutate(
    temp_mean = as.numeric(scale(temp_raw, center = TRUE, scale = FALSE)),
    temp_scaled = as.numeric(scale(temp_raw, center = TRUE, scale = TRUE))
  )

temp_clean_final <- temp_clean %>%
  select(year, temp_scaled)

# clean abundance

# gray whale
abd_gray <- gray_abd %>%
  complete(year = min(year):max(year)) %>%  # create full sequence of years
  mutate(abundance = ifelse(is.na(abundance), -999, abundance)) %>%
  mutate(species = "Gray_whale") %>%
  mutate(standard_error = (hcl-lcl)/(2*1.96)) %>%
  mutate(cv = standard_error/abundance) %>%
  # derive sigma
  mutate(sigma = sqrt(log(1+cv^2))) %>%
  mutate(sigma = round(sigma,2)) %>%
  select(species, year, abundance, sigma)

# Humpback whale
abd_hbk <- hbk_abd %>%
  mutate(cv = standard_error/abundance) %>%
  # derive sigma
  mutate(sigma = sqrt(log(1+cv^2))) %>%
  mutate(sigma = round(sigma,2)) %>%
  select(species, year, abundance, sigma)

# Blue whale
abd_blue <- blue_abd %>%
  mutate(cv = standard_error/abundance) %>%
  # derive sigma
  mutate(sigma = sqrt(log(1+cv^2))) %>%
  mutate(sigma = round(sigma,2)) %>%
  select(species, year, abundance, sigma)

# CA sealion
abd_sealion <- ca_sealion_abd %>%
  mutate(standard_error = (hcl-lcl)/(2*1.96)) %>%
  mutate(cv = standard_error/abundance) %>%
  # derive sigma
  mutate(sigma = sqrt(log(1+cv^2))) %>%
  mutate(sigma = round(sigma,2)) %>%
  select(species, year, abundance, sigma)

# CA harbor seal
abd_ca_seal <- ca_seal_abd %>%
  select(species, year, abundance_corrected, lcl, hcl) %>%
  rename(abundance = abundance_corrected) %>%
  mutate(standard_error = (hcl-lcl)/(2*1.96)) %>%
  mutate(cv = standard_error/abundance) %>%
  # derive sigma
  mutate(sigma = sqrt(log(1+cv^2))) %>%
  mutate(sigma = round(sigma,2)) %>%
  select(species, year, abundance, sigma)

# Northern elephant seal
abd_elephant_seal <- elephant_seal_abd %>%
  select(species, year, abundance_total, lcl, hcl) %>%
  rename(abundance = abundance_total) %>%
  mutate(standard_error = (hcl-lcl)/(2*1.96)) %>%
  mutate(cv = standard_error/abundance) %>%
  # derive sigma
  mutate(sigma = sqrt(log(1+cv^2))) %>%
  mutate(sigma = round(sigma,2)) %>%
  select(species, year, abundance, sigma)

# catch

# Humpback whale
hbk_catch_clean <- hbk_catch %>%
  rename(year = catch_year) %>%
  complete(year = min(year):max(year)) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch)) %>%
  select(year, catch)

# Gray whale
gray_catch_clean <- gray_catch %>%
  select(year, catch) %>%
  complete(year = min(year):max(year)) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch)) %>%
  # only to the max year of abundance data
  filter(year <= 2015)

# Blue whale
blue_catch_clean <- blue_catch %>%
  complete(year = min(year):max(year)) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch))

# CA sea lion
sealion_bycatch_est <- ca_gillnet_catch_est %>%
  filter(comm_name == "California sea lion") %>%
  group_by(year) %>%
  summarize(catch_est = round(sum(nbycatch, na.rm = TRUE), 0))

sealion_bycatch_drift <- ca_drift_gillnet_catch %>%
  filter(species == "California_sea_lion") %>%
  rename(catch_drift = catch) %>%
  mutate(catch_drift = round(catch_drift, 0))

sealion_obs_est <- mortality_obs %>%
  filter(species == "California sea lion") %>%
  filter(!interaction_type %in% c("CA halibut set gillnet fishery", "CA swordfish drift gillnet fishery")) %>%
  group_by(year) %>%
  summarize(catch_obs = n())

sealion_catch_clean <- sealion_bycatch_est %>%
  left_join(sealion_obs_est, by = "year") %>%
  left_join(sealion_bycatch_drift, by = "year") %>%
  mutate(catch_obs = ifelse(is.na(catch_obs), 0, catch_obs),
         catch_drift = ifelse(is.na(catch_drift), 0, catch_drift)) %>%
  mutate(catch = catch_obs + catch_est + catch_drift) %>%
  select(year, catch)

# CA Harbor seal
ca_seal_bycatch_est <- ca_gillnet_catch_est %>%
  filter(comm_name == "Harbor seal") %>%
  group_by(year) %>%
  summarize(catch_est = round(sum(nbycatch, na.rm = TRUE), 0))

ca_seal_obs_est <- mortality_obs %>%
  filter(species == "Harbor seal"& state == "CA") %>%
  filter(interaction_type != "CA halibut set gillnet fishery") %>%
  group_by(year) %>%
  summarize(catch_obs = n())

ca_seal_catch_clean <- left_join(ca_seal_bycatch_est, ca_seal_obs_est, by = "year") %>%
  mutate(catch_obs = ifelse(is.na(catch_obs), 0, catch_obs)) %>%
  mutate(catch = catch_obs + catch_est) %>%
  select(year, catch)

# Northern elephant seal
elephant_seal_bycatch_est <- ca_gillnet_catch_est %>%
  filter(comm_name == "Northern elephant seal") %>%
  group_by(year) %>%
  summarize(catch_est = round(sum(nbycatch, na.rm = TRUE), 0))

elephant_seal_bycatch_drift <- ca_drift_gillnet_catch %>%
  filter(species == "Northern_elephant_seal") %>%
  rename(catch_drift = catch) %>%
  mutate(catch_drift = round(catch_drift, 0))

elephant_seal_obs_est <- mortality_obs %>%
  filter(species == "Northern elephant seal") %>%
  filter(!interaction_type %in% c("CA halibut set gillnet fishery", "CA swordfish drift gillnet fishery")) %>%
  group_by(year) %>%
  summarize(catch_obs = n())

elephant_seal_catch_clean <- elephant_seal_bycatch_est %>%
  left_join(elephant_seal_bycatch_drift, by = "year") %>%
  left_join(elephant_seal_obs_est, by = "year") %>%
  mutate(catch_obs = ifelse(is.na(catch_obs), 0, catch_obs),
         catch_drift = ifelse(is.na(catch_drift), 0, catch_drift)) %>%
  mutate(catch = catch_obs + catch_est + catch_drift) %>%
  select(year, catch)



# create input data

hbk_input_final <- full_join(hbk_catch_clean, abd_hbk, by = "year") %>%
  complete(year = min(year):max(year)) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch)) %>%
  mutate(abundance = ifelse(is.na(abundance), -999, abundance)) %>%
  mutate(sigma = ifelse(is.na(sigma), 1, sigma)) %>%
  mutate(species = "Humpback_whale") %>%
  # join the temp data
  left_join(temp_clean_final, by = "year") %>%
  select(species, year, abundance, catch, sigma, temp_scaled)

gray_input_final <- full_join(gray_catch_clean, abd_gray, by = "year") %>%
  complete(year = min(year):max(year)) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch)) %>%
  mutate(abundance = ifelse(is.na(abundance), -999, abundance)) %>%
  mutate(sigma = ifelse(is.na(sigma), 1, sigma)) %>%
  mutate(species = "Gray_whale") %>%
  # join the temp data
  left_join(temp_clean_final, by = "year") %>%
  select(species, year, abundance, catch, sigma, temp_scaled)

blue_input_final <- full_join(blue_catch_clean, abd_blue, by = "year") %>%
  complete(year = min(year):max(year)) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch)) %>%
  mutate(abundance = ifelse(is.na(abundance), -999, abundance)) %>%
  mutate(sigma = ifelse(is.na(sigma), 1, sigma)) %>%
  mutate(species = "Blue_whale") %>%
  # join the temp data
  left_join(temp_clean_final, by = "year") %>%
  select(species, year, abundance, catch, sigma, temp_scaled)

sealion_input_final <- full_join(sealion_catch_clean, abd_sealion, by = "year") %>%
  complete(year = min(year):max(year)) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch)) %>%
  mutate(abundance = ifelse(is.na(abundance), -999, abundance)) %>%
  mutate(sigma = ifelse(is.na(sigma), 1, sigma)) %>%
  mutate(species = "California_sea_lion") %>%
  # join the temp data
  left_join(temp_clean_final, by = "year") %>%
  select(species, year, abundance, catch, sigma, temp_scaled)

ca_seal_input_final <- full_join(ca_seal_catch_clean, abd_ca_seal, by = "year") %>%
  complete(year = min(year):max(year)) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch)) %>%
  mutate(abundance = ifelse(is.na(abundance), -999, abundance)) %>%
  mutate(sigma = ifelse(is.na(sigma), 1, sigma)) %>%
  mutate(species = "CA_Harbor_seal") %>%
  # join the temp data
  left_join(temp_clean_final, by = "year") %>%
  select(species, year, abundance, catch, sigma, temp_scaled)

elephant_seal_input_final <- full_join(elephant_seal_catch_clean, abd_elephant_seal, by = "year") %>%
  complete(year = min(year):max(year)) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch)) %>%
  mutate(abundance = ifelse(is.na(abundance), -999, abundance)) %>%
  mutate(sigma = ifelse(is.na(sigma), 1, sigma)) %>%
  mutate(species = "Northern_elephant_seal") %>%
  # join the temp data
  left_join(temp_clean_final, by = "year") %>%
  select(species, year, abundance, catch, sigma, temp_scaled)


input_final <- bind_rows(hbk_input_final, 
                         gray_input_final, 
                         blue_input_final, 
                         sealion_input_final, 
                         ca_seal_input_final,
                         elephant_seal_input_final)

# save data

write.csv(input_final, "data/confidential/input_data/input_final.csv", row.names = FALSE)


