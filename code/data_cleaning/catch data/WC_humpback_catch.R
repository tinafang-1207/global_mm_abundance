
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)
library(sf)

### read in data ###

# IWC individual catch data
individual <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/processed/individual_NP_catch_clean.Rds")

# IWC summary catch data
summary <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/processed/summary_catch_clean.Rds")

#non-wc hbk prop
prop <- read.csv("data/confidential/input_data/catch/nonwc_humpback_catch_proportion.csv") %>%
  select(latitude, proportion)

# world map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country ="Mexico", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country = "Canada", returnclass = "sf")

### clean data ###

# Ivashchenko & Chapman 2021 
# Hawaii had no recorded catches
# The catch off Central America is very limited as is believed to be Southern Hemisphere whales
# This left only three feeding/breeding grounds for wc humpback
# Br. Columbia, USA. W coast and Mexico
# 12/21/2025 remove Br.Columbia as the abundance data not include this region

summary_clean <- summary %>%
  filter(area %in% c("USA W coast", "Mexico")) %>%
  filter(hbk != 0) %>%
  filter(iwc_ind_details == "Individual catch complete") %>%
  select(expedition_iwc, expedition_new, area, hbk) %>%
  rename(exp_id_sum = expedition_iwc)

# join with individual catch database

ind_filtered <- individual %>%
  mutate(expedition_new = paste(exp_id_sum, catch_year, sep = "-")) %>%
  select(-exp_id_ind) %>%
  select(expedition_new, exp_id_sum, everything()) %>%
  inner_join(summary_clean, by = c("exp_id_sum", "expedition_new")) %>%
  filter(species == "Humpback whale")

ind_mex <- ind_filtered %>%
  filter(area == "Mexico") %>%
  group_by(catch_year) %>%
  summarize(total_catch = n()) %>%
  # The Mexico proportion is assigned based on Ivashchenko & Chapman 2021
  # The mx_np stock is not part of CA/OR/WA stock
  mutate(mx_np = round(0.47*total_catch,0),
         mx_mainland = round(0.43*total_catch,0),
         ca = round(0.07*total_catch,0)) %>%
  mutate(catch_wc = mx_mainland + ca) %>%
  select(catch_year, catch_wc) %>%
  mutate(region = "Mexico")


ind_us <- ind_filtered %>%
  filter(area != "Mexico") %>%
  # add latitude bin (Curtis et al., 2025)
  mutate(latitude = case_when(lat_dd >48.5 & lat_dd<= 51~51,
                             lat_dd >46.3 & lat_dd<=48.5~48.5,
                             lat_dd >44.3 & lat_dd<= 46.3~46.3,
                             lat_dd >42.8 & lat_dd <= 44.3~44.3,
                             lat_dd >41.1 & lat_dd <= 42.8~42.8,
                             lat_dd >38 & lat_dd <= 41.1~41.1,
                             lat_dd >37.1 & lat_dd <= 38~38,
                             lat_dd >35.5 & lat_dd <= 37.1~37.1,
                             lat_dd >34 & lat_dd <= 35.5~35.5,
                             lat_dd >30.5 & lat_dd <=34~34)) %>%
  left_join(prop, by = "latitude") %>%
  group_by(catch_year, latitude) %>%
  summarize(total_catch = n(), proportion = first(proportion)) %>%
  ungroup() %>%
  mutate(catch_mx_np = round(proportion * total_catch, 0)) %>%
  mutate(catch_wc = total_catch-catch_mx_np) %>%
  group_by(catch_year) %>%
  summarize(catch_wc = sum(catch_wc)) %>%
  mutate (region = "US & Canada")
    

ind_hbk_final <- bind_rows(ind_mex, ind_us) %>%
  group_by(catch_year) %>%
  summarize(catch = sum(catch_wc))

######################################################################

# Examine daily catch 
# The position of daily catch is not the position of whale but instead factory ship

summary_clean_daily <- summary %>%
  filter(area %in% c("USA W coast", "Mexico")) %>%
  filter(hbk != 0) %>% 
  filter(iwc_ind_details == "Daily catch available") %>%
  select(expedition_iwc, expedition_new, area, hbk) %>%
  rename(exp_id_sum = expedition_iwc)


daily_us <- summary_clean_daily %>%
  filter(area == "USA W coast")

daily_canada <- summary_clean_daily %>%
  filter(area == "Br.Columbia")

ind_daily_canada <- individual %>%
  mutate(expedition_new = paste(exp_id_sum, catch_year, sep = "-")) %>%
  select(-exp_id_ind) %>%
  select(expedition_new, exp_id_sum, everything()) %>%
  inner_join(daily_canada, by = c("exp_id_sum", "expedition_new")) %>%
  filter(species == "Humpback whale")




### save the final catch data ###

write.csv(ind_hbk_final, "data/confidential/input_data/catch/wc_humpback_catch.csv", row.names = FALSE)

write.csv(ind_hbk_final, "data/confidential/input_data/catch/wc_humpback_catch_new.csv", row.names = FALSE)





