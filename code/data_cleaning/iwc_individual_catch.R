
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)

### read in data ###

# North Pacific

data_orig <-  read.csv ("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/original/IndivData-CSVfmt/NP 14-01-2024.csv") %>%
  janitor::clean_names()

### clean data ###

data <- data_orig %>%
  # select column needed for estimate catch
  select(day, mon, year, sp, len, l_u, sx, lat, mn, x, ac, lon, mn_1, x_1, ac_1, exp, sum_ex, nt) %>%
  rename(catch_day = day,
         catch_month = mon,
         catch_year = year,
         species = sp,
         length = len,
         length_unit = l_u,
         sex = sx,
         lat = lat,
         lat_mn = mn,
         lat_position = x,
         ac_lat = ac,
         lon = lon,
         lon_mn = mn_1,
         lon_position = x_1,
         ac_lon = ac_1,
         exp_id_ind = exp,
         exp_id_sum = sum_ex,
         nation = nt) %>%
  # clean category
  mutate(species = case_when(species == "0"~"Unknown",
                             species == "1"~"Pilot whale",
                             species == "6"~"Sperm whale",
                             species == "11"~"Right whale",
                             species == "16"~"Pygmy right whale",
                             species == "21"~"Sei/Bryde's whale",
                             species == "2"~"Bottlenose whale",
                             species == "7"~"Humpback whale",
                             species == "12"~"Gray whale",
                             species == "17"~"Cuvier's beaked whale",
                             species == "22"~"dolphin",
                             species == "3"~"Killer whale",
                             species == "8"~"Sei whale",
                             species == "13"~"Baird's beaked whale",
                             species == "18"~"Bowhead whale",
                             species == "4"~"Blue whale",
                             species == "9"~"Common minke whale",
                             species == "14"~"Baleen whale (unspecified)",
                             species == "19"~"Beaked whale (unspecified)",
                             species == "5"~"Fin whale",
                             species == "10"~"Bryde's whale",
                             species == "15"~"Pygmy blue whale",
                             species == "20"~"Antarctic minke whale") ) %>%
  # clean length data
  mutate(length = ifelse(length == "0", NA, length)) %>%
  mutate(length_unit = case_when(length_unit == "2"~"feet",
                                 length_unit == "3"~"meter",
                                 length_unit == "4"~"length and feet")) %>%
  # clean sex data 
  mutate(sex = case_when(sex == "0"~NA,
                         sex == "1"~"Male",
                         sex == "2"~"Female",
                         sex == "3"~"Both")) %>%
  # clean the position data
  mutate(lat_dd = lat + (lat_mn/60)) %>%
  mutate(long_dd = lon + (lon_mn/60),
         long_dd = ifelse(lon_position == "W ", -long_dd, long_dd)) %>%
  select(-lat, -lat_mn, -lat_position, -lon, -lon_mn, -lon_position) %>%
  mutate(ac_lat = case_when(ac_lat == "0"~"Unknown",
                            ac_lat %in%c("1", "2")~"Exact",
                            ac_lat == "3"~"Approx (landing)",
                            ac_lat == "4"~"Estimate")) %>%
  mutate(ac_lon = case_when(ac_lon == "0"~"Unknown",
                            ac_lon %in% c("1", "2")~"Exact",
                            ac_lon == "3"~"Approx (landing)",
                            ac_lon == "4"~"Estimate")) %>%
  mutate(lat_dd = ifelse(ac_lat == "Unknown", NA, lat_dd),
         long_dd = ifelse(ac_lon == "Unknown", NA, long_dd)) %>%
  # clean nation
  mutate(nation = case_when(nation == "Ca "~"CAN",
                            nation == "US "~ "USA",
                            nation == "No "~"Nor",
                            nation == "Jp "~"JPN",
                            nation == "UR "~"USSR",
                            nation == "Ko "~"KOR",
                            nation == "Ph "~"PHL",
                            nation == "Ru "~"RUS"
                            )) %>%
  select(exp_id_ind, exp_id_sum, nation, catch_day, catch_month, catch_year, species, length, length_unit, sex, lat_dd, ac_lat, long_dd, ac_lon )

# save the data
saveRDS(data, file = "/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/processed/individual_NP_catch_clean.Rds")




