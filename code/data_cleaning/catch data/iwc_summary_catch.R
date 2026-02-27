
### clean working environment ####
rm(list = ls())

### read in library ###
library(tidyverse)
library(readxl)
library(sf)

### read in data ###
catch_orig <- read_excel("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/original/SummaryDatabaseV7.1/All-V7-2-Jun-2023.xlsx", sheet = "ExtraInfo") %>%
  janitor::clean_names()

# expedition_orig <- read_excel("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/SummaryDatabaseV7.1/All-V7-2-Jun-2023.xlsx", sheet = "Expeditions")

# clean catch data
data <- catch_orig %>%
  # rename columns 
  rename(ocean = oc,
         expedition_iwc = ex,
         landing_site_company = land_st_floating_factory,
         exp_start_day = x7,
         exp_start_month = start,
         exp_start_year = x9,
         exp_end_day = x10,
         exp_end_month = end,
         exp_end_year = x12,
         unspec_lw = unsp,
         iwc_ind_details = dat,
         operation_type = ty
         ) %>%
  # break the landing_station into landing site, flt_factory and company name
  #separate(landing_station, into =c("landing_site", "company"), sep = ";")
  mutate(start_date = as.Date(paste(exp_start_day, exp_start_month, exp_start_year, sep = "-"), format = "%d-%b-%Y")) %>%
  mutate(end_date = as.Date(paste(exp_end_day, exp_end_month, exp_end_year, sep = "-"), format = "%d-%b-%Y")) %>%
  select(-exp_start_day, -exp_start_month, -exp_start_year, -exp_end_day, -exp_end_month, -exp_end_year, -girth, -blubber_thickness, -testes_wt, -weight, -detailed_lengths, -colour, -indiv_posns, -reference) %>%
  # combine info from area (location) 
  left_join(area_clean, by = c("ocean", "area")) %>%
  # create new expedition column
  mutate(expedition_new = paste(expedition_iwc, year, sep = "-")) %>%
  select(year, expedition_iwc, expedition_new, nation, ocean, area, min_lat, max_lat, min_lon, max_lon, start_date, end_date, everything()) %>%
  # clean category
  mutate(ocean = case_when(ocean == "NA"~"North Atlantic",
                           ocean == "SH"~"Southern Hemisphere",
                           ocean == "NP"~"North Pacific",
                           ocean == "IO"~"Indian Ocean",
                           ocean == "SA"~"South Atlantic",
                           ocean == "SP"~"South Pacific")) %>%
  mutate(iwc_ind_details = case_when(iwc_ind_details == "Y:d"~"Daily catch available",
                                     iwc_ind_details  == "Y"~"Individual catch complete",
                                     iwc_ind_details  == "Y:m"~"Monthly catch available",
                                     iwc_ind_details  == "Y:w"~"Weekly catch available",
                                     iwc_ind_details  == "Y:p"~"Individual catch no position",
                                     iwc_ind_details  == "R"~"Individual restricted access"
                             )) %>%
  mutate(operation_type = case_when(operation_type == "C"~"Commercial",
                                    operation_type == "S"~"Special permit",
                                    operation_type == "S*"~"Special permit*",
                                    operation_type == "A"~"Aboriginal",
                                    operation_type == "I"~"Illegle",
                                    operation_type == "Co"~"Commercial under objection",
                                    operation_type == "Cr"~"Commercial under reservation"))

saveRDS(data, file = "/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/processed/summary_catch_clean.Rds")










