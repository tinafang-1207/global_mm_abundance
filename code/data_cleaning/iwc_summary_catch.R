
### clean working environment ####
rm(list = ls())

### read in library ###
library(tidyverse)
library(readxl)
library(sf)

### read in data ###
catch_orig <- read_excel("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/original/SummaryDatabaseV7.1/All-V7-2-Jun-2023.xlsx", sheet = "ExtraInfo") %>%
  janitor::clean_names()

area_orig <- read_excel("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/original/SummaryDatabaseV7.1/All-V7-2-Jun-2023.xlsx", sheet = "Areas") %>%
  janitor::clean_names()

world <- rnaturalearth::ne_countries(returnclass = "sf")

# expedition_orig <- read_excel("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/SummaryDatabaseV7.1/All-V7-2-Jun-2023.xlsx", sheet = "Expeditions")

### clean data ###

# expedition area
area <- area_orig %>%
  rename(ocean = oc,
         min_lat = min_4,
         max_lat = max_5,
         min_lon = min_6,
         max_lon = max_7) %>%
  mutate(area_note = x9) %>%
  select(-code, -x8, -x9)

# convert the Expedition area to a sf object
area_clean <- area %>%
  separate(min_lat, into = c("min_lat_val", "min_lat_dir"), sep = "(?<=\\d)(?=\\D)") %>%
  separate(max_lat, into = c("max_lat_val", "max_lat_dir"), sep = "(?<=\\d)(?=\\D)") %>%
  separate(min_lon, into = c("min_lon_val", "min_lon_dir"), sep = "(?<=\\d)(?=\\D)") %>%
  separate(max_lon, into = c("max_lon_val", "max_lon_dir"), sep = "(?<=\\d)(?=\\D)") %>%
  mutate(across(ends_with("_val"), as.numeric)) %>%
  mutate(min_lon_val = ifelse(min_lon_dir == "E",min_lon_val, -min_lon_val),
         max_lon_val = ifelse(max_lon_dir == "E", max_lon_val, -max_lon_val),
         min_lat_val = ifelse(min_lat_dir == "N", min_lat_val, -min_lat_val),
         max_lat_val = ifelse(max_lat_dir == "N", max_lat_val, -max_lat_val)) %>%
  select(-max_lat_dir, -min_lat_dir, -max_lon_dir, -min_lon_dir) %>%
  rename(min_lat = min_lat_val,
         max_lat = max_lat_val,
         min_lon = min_lon_val,
         max_lon = max_lon_val)


areas_clean_sf <-  do.call(
  rbind,
  lapply(1:nrow(area_clean), function(i) {
    with(area_clean[i, ], {
      st_as_sf(
        data.frame(area = area, ocean = ocean),
        geometry = st_sfc(
          st_polygon(list(matrix(
            c(min_lon, min_lat,
              min_lon, max_lat,
              max_lon, max_lat,
              max_lon, min_lat,
              min_lon, min_lat),
            ncol = 2, byrow = TRUE
          )))
        ),
        crs = 4326
      )
    })
  })
)
  


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
saveRDS(areas_clean_sf, file = "/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/processed/catch_area_sf.Rds")










