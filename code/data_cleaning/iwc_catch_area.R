
### clean working environment ####
rm(list = ls())

### read in library ###
library(tidyverse)
library(readxl)
library(sf)
library(ggspatial)

### read in data ###
area_orig <- read_excel("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/original/SummaryDatabaseV7.1/All-V7-2-Jun-2023.xlsx", sheet = "Areas") %>%
  janitor::clean_names()

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# define crs
target_crs <-"+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=180 +datum=WGS84 +units=m +no_defs"

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
  filter(ocean == "NP") %>%
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
         max_lon = max_lon_val) %>%
  mutate(lon_diff = max_lon-min_lon) %>%
  mutate(min_lon_new = ifelse(lon_diff >=180, min_lon+360, min_lon)) 

area_clean_sf <-  do.call(
  rbind,
  lapply(1:nrow(area_clean), function(i) {
    with(area_clean[i, ], {
      st_as_sf(
        data.frame(area = area, ocean = ocean),
        geometry = st_sfc(
          st_polygon(list(matrix(
            c(min_lon_new, min_lat,
              min_lon_new, max_lat,
              max_lon, max_lat,
              max_lon, min_lat,
              min_lon_new, min_lat),
            ncol = 2, byrow = TRUE
          )))
        ),
        crs = 4326
      )
    })
  })
)

# transform the area crs
area_clean_sf <- st_transform(area_clean_sf, target_crs)


### plot the world map
world_transform <- world %>%
  st_break_antimeridian(lon_0 = 180) %>% 
  st_transform(crs = target_crs) 

g_catch_area <- ggplot() +
  geom_sf(data = world_transform, fill = "grey85", col = "white", linewidth=0.1) +
  geom_sf(data = area_clean_sf, aes(color = area), alpha = 0, linewidth = 0.5) +
  coord_sf(crs = st_crs(world_transform),
           default_crs = 4326,
           xlim = c(100, 280),   # degrees longitude (center = 180°)
           ylim = c(-20, 80),      # degrees latitude
           expand = FALSE) +
  theme_bw()



### save data

plotdir <- "data/confidential/iwc_catch_figure"

saveRDS(area_clean_sf, file = "/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/processed/catch_area_NP_sf.Rds")

ggsave(g_catch_area, filename=file.path(plotdir, "iwc_catch_area.png"), width=8, height=5, units="in", dpi=600)
