
### clean working environment ###
rm(list = ls())

### read in packages ###
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(tidyterra)

### read in data ###
sst_df <- readRDS("data/habitat_gis/sst_cobe2_us_west_coast.rds")
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")
states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  st_transform(4326)

# cetacean habitat shapefile
cetacean_orig <- st_read("data/habitat_gis/BIA_II/WC_WGS84.shp")
pinniped_orig <- readRDS("data/habitat_gis/cwhr_ranges_simplified.Rds")

# Blue whale feeding month: June, July, August, September, October, November (BIA2)
# Humpback whale feeding month: March, April, May, June, July, August, September, October, November (BIA2)
# Gray whale (Pacific feeding group) feeding month: June, July, August, September, October, November (BIA 2)
# The CWHR pinniped distribution is year-round
# Sealion: Primarily breed in California but their distribution seems throughout the entire U.S.West coast
# Northern elephant seal: Primarily breed in California in the winter months (December, January, February, March)
#(should filter to winter months?)
# California harbor seal: Nothing to notice, they're California stock and non-migratory

# convert sst_df back to raster
# convert dataframe to SpatRaster
# unique months
dates <- sort(unique(sst_df$date))

# create raster for each month
rast_list <- lapply(dates, function(d) {
  
  tmp <- sst_df %>%
    filter(date == d) %>%
    select(lon, lat, sst)
  
  r <- rast(tmp, type = "xyz", crs = "EPSG:4326")
  names(r) <- as.character(d)
  
  r
})

# stack into one SpatRaster
sst_rast <- rast(rast_list)


# set up habitat for each species

# Blue whale
blue_orig <- cetacean_orig %>%
  filter(CommonName == "Blue whale") %>%
  filter(BIA_Status == "PARENT")

# Humpback whale (have to filter to CA/OR states as WA/SBC use different feeding ground)
humpback_orig <- cetacean_orig %>%
  filter(CommonName == "Humpback whale") %>%
  filter(BIA_Status == "PARENT")

oregon <- states %>% filter(name == "Oregon")
washington <- states %>% filter(name == "Washington")

# shared boundary line between Oregon and Washington
or_wa_line <- st_intersection(
  st_boundary(oregon),
  st_boundary(washington)
)

border_lat <- max(st_coordinates(or_wa_line)[, 2])

# get bbox of humpback habitat
bb <- st_bbox(humpback_orig)

cut_line <- st_sfc(
  st_linestring(matrix(
    c(bb["xmin"], border_lat,
      bb["xmax"], border_lat),
    ncol = 2, byrow = TRUE
  )),
  crs = 4326
)

humpback_split <- lwgeom::st_split(humpback_orig, cut_line) %>%
  st_collection_extract("POLYGON") %>%
  st_as_sf()

# assign each piece to north/south based on centroid latitude
humpback_split2 <- humpback_split %>%
  mutate(
    centroid = st_centroid(geometry),
    centroid_lat = st_coordinates(centroid)[, 2],
    region = ifelse(centroid_lat >= border_lat, "Washington", "California/Oregon")
  ) %>%
  select(region, geometry)

# dissolve fragments within each side
humpback_final <- humpback_split2 %>%
  group_by(region) %>%
  summarise(do_union = TRUE)

# check humpback whale
ggplot() +
  geom_sf(data = humpback_final, aes (color = region), fill = NA) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  #geom_sf(data = cut_line, color = "red", linewidth = 1) +
  coord_sf(
    xlim = c(-130, -110),
    ylim = c(20, 55),
    expand = FALSE
  ) +
  theme_bw()

# California sea lion
ca_sealion_orig <- pinniped_orig %>%
  filter(comm_name == "California sea lion")

# Harbor seal (CA)
ca_harbor_seal_orig <- pinniped_orig %>%
  filter(comm_name == "Harbor seal")

# Northern elephant seal
northern_elephant_seal_orig <- pinniped_orig %>%
  filter(comm_name == "Northern elephant seal")


#########################################################

# Examine habitat and raster overlap

sst_month <- sst_df %>%
  filter(date == as.Date("1850-01-01"))

sst_rast_month <- sst_rast <- rast(
  sst_month[, c("lon", "lat", "sst")],
  type = "xyz",
  crs = "EPSG:4326"
)

ggplot() +
  tidyterra::geom_spatraster(data = sst_rast_month) +
  # geom_raster(data = sst_month,
  #             aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = mexico, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = humpback_split2 %>% filter(region == "California/Oregon"),
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.3,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-126, -110),
    ylim = c(30,48),
    expand = FALSE
  ) +
  scale_fill_viridis_c(name = "SST (°C)") +
  theme_bw()

#####################################################################

# cut the raster to the distribution range

# Blue whale (feeding)
blue_vect <- vect(blue_orig)

sst_blue <- sst_rast %>%
  crop(blue_vect) %>%
  mask(blue_vect)

sst_blue_df <- as.data.frame(sst_blue, xy = TRUE, na.rm = TRUE) %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "date",
    values_to = "sst"
  ) %>%
  mutate(date = as.Date(date)) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  # filter to feeding month
  filter(month %in% c(6, 7, 8, 9, 10, 11)) %>%
  # derive annual grid mean
  group_by(x, y, year) %>%
  mutate(annual_grid_mean = mean(sst)) %>%
  ungroup() %>%
  # derive yearly mean
  group_by(year) %>%
  summarize(annual_sst_mean = mean(annual_grid_mean)) %>%
  mutate(species = "Blue_whale")

# Humpback whale feeding
humpback_vect <- vect(humpback_split2 %>% filter(region == "California/Oregon"))

sst_humpback <- sst_rast %>%
  crop(humpback_vect) %>%
  mask(humpback_vect)

sst_humpback_df <- as.data.frame(sst_humpback, xy = TRUE, na.rm = TRUE) %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "date",
    values_to = "sst"
  ) %>%
  mutate(date = as.Date(date)) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  # filter to feeding month
  filter(month %in% c(3,4,5,6,7,8,9,10,11)) %>%
  # derive annual grid mean
  group_by(x, y, year) %>%
  mutate(annual_grid_mean = mean(sst)) %>%
  ungroup() %>%
  # derive yearly mean
  group_by(year) %>%
  summarize(annual_sst_mean = mean(annual_grid_mean)) %>%
  mutate(species = "Humpback_whale")

# California sea lion

ca_sealion_vect <-  vect(ca_sealion_orig)

sst_ca_sealion <- sst_rast %>%
  crop(ca_sealion_vect) %>%
  mask(ca_sealion_vect)

sst_ca_sealion_df <- as.data.frame(sst_ca_sealion, xy = TRUE, na.rm = TRUE) %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "date",
    values_to = "sst"
  ) %>%
  mutate(date = as.Date(date)) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  # derive annual grid mean
  group_by(x, y, year) %>%
  mutate(annual_grid_mean = mean(sst)) %>%
  ungroup() %>%
  # derive yearly mean
  group_by(year) %>%
  summarize(annual_sst_mean = mean(annual_grid_mean)) %>%
  mutate(species = "California_sea_lion")

# Harbor seal
ca_harbor_seal_vect <-  vect(ca_harbor_seal_orig)

sst_ca_harbor_seal <- sst_rast %>%
  crop(ca_harbor_seal_vect) %>%
  mask(ca_harbor_seal_vect)

sst_ca_harbor_seal_df <- as.data.frame(sst_ca_harbor_seal, xy = TRUE, na.rm = TRUE) %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "date",
    values_to = "sst"
  ) %>%
  mutate(date = as.Date(date)) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  # derive annual grid mean
  group_by(x, y, year) %>%
  mutate(annual_grid_mean = mean(sst)) %>%
  ungroup() %>%
  # derive yearly mean
  group_by(year) %>%
  summarize(annual_sst_mean = mean(annual_grid_mean)) %>%
  mutate(species = "California_harbor_seal")

# Northern elephant seal

northern_elephant_seal_vect <-  vect(northern_elephant_seal_orig)

sst_northern_elephant_seal <- sst_rast %>%
  crop(northern_elephant_seal_vect) %>%
  mask(northern_elephant_seal_vect)

sst_northern_elephant_seal_df <- as.data.frame(sst_northern_elephant_seal, xy = TRUE, na.rm = TRUE) %>%
  pivot_longer(
    cols = -c(x, y),
    names_to = "date",
    values_to = "sst"
  ) %>%
  mutate(date = as.Date(date)) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  # derive annual grid mean
  group_by(x, y, year) %>%
  mutate(annual_grid_mean = mean(sst)) %>%
  ungroup() %>%
  # derive yearly mean
  group_by(year) %>%
  summarize(annual_sst_mean = mean(annual_grid_mean)) %>%
  mutate(species = "Northern_elephant_seal")

sst_total <- bind_rows(sst_blue_df, sst_humpback_df, sst_ca_sealion_df, sst_ca_harbor_seal_df, sst_northern_elephant_seal_df)


# save the sst_rast object
saveRDS(sst_rast, "data/habitat_gis/sst_rast.rds")

write.csv(sst_total, "data/habitat_gis/sst_total_by_species.csv", row.names = FALSE)

#####################################################################################

# Check the temperature data

temp_orig <- read.csv("data/habitat_gis/sst_total_by_species.csv")
pdo <- read.csv("data/habitat_gis/PDO_temp.csv")

temp_orig_scaled <- temp_orig %>%
  filter(year < 2026) %>%
  group_by(species) %>%
  mutate(temp_scaled = as.numeric(scale(annual_sst_mean, center = TRUE, scale = TRUE))) %>%
  ungroup() %>%
  mutate(temp_state = ifelse(temp_scaled > 0, "warm", "cold"))

pdo_scaled <- pdo %>% 
  select(Year, average) %>%
  filter(Year < 2025) %>%
  rename(
    year = Year,
    temp_raw = average
  ) %>%
  mutate(temp_raw = as.numeric(temp_raw)) %>%
  mutate(
    temp_scaled = as.numeric(scale(temp_raw, center = TRUE, scale = TRUE))
  ) %>%
  mutate(temp_state = ifelse(temp_scaled > 0, "warm", "cold"))

temp_habitat <- ggplot(temp_orig_scaled, aes(x = year, y = temp_scaled)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.5, color = "grey") +
  geom_point(aes(color = temp_state), size = 0.5) +
  scale_color_manual(
    values = c("warm" = "red", "cold" = "blue"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(1850, 2025, by = 50)) +
  facet_wrap(.~species) +
  theme_bw()

temp_pdo <- ggplot(pdo_clean, aes(x = year, y = temp_scaled)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.5, color = "grey") +
  geom_point(aes(color = temp_state), size = 0.5) +
  scale_color_manual(
    values = c("warm" = "red", "cold" = "blue"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(1854, 2024, by = 50)) +
  theme_bw()

# save figure

plot_dir <- "figures"

ggsave(temp_habitat, filename=file.path(plot_dir, "temp_habitat_scaled.png"), 
       width=5, height=4, units="in", dpi=600)

ggsave(temp_pdo, filename=file.path(plot_dir, "temp_pdo_scaled.png"), 
       width=5, height=4, units="in", dpi=600)

# save the scaled temp data
write.csv(temp_orig_scaled, "data/habitat_gis/sst_total_by_species_scaled.csv")
write.csv(pdo_clean, "data/habitat_gis/pdo_scaled.csv")



