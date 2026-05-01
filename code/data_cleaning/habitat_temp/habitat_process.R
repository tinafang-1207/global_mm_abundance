
### clean working environment ###
rm(list = ls())

### read in packages ###
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)

### read in data ###
sst_wc <- readRDS("data/habitat_gis/clean/sst_cobe2_us_west_coast.rds")
sst_gray <- readRDS("data/habitat_gis/clean/sst_cobe2_gray_whale_arctic.rds")
sst_rast <- readRDS("data/habitat_gis/clean/sst_rast.rds")
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")
usa_coastline <- read_sf("data/habitat_gis/raw/tl_2025_us_coastline/tl_2025_us_coastline.shp")

# cetacean habitat shapefile
bia_wc <- st_read("data/habitat_gis/raw/BIA_II/WC_WGS84.shp")
bia_arc <- st_read("data/habitat_gis/raw/BIA_II/ARC_WGS84.shp")
bia_abs <- st_read("data/habitat_gis/raw/BIA_II/ABS_WGS84.shp")
cetacean_orig <- bind_rows(bia_wc, bia_arc, bia_abs)
pinniped_orig <- readRDS("data/habitat_gis/raw/cwhr_ranges_simplified.Rds")
sea_otter_orig <- st_read("data/habitat_gis//raw/RangeExtentOfSouthernSeaOtter2018/RangeExtentOfSouthernSeaOtter2018.shp")

# Blue whale feeding month: June, July, August, September, October, November (BIA2)
# Humpback whale feeding month: March, April, May, June, July, August, September, October, November (BIA2)
# Gray whale feeding month: June, July, August, September, October, November (BIA 2)
# The CWHR pinniped distribution is year-round
# Sealion: Primarily breed in California but their distribution seems throughout the entire U.S.West coast
# Northern elephant seal: Primarily breed in California in the winter months (December, January, February, March)
# California harbor seal: Nothing to notice, they're California stock and non-migratory
# Southern sea otter: Nothing to notice, they're California stock and non-migratory

# set up habitat for each species

##############################################################
# Blue whale
blue_orig <- cetacean_orig %>%
  filter(Region_Cod == "WC") %>%
  filter(CommonName == "Blue whale") %>%
  filter(BIA_Status == "PARENT")

###############################################################
# Humpback whale (have to filter to CA/OR states as WA/SBC use different feeding ground)
humpback_orig <- cetacean_orig %>%
  filter(Region_Cod == "WC") %>%
  filter(CommonName == "Humpback whale") %>%
  filter(BIA_Status == "PARENT")

oregon <- usa %>% filter(name == "Oregon")
washington <- usa %>% filter(name == "Washington")

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

######################################################
# ENP gray whale

# 1. Filter gray whale feeding grounds
gray_orig <- cetacean_orig %>%
  filter(CommonName == "Gray whale",
         Area_Type == "Feeding",
         Region != "West Coast") %>%
  filter(!st_is_empty(.)) %>%
  st_make_valid()

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(4326)

# Manually define bbox around both feeding areas,
# ending near St. Lawrence Island region
# gray_bbox_sf <- st_as_sfc(st_bbox(c(
#   xmin = -173.5,
#   xmax = -165.0,
#   ymin = 62.7,
#   ymax = 69.0
# ), crs = 4326)) %>%
#   st_sf()

# gray whale feeding ground bounding box from Stewart et al., 2023
gray_bbox_sf <- st_as_sfc(st_bbox(c(
  xmin = -171.5,
  xmax = -165.5,
  ymin = 63.3,
  ymax = 68.4
), crs = 4326)) %>%
  st_sf()


# check the bounding box and gray whale feeding ground
sst_gray_month <- sst_gray %>%
  filter(date == "2014-06-01")

# Plot
ggplot() +
  geom_raster(data = sst_gray_month, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = land, fill = "gray85", color = "white") +
  geom_sf(data = gray_orig, fill = "orange", color = "black", alpha = 0.6) +
  geom_sf(data = gray_bbox_sf, fill = NA, color = "black", linewidth = 0.8) +
  coord_sf(
    xlim = c(-175, -155),
    ylim = c(62, 71),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(
    title = "Gray Whale Feeding Grounds with Bounding Box",
    x = "Longitude",
    y = "Latitude"
  )

  
######################################################  
# California sea lion
ca_sealion_orig <- pinniped_orig %>%
  filter(comm_name == "California sea lion")

######################################################
# Harbor seal (CA)
ca_harbor_seal_orig <- pinniped_orig %>%
  filter(comm_name == "Harbor seal")

#####################################################
# Northern elephant seal
northern_elephant_seal_orig <- pinniped_orig %>%
  filter(comm_name == "Northern elephant seal")

####################################################
# Harbor seal (OR)

# make sure it is in lon/lat
usa_coastline <- st_transform(usa_coastline, 4326)

# define West Coast bounding box
west_bbox <- st_bbox(c(
  xmin = -130,
  xmax = -110,
  ymin = 30,
  ymax = 50
), crs = st_crs(4326))

west_bbox_sf <- st_as_sfc(west_bbox)

# keep only coastline features that intersect the box
usa_west_coast <- usa_coastline %>%
  st_intersection(west_bbox_sf)

# Get the oregon boundary 
or_bbox <- st_bbox(oregon)
ymin_or <- unname(or_bbox["ymin"])   # Oregon-California border latitude
ymax_or <-unname(or_bbox["ymax"])

oregon_coast_box <- st_as_sfc(st_bbox(c(
  xmin = -130,
  xmax = -120,
  ymin = ymin_or,
  ymax = ymax_or
), crs = st_crs(4326)))

oregon_coast <- usa_west_coast %>%
  st_intersection(oregon_coast_box) %>%
  st_union() %>%
  st_transform(crs = 5070)

# buffer 31 miles
oregon_buffer_raw <- st_buffer(oregon_coast, dist = 31 * 1609.34)

# convert back to lon/lat for plotting
oregon_buffer_raw <- st_transform(oregon_buffer_raw, 4326)

# 2. Remove land portion of buffer
oregon_buffer_ocean <- st_difference(
  st_make_valid(oregon_buffer_raw),
  st_make_valid(oregon)
) %>%
  st_intersection(oregon_coast_box)

# make sure both are in lon/lat
oregon_coast <- st_transform(oregon_coast, 4326)
oregon_buffer_ocean <- st_transform(oregon_buffer_ocean, 4326)

# get Oregon coastline x/y extent
coast_bbox <- st_bbox(oregon_coast)
buf_bbox   <- st_bbox(oregon_buffer_ocean)

# build a box that keeps only the west side of the coastline
xmin_buf <- as.numeric(buf_bbox["xmin"])
xmax_buf <- as.numeric(buf_bbox["xmax"])
ymin_buf <- as.numeric(buf_bbox["ymin"])
ymax_buf <- as.numeric(buf_bbox["ymax"])

xmax_coast <- as.numeric(coast_bbox["xmax"])

# build a west-side rectangle manually
west_box <- st_polygon(list(matrix(
  c(
    xmin_buf, ymin_buf,
    xmax_coast, ymin_buf,
    xmax_coast, ymax_buf,
    xmin_buf, ymax_buf,
    xmin_buf, ymin_buf
  ),
  ncol = 2,
  byrow = TRUE
))) %>%
  st_sfc(crs = st_crs(oregon_buffer_ocean)) %>%
  st_as_sf()

# keep only the left/west side of the buffer
oregon_buffer_pacific <- st_intersection(
  st_make_valid(oregon_buffer_ocean),
  st_make_valid(west_box)
)

ggplot() +
  #geom_sf(data = oregon, fill = "grey90", color = "black", linewidth = 0.3) +
  geom_sf(data = oregon_buffer_pacific, fill = "lightblue") +
  geom_sf(data = oregon_coast, color = "blue", linewidth = 0.5) +
  coord_sf(
    xlim = c(-125.5, -121),
    ylim = c(41, 47),
    expand = FALSE
  ) +
  theme_bw() +
  labs(
    title = "Oregon Coastline",
    x = "Longitude",
    y = "Latitude"
  )

#####################################################
# Southern sea otter

# Buffer the original distribution to overlap with raster

sea_otter_buffer <- sea_otter_orig %>%
  st_buffer(dist = 25000) %>%   # 25,000 m = 25 km
  st_make_valid()

sea_otter_buffer_transfer <- st_transform(sea_otter_buffer, crs(sst_rast))

california <- usa %>% filter(name == "California") %>%
  st_transform(st_crs(sea_otter_buffer))

# Keep only nearby states/land
california_nearby <- st_crop(california, st_bbox(sea_otter_buffer))

# Union California land geometry
california_land <- california_nearby %>%
  st_make_valid() %>%
  st_union()

# Subtract California land from sea otter buffer
sea_otter_buffer_ocean <- st_difference(
  st_make_valid(sea_otter_buffer),
  california_land
)

# Transform to SST raster CRS
sea_otter_buffer_transfer <- st_transform(
  sea_otter_buffer_ocean,
  crs(sst_rast)
)

# Check sea otter distribution and buffer

sst_month <- sst_wc %>%
  filter(date == as.Date("2014-09-01"))

ggplot() +
  geom_raster(data = sst_month, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = mexico, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = sea_otter_orig,
          color = "orange",fill = NA, linewidth = 1,
          inherit.aes = FALSE) +
  geom_sf(data = sea_otter_buffer_transfer,
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.2,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-123, -120),
    ylim = c(34.2, 38),
    expand = FALSE
  )

#########################################################################

# create a dataframe that contains the habitat information of all species

blue_whale_geom <- blue_orig %>%
  transmute(species = "Blue whale") %>%
  mutate(season = "Feeding ground (6,7,8,9,10,11)")

humpback_whale_geom <- humpback_final %>%
  filter(region == "California/Oregon") %>%
  transmute(species = "CA/OR Humpback whale") %>%
  mutate(season = "Feeding ground (3,4,5,6,7,8,9,10,11)")

gray_whale_geom <- gray_bbox_sf %>%
  transmute(species = "ENP gray whale") %>%
  mutate(season = "Feeding ground (6,7,8,9,10,11)")

ca_sealion_geom <- ca_sealion_orig %>%
  transmute(species = "California sea lion") %>%
  mutate(season = "Year-round (California)")

ca_harbor_seal_geom <- ca_harbor_seal_orig %>% 
  transmute(species = "CA harbor seal") %>%
  mutate(season = "Year-round (California)")

northern_elephant_seal_geom <- northern_elephant_seal_orig %>%
  transmute(species = "Northern elephant seal") %>%
  mutate(season = "Year-round (California)")

or_harbor_seal_geom <- st_as_sf(oregon_buffer_pacific) %>%
  transmute(species = "OR harbor seal") %>%
  rename(geometry = x) %>%
  mutate(season = "Year-round (Oregon)") 

sea_otter_geom <- st_as_sf(sea_otter_buffer_transfer) %>%
  transmute(species = "Southern sea otter") %>%
  mutate(season = "Year-round (California)")

species_habitat <- bind_rows(blue_whale_geom,
                             humpback_whale_geom,
                             gray_whale_geom,
                             ca_sealion_geom,
                             ca_harbor_seal_geom,
                             northern_elephant_seal_geom,
                             or_harbor_seal_geom,
                             sea_otter_geom)

saveRDS(species_habitat, "data/habitat_gis/clean/species_habitat_final.rds")

#########################################################

library(tidyverse)
library(sf)

# read in data
species_habitat <- readRDS("data/habitat_gis/clean/species_habitat_final.rds")
sst_wc <- readRDS("data/habitat_gis/clean/sst_cobe2_us_west_coast.rds")
sst_gray <- readRDS("data/habitat_gis/clean/sst_cobe2_gray_whale_arctic.rds")

# Examine habitat and raster overlap

sst_gray_month <- sst_gray %>%
  filter(date == as.Date("2014-09-01"))

sst_month <- sst_wc %>%
  filter(date == as.Date("2014-09-01"))

base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=8),
                    plot.tag =element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

blue_whale <- ggplot() +
  geom_raster(data = sst_month, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = mexico, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = species_habitat %>% filter(species == "Blue whale"),
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.3,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-126, -115),
    ylim = c(32, 44.5),
    expand = FALSE
  ) +
  scale_x_continuous(breaks=seq(-126, -115, 3)) +
  scale_y_continuous(breaks=seq(32, 44.5, 3)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  labs(x = NULL, y = NULL) +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")

blue_whale

humpback_whale <- ggplot() +
  geom_raster(data = sst_month, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = mexico, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = species_habitat %>% filter(species == "CA/OR Humpback whale"),
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.3,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-126, -115),
    ylim = c(32, 47.5),
    expand = FALSE
  ) +
  scale_x_continuous(breaks=seq(-126, -115, 3)) +
  scale_y_continuous(breaks=seq(32, 47.5, 3)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  labs(x = NULL, y = NULL) +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")

humpback_whale

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(4326)
  
gray_whale <- ggplot() +
  geom_raster(data = sst_gray, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = land, fill = "gray85", color = "white") +
  geom_sf(data = gray_orig, fill = "lightgreen", color = "black", alpha = 0.6) +
  geom_sf(data = species_habitat %>% filter(species == "ENP gray whale"),
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.3,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-175, -155),
    ylim = c(62, 71),
    expand = FALSE
  ) +
  scale_x_continuous(breaks=seq(-175, -155, 3)) +
  scale_y_continuous(breaks=seq(62, 71, 3)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  labs(x = NULL, y = NULL) +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")

gray_whale


California_sea_lion <- ggplot() +
  geom_raster(data = sst_month, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = mexico, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = species_habitat %>% filter(species == "California sea lion"),
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.3,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-126, -117),
    ylim = c(32, 43),
    expand = FALSE
  ) +
  scale_x_continuous(breaks=seq(-126, -117, 3)) +
  scale_y_continuous(breaks=seq(32, 43, 3)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  labs(x = NULL, y = NULL) +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")

California_sea_lion

CA_harbor_seal <- ggplot() +
  geom_raster(data = sst_month, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = mexico, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = species_habitat %>% filter(species == "CA harbor seal"),
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.3,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-126, -117),
    ylim = c(32, 43),
    expand = FALSE
  ) +
  scale_x_continuous(breaks=seq(-126, -117, 3)) +
  scale_y_continuous(breaks=seq(32, 43, 3)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  labs(x = NULL, y = NULL) +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")

CA_harbor_seal

Northern_elephant_seal <- ggplot() +
  geom_raster(data = sst_month, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = mexico, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = species_habitat %>% filter(species == "Northern elephant seal"),
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.3,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-126, -117),
    ylim = c(32, 43),
    expand = FALSE
  ) +
  scale_x_continuous(breaks=seq(-126, -117, 3)) +
  scale_y_continuous(breaks=seq(32, 43, 3)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  labs(x = NULL, y = NULL) +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")
Northern_elephant_seal

OR_harbor_seal <- ggplot() +
  geom_raster(data = sst_month, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = species_habitat %>% filter(species == "OR harbor seal"),
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.3,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-126, -120),
    ylim = c(41, 47),
    expand = FALSE
  ) +
  scale_x_continuous(breaks=seq(-126, -120, 3)) +
  scale_y_continuous(breaks=seq(41, 47, 3)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  labs(x = NULL, y = NULL) +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")

OR_harbor_seal

southern_sea_otter <-  ggplot() +
  geom_raster(data = sst_month, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = sea_otter_orig,
          color = "lightgreen",fill = NA, linewidth = 1,
          inherit.aes = FALSE) +
  geom_sf(data = species_habitat %>% filter(species == "Southern sea otter"),
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.3,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-123, -120),
    ylim = c(34.2, 38),
    expand = FALSE
  ) +
  scale_x_continuous(breaks=seq(-123, -120, 1)) +
  scale_y_continuous(breaks=seq(34.2, 38, 1)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  labs(x = NULL, y = NULL) +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")

southern_sea_otter 

g_total <- gridExtra::grid.arrange(blue_whale, humpback_whale, gray_whale, California_sea_lion, Northern_elephant_seal, CA_harbor_seal, OR_harbor_seal, southern_sea_otter, ncol = 3)

g_total

plot_dir <- "figures/species_habitat"

ggsave(blue_whale, filename=file.path(plot_dir, "blue_whale.png"), 
       width=4, height=4, units="in", dpi=600)
ggsave(humpback_whale, filename=file.path(plot_dir, "humpback_whale.png"), 
       width=4, height=4, units="in", dpi=600)
ggsave(gray_whale, filename=file.path(plot_dir, "gray_whale.png"), 
       width=4, height=4, units="in", dpi=600)
ggsave(California_sea_lion, filename=file.path(plot_dir, "california_sea_lion.png"), 
       width=4, height=4, units="in", dpi=600)
ggsave(CA_harbor_seal, filename=file.path(plot_dir, "CA_harbor_seal.png"), 
       width=4, height=4, units="in", dpi=600)
ggsave(Northern_elephant_seal, filename = file.path(plot_dir, "Northern_elephant_seal.png"), 
       width=4, height=4, units="in", dpi=600)
ggsave(OR_harbor_seal, filename = file.path(plot_dir, "OR_harbor_seal.png"), 
       width=4, height=4, units="in", dpi=600)
ggsave(southern_sea_otter, filename = file.path(plot_dir, "southern_sea_otter.png"), 
       width=4, height=4, units="in", dpi=600)











#############################################################################

#convert sst_df back to raster
#convert dataframe to SpatRaster
#unique months
dates <- sort(unique(sst_gray$date))

# create raster for each month
rast_list <- lapply(dates, function(d) {

  tmp <- sst_gray %>%
    filter(date == d) %>%
    select(lon, lat, sst)

  r <- rast(tmp, type = "xyz", crs = "EPSG:4326")
  names(r) <- as.character(d)

  r
})

# stack into one SpatRaster
sst_rast_wc <- rast(rast_list)
sst_rast_gray <- rast(rast_list)

saveRDS(sst_rast_wc, "data/habitat_gis/clean/sst_rast_wc.rds")
saveRDS(sst_rast_gray, "data/habitat_gis/clean/sst_rast_gray.rds")







