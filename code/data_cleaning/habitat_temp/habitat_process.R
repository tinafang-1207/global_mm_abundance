
### clean working environment ###
rm(list = ls())

### read in packages ###
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(tidyterra)

### read in data ###
sst_df <- readRDS("data/habitat_gis/clean/sst_cobe2_us_west_coast.rds")
sst_rast <- readRDS("data/habitat_gis/clean/sst_rast.rds")
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country = "Mexico", returnclass = "sf")
states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  st_transform(4326)
usa_coastline <- read_sf("data/habitat_gis/raw/tl_2025_us_coastline/tl_2025_us_coastline.shp")

# cetacean habitat shapefile
cetacean_orig <- st_read("data/habitat_gis/raw/BIA_II/WC_WGS84.shp")
pinniped_orig <- readRDS("data/habitat_gis/raw/cwhr_ranges_simplified.Rds")

# Blue whale feeding month: June, July, August, September, October, November (BIA2)
# Humpback whale feeding month: March, April, May, June, July, August, September, October, November (BIA2)
# Gray whale (Pacific feeding group) feeding month: June, July, August, September, October, November (BIA 2)
# The CWHR pinniped distribution is year-round
# Sealion: Primarily breed in California but their distribution seems throughout the entire U.S.West coast
# Northern elephant seal: Primarily breed in California in the winter months (December, January, February, March)
# California harbor seal: Nothing to notice, they're California stock and non-migratory

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

# ENP gray whale
gray_orig <- 


# California sea lion
ca_sealion_orig <- pinniped_orig %>%
  filter(comm_name == "California sea lion")

# Harbor seal (CA)
ca_harbor_seal_orig <- pinniped_orig %>%
  filter(comm_name == "Harbor seal")

# Northern elephant seal
northern_elephant_seal_orig <- pinniped_orig %>%
  filter(comm_name == "Northern elephant seal")

################################################# Harbor seal (OR)

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

#########################################################################

# create a dataframe that contains the habitat information of all species

blue_whale_geom <- blue_orig %>%
  transmute(species = "Blue whale") %>%
  mutate(season = "Feeding ground (6,7,8,9,10,11)")

humpback_whale_geom <- humpback_final %>%
  filter(region == "California/Oregon") %>%
  transmute(species = "CA/OR Humpback whale") %>%
  mutate(season = "Feeding ground (3,4,5,6,7,8,9,10,11)")

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

species_habitat <- bind_rows(blue_whale_geom,
                             humpback_whale_geom,
                             ca_sealion_geom,
                             ca_harbor_seal_geom,
                             northern_elephant_seal_geom,
                             or_harbor_seal_geom)

saveRDS(species_habitat, "data/habitat_gis/clean/species_habitat_final.rds")

#########################################################

# read in data
species_habitat <- readRDS("data/habitat_gis/clean/species_habitat_final.rds")
sst_df <- readRDS("data/habitat_gis/clean/sst_cobe2_us_west_coast.rds")

# Examine habitat and raster overlap

sst_month <- sst_df %>%
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
  scale_x_continuous(breaks=seq(-126, -115, 2)) +
  scale_y_continuous(breaks=seq(32, 44.5, 2)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
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
  scale_x_continuous(breaks=seq(-126, -115, 2)) +
  scale_y_continuous(breaks=seq(32, 47.5, 2)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")

humpback_whale

California_sea_lion <- humpback_whale <- ggplot() +
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
  scale_x_continuous(breaks=seq(-126, -117, 2)) +
  scale_y_continuous(breaks=seq(32, 43, 2)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
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
  scale_x_continuous(breaks=seq(-126, -117, 2)) +
  scale_y_continuous(breaks=seq(32, 43, 2)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
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
  scale_x_continuous(breaks=seq(-126, -117, 2)) +
  scale_y_continuous(breaks=seq(32, 43, 2)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")
Northern_elephant_seal

OR_harbor_seal <- ggplot() +
  geom_raster(data = sst_month, aes(x = lon, y = lat, fill = sst)) +
  geom_sf(data = usa, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = mexico, fill = "grey85", col = "white",
          linewidth = 0.01, inherit.aes = FALSE) +
  geom_sf(data = species_habitat %>% filter(species == "OR harbor seal"),
          color = "red",fill = NA,alpha = 0.4, linewidth = 0.3,
          inherit.aes = FALSE) +
  coord_sf(
    xlim = c(-126, -120),
    ylim = c(41, 47),
    expand = FALSE
  ) +
  scale_x_continuous(breaks=seq(-126, -120, 2)) +
  scale_y_continuous(breaks=seq(41, 47, 2)) +
  facet_wrap(.~species) +
  scale_fill_viridis_c(name = "SST (°C)") +
  theme_bw() + base_theme + theme(legend.position= c(0.75, 0.7),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "vertical")

OR_harbor_seal

g_total <- gridExtra::grid.arrange(blue_whale, humpback_whale, California_sea_lion, Northern_elephant_seal, CA_harbor_seal, OR_harbor_seal, ncol = 3)

g_total


plot_dir <- "figures"

ggsave(g_total, filename=file.path(plot_dir, "species_habitat.png"), 
       width=8, height=6, units="in", dpi=600)



#############################################################################

# convert sst_df back to raster
# convert dataframe to SpatRaster
# unique months
# dates <- sort(unique(sst_df$date))
# 
# # create raster for each month
# rast_list <- lapply(dates, function(d) {
#   
#   tmp <- sst_df %>%
#     filter(date == d) %>%
#     select(lon, lat, sst)
#   
#   r <- rast(tmp, type = "xyz", crs = "EPSG:4326")
#   names(r) <- as.character(d)
#   
#   r
# })
# 
# # stack into one SpatRaster
# sst_rast <- rast(rast_list)


