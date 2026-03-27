
### Clean working Environment ###
rm(list = ls())

### read in package ###
library(tidyverse)
library(sf)

### read in cetacean habitat info (BIA2) ###
cetacean_orig <- st_read("data/habitat_gis/BIA_II/WC_WGS84.shp")
harbor_seal_orig <- readRDS("data/habitat_gis/2001_2003_harbor_seal_haulouts.Rds")
ranges <- readRDS("data/habitat_gis/cwhr_ranges_simplified.Rds")

  
### read country information ###
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")


### examine habitat (cetacean)
ggplot() +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = cetacean_orig, aes(fill = Area_Type, color = BIA_Status), alpha = 0.4, linewidth = 0.3) +
  coord_sf(xlim = c(-126, -118), ylim = c(32, 46)) +
  scale_x_continuous(breaks=seq(-128, -118, 2)) +
  scale_y_continuous(breaks=seq(32, 46, 2)) +
  facet_wrap(~`CommonName`) +
  theme_bw()



ggplot(ranges %>% filter(comm_name=="Harbor seal")) +
  facet_wrap(~comm_name) +
  geom_sf(fill="lightblue", color=NA) +
  # Plot reference lines
  # geom_hline(yintercept=lats,
  #            linetype="dotted", linewidth=0.3, color="grey50") +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # Haulouts
  geom_sf(data=harbor_seal_orig %>% filter(count_photo>0), mapping=aes(size=count_photo)) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Legend
  scale_size_continuous(name="Haulout size\n(# of seals)", range=c(0.2, 3)) +
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + 
  theme(legend.key.size=unit(0.2, "cm"),
        legend.position = c(0.2, 0.2))
  
  