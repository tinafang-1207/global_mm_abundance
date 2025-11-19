
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


summary_trial <- summary %>%
  filter(ocean == "North Pacific") %>%
  filter(area %in% c("Br.Columbia", "USA W coast", "Mexico")) %>%
  filter(operation_type == "Commercial") %>%
  group_by(year) %>%
  summarize(hbk_total = sum(hbk))


# world map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country ="Mexico", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country = "Canada", returnclass = "sf")


# identify the tour that potentially catch the interested stocks
# 3 breeding grounds: Hawaii, Central America, Mainland Mexico

# No catch found around Hawaii and Central America
# How to judge the catch_area to be filtered with?
# The catch areas filtered should be: USA West coast, Mexico, Br.Columbia
# These areas likely contain the catch of humpback whale belongs to CA/OR/WA stock

summary_clean <- summary%>%
  filter(area %in% c("USA W coast", "Br.Columbia", "Mexico")) %>%
  filter(operation_type == "Commercial") %>%
  select(expedition_iwc, expedition_new, area) %>%
  rename(exp_id_sum = expedition_iwc)


# join with individual catch database

ind_filtered <- individual %>%
  mutate(expedition_new = paste(exp_id_sum, catch_year, sep = "-")) %>%
  select(-exp_id_ind) %>%
  select(expedition_new, exp_id_sum, everything()) %>%
  inner_join(summary_clean, by = c("exp_id_sum", "expedition_new")) %>%
  filter(species == "Humpback whale") %>%
  # the whales were typically seen-off west coast from April through November (when they're feeding)
  mutate(catching_season = ifelse(catch_month %in% c(4,5,6,7,8,9,10,11), "Feeding", "Breeding"))

ind_feeding <- ind_filtered %>%
  mutate(exp_id_sum = as.factor(exp_id_sum)) %>%
  filter(catching_season == "Feeding") %>%
  #filter(area == "USA W coast"|area == "Br.Columbia") %>%
  # only keep the catch location with latitude less than 49N - keep it consistent with the survey region
  filter(lat_dd <= 50) %>%
  group_by(catch_year, exp_id_sum) %>%
  summarize(total_catch = n(), lat_dd = first(lat_dd), long_dd = first(long_dd)) %>%
  ungroup()


ind_breeding <- ind_filtered %>%
  mutate(exp_id_sum = as.factor(exp_id_sum)) %>%
  filter(catching_season == "Breeding") %>%
  filter(lat_dd <= 50) %>%
  # should the US West coast be kept?
  # filter(area == "USA W coast"|area == "Mexico") %>%
  group_by(catch_year, exp_id_sum, area) %>%
  summarize(total_catch = n(), lat_dd = first(lat_dd), long_dd = first(long_dd)) %>%
  ungroup()

# There were whales caught in Br.Columbia during breeding season and whale caught in Mexico during feeding season

# Compile the total catch data
ind_feeding_sum <- ind_filtered %>%
  mutate(exp_id_sum = as.factor(exp_id_sum)) %>%
  filter(catching_season == "Feeding") %>%
  filter(lat_dd <= 50) %>%
  group_by(catch_year) %>%
  summarize(total_catch = n())

ind_breeding_sum <- ind_filtered%>%
  mutate(exp_id_sum = as.factor(exp_id_sum)) %>%
  filter(catching_season == "Breeding") %>%
  filter(lat_dd <= 50) %>%
  group_by(catch_year) %>%
  summarize(total_catch = n())

catch_humpback <- rbind(ind_feeding_sum, ind_breeding_sum) %>%
  group_by(catch_year) %>%
  summarize(catch = sum(total_catch))
  

################################################################# 
# Examine catch data

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

g_feeding <- ggplot(ind_feeding, aes(x = long_dd, y = lat_dd)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = canada, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_point(aes(size = total_catch, color = exp_id_sum), alpha = 0.7) +
  coord_sf(xlim = c(-130, -105), ylim = c(20, 50)) +
  scale_x_continuous(breaks=seq(-130, -110, 5)) +
  scale_y_continuous(breaks=seq(20, 50, 5)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ catch_year, ncol = 10) +
  guides(color = "none") + 
  base_theme + theme(legend.background = element_blank(),
                     legend.key = element_blank())

g_feeding


g_breeding <- ggplot(ind_breeding, aes(x = long_dd, y = lat_dd)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = canada, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_point(aes(size = total_catch, color = exp_id_sum), alpha = 0.7) +
  coord_sf(xlim = c(-130, -105), ylim = c(20, 49)) +
  scale_x_continuous(breaks=seq(-130, -110, 5)) +
  scale_y_continuous(breaks=seq(20, 49, 5)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ catch_year, ncol = 5) +
  guides(color = "none") + 
  base_theme + theme(legend.background = element_blank(),
                     legend.key = element_blank())

g_breeding

# save figure
plotdir <- "data/confidential/iwc_catch_figure"

ggsave(g_feeding, filename=file.path(plotdir, "hbk_feeding.png"), 
              width=8, height=5, units="in", dpi=600)

ggsave(g_breeding, filename=file.path(plotdir, "hbk_breeding.png"), 
       width=8, height=5, units="in", dpi=600)

write.csv(catch_humpback, "data/confidential/input_data/wc_humpback_catch.csv", row.names = FALSE)








