
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)
library(sf)

# set up plot directory
plotdir <- "data/confidential/iwc_catch_figure"

### read in data ###

# IWC individual catch data
individual <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/processed/individual_NP_catch_clean.Rds")

# IWC summary catch data
summary <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/processed/summary_catch_clean.Rds")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

area <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/iwc_data/processed/catch_area_NP_sf.Rds")

# set the target crs
target_crs <-"+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=180 +datum=WGS84 +units=m +no_defs"

### clean data ###

summary_clean <- summary %>%
  filter(gray != 0) %>%
  select(expedition_iwc, expedition_new, area) %>%
  rename(exp_id_sum = expedition_iwc)

individual_clean <- individual %>%
  mutate(expedition_new = paste(exp_id_sum, catch_year, sep = "-")) %>%
  select(-exp_id_ind) %>%
  select(expedition_new, exp_id_sum, everything()) %>%
  inner_join(summary_clean, by = c("exp_id_sum", "expedition_new")) %>%
  mutate(exp_id_sum = as.factor(exp_id_sum)) %>%
  filter(species == "Gray whale") %>%
  group_by(catch_year, exp_id_sum) %>%
  summarize(total_catch = n(), lat_dd = first(lat_dd), long_dd = first(long_dd), area = first(area)) %>%
  ungroup()

individual_clean_sf <- individual_clean %>%
  st_as_sf(coords = c("long_dd", "lat_dd"), crs = 4326, remove = FALSE)

# transform the world/point data
individual_clean_trans <-st_transform(individual_clean_sf, target_crs)

world_transform <- world %>%
  st_break_antimeridian(lon_0 = 180) %>% 
  st_transform(crs = target_crs) 


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



g_catch <- ggplot() +
  geom_sf(data = world_transform, fill = "grey85", col = "white", linewidth=0.1) +
  geom_sf(data = individual_clean_trans, aes(color = exp_id_sum, size = total_catch), alpha = 0.7) +
  coord_sf(crs = st_crs(world_transform),
           default_crs = 4326,
           xlim = c(150, 260),  
           ylim = c(10, 80),      
           expand = FALSE) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~ catch_year, ncol = 10) +
  guides(color = "none") + 
  base_theme + theme(legend.background = element_blank(),
                     legend.key = element_blank(),
                     legend.position = "none")
g_catch

### summarize total catch ###

catch_gray <- individual_clean %>%
  group_by(catch_year) %>%
  summarize(catch = sum(total_catch))


ggsave(g_catch, filename=file.path(plotdir, "gray_catch.png"), width=12, height=8, units="in", dpi=600)

write.csv(catch_gray, "data/confidential/input_data/ENP_gray_catch.csv", row.names = FALSE)





summary_trial <- summary %>%
  filter(gray!= 0) 

