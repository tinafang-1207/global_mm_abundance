
### clean working environment ###
rm(list = ls())

### read in packages ###
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(tidyterra)
library(tidyverse)

### read in data 
sst_df <- readRDS("data/habitat_gis/clean/sst_cobe2_us_west_coast.rds")
sst_rast <- readRDS("data/habitat_gis/clean/sst_rast.rds")
species_habitat <- readRDS("data/habitat_gis/clean/species_habitat_final.rds")
input_df <- read.csv("data/confidential/input_data/input_final.csv")


# cut the raster to the distribution range

# Blue whale (feeding)
blue_vect <- vect(species_habitat %>% filter (species == "Blue whale"))

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
humpback_vect <- vect(species_habitat %>% filter(species == "CA/OR Humpback whale"))

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

ca_sealion_vect <-  vect(species_habitat %>% filter(species == "California sea lion"))

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
ca_harbor_seal_vect <-  vect(species_habitat %>% filter(species == "CA harbor seal"))

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
  mutate(species = "CA_harbor_seal")

# Northern elephant seal

northern_elephant_seal_vect <-  vect(species_habitat %>% filter(species == "Northern elephant seal"))

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

# OR Harbor seal

or_harbor_seal_vect <-  vect(species_habitat %>% filter(species == "OR harbor seal"))

sst_or_harbor_seal <- sst_rast %>%
  crop(or_harbor_seal_vect ) %>%
  mask(or_harbor_seal_vect )

sst_or_harbor_seal_df <- as.data.frame(sst_or_harbor_seal, xy = TRUE, na.rm = TRUE) %>%
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
  mutate(species = "OR_harbor_seal")




sst_total <- bind_rows(sst_blue_df, 
                       sst_humpback_df, 
                       sst_ca_sealion_df, 
                       sst_ca_harbor_seal_df, 
                       sst_northern_elephant_seal_df, 
                       sst_or_harbor_seal_df)



#####################################################################################

# Check the temperature data

species <- input_df %>% distinct(species)
temp_orig <- sst_total
pdo <- read.csv("data/habitat_gis/clean/PDO_temp.csv")

# scaled temp data by all years

temp_orig_scaled <- temp_orig %>%
  filter(year < 2026) %>%
  group_by(species) %>%
  mutate(temp_scaled_all_year = as.numeric(scale(annual_sst_mean, center = TRUE, scale = TRUE))) %>%
  ungroup() %>%
  mutate(temp_state = ifelse(temp_scaled_all_year > 0, "warm", "cold"))

pdo_scaled <- pdo %>% 
  select(Year, average) %>%
  filter(Year < 2025) %>%
  rename(
    year = Year,
    temp_raw = average
  ) %>%
  mutate(temp_raw = as.numeric(temp_raw)) %>%
  # 🔑 replicate rows for each species
  crossing(species) %>%
  mutate(
    pdo_scaled_all_year = as.numeric(scale(temp_raw, center = TRUE, scale = TRUE))
  ) %>%
  mutate(temp_state = ifelse(pdo_scaled_all_year > 0, "warm", "cold"))


# scaled temp data by years with abundance ONLY

year_abd <- input_df %>%
  filter(abundance != -999) %>%   # adjust if you use -999 instead
  select(species, year)

temp_flag <- temp_orig %>%
  left_join(year_abd %>% mutate(has_abundance = TRUE),
            by = c("species", "year")) %>%
  mutate(has_abundance = ifelse(is.na(has_abundance), FALSE, TRUE))

pdo_flag <- pdo_scaled %>%
  left_join(year_abd %>%  mutate(has_abundance = TRUE),
            by = c("species", "year")) %>%
  mutate(has_abundance = ifelse(is.na(has_abundance), FALSE, TRUE))

# 3. Compute mean and sd ONLY from years with abundance (per species)
sst_stats_habitat <- temp_flag %>%
  filter(has_abundance == "TRUE") %>%
  group_by(species) %>%
  summarise(
    sst_mean = mean(annual_sst_mean, na.rm = TRUE),
    sst_sd   = sd(annual_sst_mean, na.rm = TRUE),
    .groups = "drop"
  )

sst_stats_pdo <- pdo_flag %>%
  filter(has_abundance == "TRUE") %>%
  group_by(species) %>%
  summarise(pdo_mean = mean(temp_raw, na.rm = TRUE),
            pdo_sd = sd(temp_raw, na.rm = TRUE),
            .groups = "drop")

# 4. Apply scaling to ALL SST data using those stats
temp_orig_scaled_final <- temp_orig_scaled %>%
  left_join(sst_stats_habitat, by = "species") %>%
  mutate(
    temp_scaled_abd_year = (annual_sst_mean - sst_mean) / sst_sd
  ) %>%
  select(species, year, annual_sst_mean, temp_scaled_all_year, temp_scaled_abd_year) %>%
  mutate(all_year_state = ifelse(temp_scaled_all_year > 0, "warm", "cold")) %>%
  mutate(abd_year_state = ifelse(temp_scaled_abd_year > 0, "warm", "cold"))

pdo_scaled_final <- pdo_scaled %>%
  left_join(sst_stats_pdo, by = "species") %>%
  mutate(pdo_scaled_abd_year = (temp_raw  - pdo_mean)/pdo_sd) %>%
  select(species, year, temp_raw, pdo_scaled_abd_year, pdo_scaled_all_year) %>%
  mutate(all_year_state = ifelse(pdo_scaled_all_year > 0, "warm", "cold")) %>%
  mutate(abd_year_state = ifelse(pdo_scaled_abd_year > 0, "warm", "cold"))


# plot the temp data

temp_plot_df <- temp_orig_scaled_final %>%
  pivot_longer(
    cols = c(temp_scaled_all_year, temp_scaled_abd_year),
    names_to = "temp_type",
    values_to = "temp_scaled"
  ) %>%
  mutate(
    temp_state = case_when(
      temp_type == "temp_scaled_all_year" ~ all_year_state,
      temp_type == "temp_scaled_abd_year" ~ abd_year_state
    ),
    temp_type = recode(
      temp_type,
      temp_scaled_all_year = "All years",
      temp_scaled_abd_year = "Abundance years"
    )
  )


temp_habitat_all_year <- ggplot(temp_orig_scaled_final, aes(x = year, y = temp_scaled_all_year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.5, color = "grey") +
  geom_point(aes(color = all_year_state), size = 0.5) +
  scale_color_manual(
    values = c("warm" = "red", "cold" = "blue"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(1850, 2025, by = 50)) +
  facet_wrap(.~species, scales = "free_y") +
  theme_bw()

temp_habitat_abd_year <-  ggplot(temp_orig_scaled_final, aes(x = year, y = temp_scaled_abd_year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.5, color = "grey") +
  geom_point(aes(color = abd_year_state), size = 0.5) +
  scale_color_manual(
    values = c("warm" = "red", "cold" = "blue"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(1850, 2025, by = 50)) +
  facet_wrap(.~species, scales = "free_y") +
  theme_bw()

temp_habitat_all_year 

temp_habitat_abd_year 

temp_pdo_all_year <- ggplot(pdo_scaled_final, aes(x = year, y = pdo_scaled_all_year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.5, color = "grey") +
  geom_point(aes(color = all_year_state), size = 0.5) +
  scale_color_manual(
    values = c("warm" = "red", "cold" = "blue"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(1854, 2024, by = 50)) +
  facet_wrap(.~species, scales = "free_y") +
  theme_bw()


temp_pdo_abd_year <- ggplot(pdo_scaled_final, aes(x = year, y = pdo_scaled_abd_year)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.5, color = "grey") +
  geom_point(aes(color = abd_year_state), size = 0.5) +
  scale_color_manual(
    values = c("warm" = "red", "cold" = "blue"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(1854, 2024, by = 50)) +
  facet_wrap(.~species, scales = "free_y") +
  theme_bw()

temp_pdo_all_year 

temp_pdo_abd_year

# save figure

plot_dir <- "figures"

ggsave(temp_habitat_all_year , filename=file.path(plot_dir, "temp_habitat_scaled_all_year.png"), 
       width=5, height=4, units="in", dpi=600)

ggsave(temp_habitat_abd_year, filename=file.path(plot_dir, "temp_habitat_abd_year.png"), 
       width=5, height=4, units="in", dpi=600)


ggsave(temp_pdo_all_year, filename=file.path(plot_dir, "temp_pdo_scaled_all_year.png"), 
       width=5, height=4, units="in", dpi=600)

ggsave(temp_pdo_abd_year, filename=file.path(plot_dir, "temp_pdo_scaled_abd_year.png"), 
       width=5, height=4, units="in", dpi=600)


# save the scaled temp data
write.csv(temp_orig_scaled_final, "data/habitat_gis/clean/sst_total_by_species_scaled.csv")
write.csv(pdo_scaled_final, "data/habitat_gis/clean/pdo_scaled.csv")


