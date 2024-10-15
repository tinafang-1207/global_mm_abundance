
#### clean working environment #####
rm(list = ls())

#### read in package ####
library(tidyverse)
library(scatterpie)
library(cowplot)
library(sf)


### read in data ###
data <- read.csv("data/clean_data/U.S_Abundance_Clean.csv")

species_key <- read.csv("data/raw_data/species_key.csv", na = "-") %>%
  rename(us_region = `U.S..region`) %>%
  select(-notes)

us_region_coord <- read.csv("data/raw_data/us_region_coordinates.csv")


# read in u.s shapefile
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")

usa <- usa %>%
  mutate(fishery_region = case_when(name %in% c("Connecticut",
                                                 "Maine",
                                                 "Massachusetts",
                                                 "New Hemisphere",
                                                 "Rhode Islands",
                                                 "Vermont",
                                                 "Maryland",
                                                 "Delaware",
                                                 "Virginia",
                                                 "New Jersey",
                                                 "New York",
                                                 "Pennsylvania"
                                                 )~"New England/Mid-Atlantic",
                                    name %in% c("Alabama",
                                                 "Mississipi",
                                                 "Louisiana",
                                                 "Texas",
                                                 "North Carolina",
                                                 "South Carolina",
                                                 "Georgia",
                                                 "Florida")~"Southeast",
                                    name %in% c("California", "Oregon", "Washignton")~"West Coast",
                                    name == "Alaska"~"Alaska",
                                    name == "Hawaii"~"Pacific Islands"))



### clean data ###

# clean data for cetaceans

data_clean_cetacean <- data %>%
  filter(catg1 == "Cetaceans") %>%
  select(stock, stock_id, survey_method, abundance_units, year, abundance, us_region, parent_stock_id, catg1, catg2) %>%
  filter(abundance_units == "Total number") %>%
  group_by(stock_id, survey_method, abundance_units) %>%
  summarize(total_years = n()) %>%
  group_by(stock_id) %>%
  filter(total_years == max(total_years)) %>%
  filter(!duplicated(stock_id)) %>%
  rename(max_est = total_years) %>%
  select(-survey_method, -abundance_units) %>%
  mutate(catg_est = case_when(max_est<7~"<7",
                              max_est>=7 ~">=7"))


# clean data for pinnipeds

data_clean_pinniped <- data %>%
  filter(catg1 == "Pinnipeds"|catg1 == "Fissipeds") %>%
  select(stock, stock_id, survey_method, abundance_units, year, abundance, us_region, parent_stock_id, catg1, catg2) %>%
  group_by(stock_id, survey_method, abundance_units) %>%
  summarize(total_years = n()) %>%
  filter(!stock_id %in% c("HBSEAL-CAISLANDS",
                          "HBSEAL-CAMAIN",
                          "NESEAL-CACENTRAL",
                          "NESEAL-CAISLANDS",
                          "NFSEAL-BO",
                          "NFSEAL-GR",
                          "NFSEAL-PL",
                          "STSEALION-CA",
                          "STSEALION-CALISLAND",
                          "STSEALION-CGUAK",
                          "STSEALION-EALISLAND",
                          "STSEALION-EGUAK",
                          "STSEALION-OR",
                          "STSEALION-SEAK",
                          "STSEALION-WA",
                          "STSEALION-WALISLAND",
                          "STSEALION-WGUAK",
                          "HBSEAL-WACOA",
                          "HBSEAL-ORCOA"
                          )) %>%
  group_by(stock_id) %>%
  filter(total_years == max(total_years)) %>%
  filter(!duplicated(stock_id)) %>%
  select(-survey_method, -abundance_units)%>%
  rename(max_est = total_years) %>%
  mutate(catg_est = case_when(max_est<7~"<7",
                              max_est>=7~">=7"))

# clean species key
species_key_clean <- species_key %>%
  select(stock, stock_id, us_region, parent_stock_id, catg1, catg2) %>%
  filter(is.na(parent_stock_id)) %>%
  filter(!is.na(us_region)) %>%
  select(-parent_stock_id) %>%
  separate(us_region, into = c("us_region_1", "us_region_2"), sep=",")

# calculate total stocks by us region
us_region_1 <- species_key_clean %>%
  group_by(us_region_1, catg2) %>%
  summarize(total_stocks = n()) %>%
  rename(us_region = us_region_1)

us_region_2 <- species_key_clean %>%
  group_by(us_region_2, catg2) %>%
  summarize(total_stocks = n()) %>%
  rename(us_region = us_region_2) %>%
  na.omit() %>%
  mutate(us_region = case_when(us_region == " Southeast"~"Southeast",
                               us_region == " Alaska"~"Alaska"))

us_region <- bind_rows(us_region_1, us_region_2) %>%
  mutate(total_stocks = case_when(us_region == "Alaska"&catg2 == "Large whales"~7,
                                  us_region == "Alaska"&catg2 == "Otarrids"~3,
                                  us_region == "Alaska"&catg2 == "Phocids"~19,
                                  us_region == "Southeast"&catg2 == "Dolphins"~46,
                                  us_region == "Southeast"&catg2 == "Large whales"~6,
                                  us_region == "Southeast"&catg2 == "Small whales"~25,
                                  .default = total_stocks)) %>%
  ungroup() %>%
  slice(-c(31,32,33,34,35,36))


# combine the year numbers of cetacean and pinnipeds, calculate total stocks by region
data_clean_total <- bind_rows(data_clean_cetacean, data_clean_pinniped)

us_region_1_est <- left_join(data_clean_total, species_key_clean, by = "stock_id") %>%
  group_by(us_region_1, catg2, catg_est) %>%
  summarize(total_stocks = n()) %>%
  rename(us_region = us_region_1)

us_region_2_est <- left_join(data_clean_total, species_key_clean, by = "stock_id") %>%
  group_by(us_region_2, catg2, catg_est) %>%
  summarize(total_stocks = n()) %>%
  rename(us_region = us_region_2) %>%
  na.omit() %>%
  mutate(us_region = case_when(us_region == " Southeast"~"Southeast",
                               us_region == " Alaska"~"Alaska"))

us_region_est <- bind_rows(us_region_1_est, us_region_2_est) %>%
  mutate(total_stocks = case_when(us_region == "Alaska"&catg2 == "Large whales"&catg_est == ">=7"~2,
                                  us_region == "Alaska"&catg2 == "Otarrids"&catg_est == ">=7"~3,
                                  us_region == "Alaska"&catg2 == "Phocids"&catg_est == ">=7"~12,
                                  .default = total_stocks)) %>%
  ungroup() %>%
  slice(-c(17,18,19)) %>%
  select(us_region, catg2, catg_est, total_stocks) %>%
  rename(total_stocks_est = total_stocks)

# produce final data

data_final <- left_join(us_region, us_region_est, by = c("us_region" = "us_region", "catg2" = "catg2"), relationship = "many-to-many") %>%
  mutate(catg_est = ifelse(is.na(catg_est), "No estimates", catg_est)) %>%
  mutate(total_stocks_est = ifelse(catg_est == "No estimates", total_stocks, total_stocks_est)) %>%
  mutate(catg_no_est = ifelse(catg_est != "No estimates", total_stocks-total_stocks_est, total_stocks_est)) %>%
  mutate(catg_less = ifelse(catg_est=="<7", total_stocks_est, 0)) %>%
  mutate(catg_great = ifelse(catg_est==">=7", total_stocks_est, 0)) %>%
  slice(-24) %>%
  mutate(catg_no_est = case_when(us_region == "West Coast"&catg2 == "Large whales"~1,
                                 .default = catg_no_est)) %>%
  mutate(catg_less = case_when(us_region == "West Coast"&catg2 == "Large whales"~1,
                                   .default = catg_less)) %>%
  select(-catg_est, -total_stocks_est) %>%
  mutate(percentage_no_est = (catg_no_est/total_stocks)*100,
         percentage_less_est = (catg_less/total_stocks)*100,
         percentage_great_est = (catg_great/total_stocks)*100) %>%
  mutate(percentage_no_est = round(percentage_no_est,0),
         percentage_less_est = round(percentage_less_est,0),
         percentage_great_est = round(percentage_great_est,0)) %>%
  left_join(us_region_coord, by = "us_region") %>%
  mutate(radius = case_when(total_stocks >=1 & total_stocks<5~1,
                            total_stocks >=5 & total_stocks<10~2,
                            total_stocks >=10 & total_stocks<20~3,
                            total_stocks >=20~4))



data_final_ak <- data_final %>% 
  filter(us_region == "Alaska")

data_final_ak_sf <- st_as_sf(x = data_final_ak,
                             coords = c("long", "lat"),
                             crs = 4326)

data_final_ak_sf_3467 <- data_final_ak_sf %>%
  st_transform(data_final_ak_sf, crs = 3467)

data_final_ak_sf_3467$long <- st_coordinates(data_final_ak_sf_3467)[,1]
data_final_ak_sf_3467$lat <- st_coordinates(data_final_ak_sf_3467)[,2]
data_final_ak_3467_df <- data_final_ak_sf_3467 %>%
  st_drop_geometry()

data_final_ak$long <- st_coordinates(data_final_ak_sf)[,1]
data_final_ak$lat <-st_coordinates(data_final_ak_sf)[,2]

usa_ak <- usa%>%
  filter(name == "Alaska")

usa_ak_transform <- usa_ak %>%
  st_transform(usa_ak, crs = 3467)

ggplot() +
  geom_sf(usa_ak_transform, mapping = aes())
  

# Make the figure

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
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))


main_map <- ggplot() +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth = 0.2, inherit.aes = F) +
  geom_scatterpie(data = data_final %>% filter(catg2 == "Dolphins"),
                  aes(x = long, y = lat, group = us_region, r = radius),
                  cols = c("percentage_no_est", "percentage_less_est", "percentage_great_est"), color = NA )+
  coord_sf(xlim = c(-65, -130), ylim = c(10, 50)) +
  facet_wrap(.~catg2) +
  theme_bw() +base_theme

main_map



insert_AK <- ggplot() +
  geom_sf(data = usa_ak, fill = "grey85", col = "white", linewidth = 0.2, inherit.aes = F) +
  geom_scatterpie(data = data_final_ak_3467_df%>% filter(catg2 == "Dolphins"),
                  aes(x = long, y = lat, group = us_region, r = 200000),
                  cols = colnames(data_final_ak_3467_df[,c(7:9)]), color = NA )+
  coord_sf(xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), crs = 3467) +
  theme_void() + theme(legend.position = "none")
  

insert_AK

insert_HI <- ggplot() +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth = 0.2, inherit.aes = F) +
  geom_scatterpie(data = data_final%>% filter(catg2 == "Dolphins"),
                  aes(x = long, y = lat, group = us_region, r = 0.7),
                  cols = c("percentage_no_est", "percentage_less_est", "percentage_great_est"), color = NA )+
  coord_sf(xlim = c(-155, -161), ylim = c(19,23)) +
  theme_void() + theme(legend.position = "none")
  
  
  
insert_HI

ggdraw() +
  draw_plot(main_map) +
  draw_plot(insert_AK,
            height = 0.2,
            x = -0.15,
            y = 0.23) +
  draw_plot(insert_HI,
            height = 0.2,
            x = 0.1,
            y = 0.23)







