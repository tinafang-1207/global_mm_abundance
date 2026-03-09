
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)

### read in data ###
data_orig <- read.csv("data/clean_data/U.S_Abundance_Clean.csv")

plotdir <- "figures/meeting_Trevor"

# clean data for large whales #

data_lw_survey <- data_orig %>%
  filter(catg2 == "Large whales") %>%
  group_by(stock_id,survey_method, abundance_units) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  group_by(stock_id, abundance_units) %>%
  slice_max(n_years, with_ties = FALSE) %>%
  ungroup()

# keep only the survey with the longest time series
data_lw <- data_orig %>%
  filter(catg2 == "Large whales") %>%
  semi_join(data_lw_survey, by = c("stock_id", "survey_method", "abundance_units"))

# clean data for small whale
data_sw_survey <- data_orig %>%
  filter(catg2 == "Small whales") %>%
  group_by(stock_id,survey_method, abundance_units) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  group_by(stock_id, abundance_units) %>%
  slice_max(n_years, with_ties = FALSE) %>%
  ungroup()

data_sw <- data_orig %>%
  filter(catg2 == "Small whales") %>%
  semi_join(data_sw_survey, by = c("stock_id", "survey_method", "abundance_units"))

# clean data for dolphins
data_dolphin_survey <- data_orig %>%
  filter(catg2 == "Dolphins") %>%
  group_by(stock_id,survey_method, abundance_units) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  group_by(stock_id, abundance_units) %>%
  slice_max(n_years, with_ties = FALSE) %>%
  ungroup()

data_dolphin <- data_orig %>%
  filter(catg2 == "Dolphins") %>%
  semi_join(data_dolphin_survey, by = c("stock_id", "survey_method", "abundance_units"))

# clean data for porpoise
data_porpoise_survey <- data_orig %>%
  filter(catg2 == "Porpoise") %>%
  group_by(stock_id,survey_method, abundance_units) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  group_by(stock_id, abundance_units) %>%
  slice_max(n_years, with_ties = FALSE) %>%
  ungroup()

data_porpoise <- data_orig %>%
  filter(catg2 == "Porpoise") %>%
  semi_join(data_porpoise_survey, by = c("stock_id", "survey_method", "abundance_units"))

# clean data for Ottarids
data_ottarids_survey <- data_orig %>%
  filter(catg2=="Otarrids") %>%
  group_by(stock_id,survey_method, abundance_units) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  group_by(stock_id, abundance_units) %>%
  slice_max(n_years, with_ties = FALSE) %>%
  ungroup()

data_ottarids <- data_orig %>%
  filter(catg2 == "Otarrids") %>%
  semi_join(data_ottarids_survey, by = c("stock_id", "survey_method", "abundance_units"))

# clean data for Phocids
data_phocid_survey <- data_orig %>%
  filter(catg2=="Phocids") %>%
  group_by(stock_id,survey_method, abundance_units) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  group_by(stock_id, abundance_units) %>%
  slice_max(n_years, with_ties = FALSE) %>%
  ungroup()

data_phocids <- data_orig %>%
  filter(catg2 == "Phocids") %>%
  semi_join(data_phocid_survey, by = c("stock_id", "survey_method", "abundance_units"))




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

g_lw <- ggplot(data = data_lw, aes(x = year, y = abundance, color = abundance_units)) +
  geom_point()+
  geom_line() +
  facet_wrap(.~stock_id, scales = "free") +
  labs(x = "", y = "")+
  base_theme + theme(legend.position = "bottom")
  
g_lw 

g_sw <- ggplot(data = data_sw, aes(x = year, y = abundance, color = abundance_units)) +
  geom_point()+
  geom_line() +
  facet_wrap(.~stock_id, scales = "free") +
  labs(x = "", y = "")+
  base_theme + theme(legend.position = "bottom")

g_sw 
  
g_dolphin <-  ggplot(data = data_dolphin, aes(x = year, y = abundance, color = abundance_units)) +
  geom_point()+
  geom_line() +
  facet_wrap(.~stock_id, scales = "free") +
  labs(x = "", y = "")+
  base_theme + theme(legend.position = "bottom")

g_dolphin


g_porpoise <- ggplot(data = data_porpoise, aes(x = year, y = abundance, color = abundance_units)) +
  geom_point()+
  geom_line() +
  facet_wrap(.~stock_id, scales = "free") +
  labs(x = "", y = "")+
  base_theme + theme(legend.position = "bottom")

g_porpoise

g_ottarids <- ggplot(data = data_ottarids, aes(x = year, y = abundance, color = abundance_units)) +
  geom_point()+
  geom_line() +
  facet_wrap(.~stock_id, scales = "free") +
  labs(x = "", y = "")+
  base_theme + theme(legend.position = "bottom")

g_ottarids 

g_phocids <- ggplot(data = data_phocids, aes(x = year, y = abundance, color = abundance_units)) +
  geom_point()+
  geom_line() +
  facet_wrap(.~stock_id, scales = "free") +
  labs(x = "", y = "")+
  base_theme + theme(legend.position = "bottom")

g_phocids


# save plot

ggsave(g_lw, filename = file.path(plotdir, "lw_abundance.png"),
       width = 6, height = 6, units = "in", dpi = 600)

ggsave(g_sw, filename = file.path(plotdir, "sw_abundance.png"),
       width = 6, height = 6, units = "in", dpi = 600)

ggsave(g_dolphin, filename = file.path(plotdir, "dolphin_abundance.png"),
       width = 6, height = 6, units = "in", dpi = 600)

ggsave(g_porpoise, filename = file.path(plotdir, "porpoise_abundance.png"),
       width = 6, height = 6, units = "in", dpi = 600)

ggsave(g_ottarids, filename = file.path(plotdir, "ottarids_abundance.png"),
       width = 8, height = 8, units = "in", dpi = 600)

ggsave(g_phocids, filename = file.path(plotdir, "phocids_abundance.png"),
       width = 8, height = 8, units = "in", dpi = 600)
