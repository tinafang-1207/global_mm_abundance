
# clean working environment
rm(list = ls())

# read in package
library(tidyverse)

# set plot directory
plotdir <- "figures"

# read in data
data <- read.csv("data/clean_data/U.S_Abundance_Clean.csv")

# data_time_pinniped <- data %>%
#   filter(!stock %in% c("Northern elephant seal - Central California" ,
#                        "Northern elephant seal - Channel Islands",
#                        "Northern fur seal - Bogoslof",
#                        "Northern fur seal  - St. George",
#                        "Northern fur seal  - St. Paul",
#                        "Steller sea lion - Southeast Alaska",
#                        "Steller sea lion - Washignton",
#                        "Steller sea lion - California",
#                        "Steller sea lion - Oregon",
#                        "Steller sea lion - Eastern Aluetian Islands",
#                        "Steller sea lion - Western Aleutian Islands",
#                        ))

data_time_cetacean <- data %>%
  mutate(catg1 = ifelse(common_name == "Mesoplodont beaked whale", "Cetaceans", catg1)) %>%
  mutate(catg2 = ifelse(common_name == "Mesoplodont beaked whale", "Small whales", catg2)) %>%
  filter(catg1 == "Cetaceans")

# make time series figure 

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

# set color palette for survey methods
survey_color <- c("Other model" = "#00798c", "Line-transect" = "#d1495b", "Count" = "#edae49", "Bayesian analysis" = "#66a182", "Mark recapture" = "#2e4057", "Aerial survey" = "#8d96a3")

large_whale <- ggplot(data = data_time_cetacean %>% filter(catg2 == "Large whales"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  facet_wrap(.~stock, scales = "free", ncol = 3) +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme+theme(legend.position = "none")

large_whale

small_whale <- ggplot(data = data_time_cetacean %>% filter(catg2 == "Small whales"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  facet_wrap(.~stock, scales = "free", ncol = 3) +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme+theme(legend.position = "none")

small_whale

dol_por <- ggplot(data = data_time_cetacean %>% filter(catg2 %in% c("Dolphins", "Porpoise")), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  facet_wrap(.~stock, scales = "free", ncol = 3) +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme+theme(legend.position = "none")

dol_por



# save the time-series figures for whales
ggsave(large_whale, filename = file.path(plotdir, "Fig1a_large_whale_time_series.png"),
       width = 8, height = 8, units = "in", dpi = 600)

ggsave(small_whale, filename = file.path(plotdir, "Fig1b_small_whale_time_series.png"),
       width = 10, height = 10, units = "in", dpi = 600)

ggsave(dol_por,  filename = file.path(plotdir, "Fig1c_dolphin_porpoise_time_series.png"),
       width = 10, height = 10, units = "in", dpi = 600)


