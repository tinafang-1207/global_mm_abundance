
# clean working environment
rm(list = ls())

# read in package
library(tidyverse)

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

large_whale <- ggplot(data = data_time_cetacean %>% filter(catg2 == "Large whales") %>% filter(abundance_units == "Total number"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~stock, scales = "free") +
  base_theme

large_whale

small_whale <- ggplot(data = data_time_cetacean %>% filter(catg2 == "Small whales"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~stock, scales = "free") +
  base_theme

small_whale

dolphin <- ggplot(data = data_time_cetacean %>% filter(catg2 == "Dolphins"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~stock, scales = "free") +
  base_theme

dolphin

porpoise <- ggplot(data = data_time_cetacean %>% filter(catg2 == "Porpoise"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~stock, scales = "free") +
  base_theme

porpoise




