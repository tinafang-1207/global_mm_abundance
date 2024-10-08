
# clean working environment
rm(list = ls())

# read in package
library(tidyverse)

# set plot directory
plotdir <- "figures"

# read in data
data <- read.csv("data/clean_data/U.S_Abundance_Clean.csv")

data_time_pinniped <- data %>%
  filter(catg1 == "Pinnipeds") %>%
  mutate(sub_area = ifelse(stock == "Harbor seal - Washington Inland", "Washington Inland", sub_area))

data_time_cetacean <- data %>%
  mutate(catg1 = ifelse(common_name == "Mesoplodont beaked whale", "Cetaceans", catg1)) %>%
  mutate(catg2 = ifelse(common_name == "Mesoplodont beaked whale", "Small whales", catg2)) %>%
  filter(catg1 == "Cetaceans")

data_time_otter <- data %>%
  filter(catg1 == "Fissipeds")


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

# Make figure for Phocids

nes <- ggplot(data = data_time_pinniped %>% filter(common_name %in% c("Northern elephant seal")), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = sub_area), position = position_dodge(width = 1))+
  #geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = sub_area), position = position_dodge(width = 1), fatten = 1)+
  facet_wrap(.~stock, scales = "free") +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Sub-area") +
  base_theme+guides(color = "none")

nes


hms <- ggplot(data = data_time_pinniped %>% filter(common_name %in% c("Hawaiian monk seal")), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  facet_wrap(.~stock_id, scales = "free") +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme + guides(shape = "none")

hms


hs_ak <- ggplot(data = data_time_pinniped %>% filter(common_name == "Harbor seal"&us_region == "Alaska"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  facet_wrap(.~stock_id, scales = "free", ncol = 3) +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme + theme(legend.position = c(0.8, 0.1))

hs_ak

# Time-series figures below are problematic

hs_wc <- ggplot(data = data_time_pinniped %>% filter(stock == "Harbor seal - California"|stock == "Harbor seal - Oregon/Washington Coastal"|stock == "Harbor seal - Washington Inland"), 
                aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = sub_area))+
  facet_wrap(.~stock, scales = "free", ncol = 2) +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Sub-area") +
  base_theme + theme(legend.position = c(0.6, 0.3))

hs_wc


# Make figures for Otarrids (fur seal and sea lion)

# California sea lion, Northern fur seal, Steller sea lion

casl <- ggplot(data = data_time_pinniped %>% filter(common_name == "California sea lion"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(.~stock, scales = "free") +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme

casl

nfs <- ggplot(data = data_time_pinniped %>% filter(common_name == "Northern fur seal"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(.~stock, scales = "free") +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme

nfs


ssl_e <- ggplot(data = data_time_pinniped %>% filter(stock == "Steller sea lion - Eastern"|parent_stock_id == "STSEALION-E"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(.~stock, scales = "free") +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme + theme(legend.position = c(0.8, 0.4))

ssl_e

ssl_w <- ggplot(data = data_time_pinniped %>% filter(stock == "Steller sea lion - Western"|parent_stock_id == "STSEALION-W"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(.~stock, scales = "free") +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme + theme(legend.position = c(0.4, 0.2))

ssl_w

# Make figure for sea otters

otter <- ggplot(data = data_time_otter, aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  #geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(.~stock, scales = "free") +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme

otter





# Make figure for Cetaceans

# set color palette for survey methods
survey_color <- c("Other model" = "#00798c", "Line-transect" = "#d1495b", "Count" = "#edae49", "Bayesian analysis" = "#66a182", "Mark recapture" = "#2e4057", "Aerial survey" = "#8d96a3")

large_whale <- ggplot(data = data_time_cetacean %>% filter(catg2 == "Large whales"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  facet_wrap(.~stock_id, scales = "free", ncol = 3) +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme

large_whale

small_whale <- ggplot(data = data_time_cetacean %>% filter(catg2 == "Small whales"), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  facet_wrap(.~stock_id, scales = "free", ncol = 3) +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme+theme(legend.position = c(0.4, 0.2))+guides(shape = "none")

small_whale

dol_por <- ggplot(data = data_time_cetacean %>% filter(catg2 %in% c("Dolphins", "Porpoise")), aes(x = year, y = abundance, color = survey_method)) +
  geom_point(aes(shape = abundance_units), position = position_dodge(width = 1))+
  geom_pointrange(aes(ymin = abundance_low, ymax = abundance_hi, shape = abundance_units), position = position_dodge(width = 1), fatten = 1)+
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(.~stock_id, scales = "free", ncol = 3) +
  labs(x = "Year", y = "Abundance")+
  scale_color_manual(name = "Survey method", values = survey_color) +
  scale_shape_discrete(name = "Abundance units") +
  base_theme+guides(shape = "none")

dol_por



# save the time-series figures for whales
ggsave(large_whale, filename = file.path(plotdir, "Fig1a_large_whale_time_series.png"),
       width = 8, height = 8, units = "in", dpi = 600)

ggsave(small_whale, filename = file.path(plotdir, "Fig1b_small_whale_time_series.png"),
       width = 6, height = 6, units = "in", dpi = 600)

ggsave(dol_por,  filename = file.path(plotdir, "Fig1c_dolphin_porpoise_time_series.png"),
       width = 8, height = 9, units = "in", dpi = 600)

# save the time-series figures for Phocids (seal)

ggsave(nes, filename = file.path(plotdir, "Fig2a_Northern_elephant_seal_time_series.png"),
       width = 6, height = 6, units = "in", dpi = 600)

ggsave(hms, filename = file.path(plotdir, "Fig2b_Hawaiian_monk_seal_time_series.png"),
       width = 6, height = 6, units = "in", dpi = 600)

ggsave(hs_ak, filename = file.path(plotdir, "Fig2c_Harbor_seal_Alaska_time_series.png"),
       width = 8, height = 8, units = "in", dpi = 600)

ggsave(hs_wc, filename = file.path(plotdir, "Fig2d_Harbor_seal_west_coast_time_series.png"),
       width = 6, height = 7, units = "in", dpi = 600)

# save the time-series figures for Otarrids (sealion)

ggsave(casl, filename = file.path(plotdir, "Fig3a_California_sea_lion_time_series.png"),
       width = 6, height = 6, units = "in", dpi = 600)

ggsave(nfs, filename = file.path(plotdir, "Fig3b_Northern_fur_seal_time_series.png"),
       width = 8, height = 8, units = "in", dpi = 600)

ggsave(ssl_e, filename = file.path(plotdir, "Fig3c_Steller_sea_lion_eastern_time_series.png"),
       width = 8, height = 8, units = "in", dpi = 600)

ggsave(ssl_w, filename = file.path(plotdir, "Fig3d_Steller_sea_lion_western_time_series.png"),
       width = 8, height = 8, units = "in", dpi = 600)

# save the time-series figure for sea otter

ggsave(otter, filename = file.path(plotdir, "Fig4a_sea_otter_time_series.png"),
       width = 6, height = 4, units = "in", dpi = 600)

