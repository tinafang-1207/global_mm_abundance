
#### clean working environment #####
rm(list = ls())

#### read in package ####
library(tidyverse)

# set plot directory
plotdir <- "figures"

### read in data ###
data <- read.csv("data/clean_data/U.S_Abundance_Clean.csv")

species_key <- read.csv("data/raw_data/species_key.csv", na = "-") %>%
  rename(us_region = `U.S..region`) %>%
  select(-notes)

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
         percentage_great_est = round(percentage_great_est,0))


data_final_bar <- data_final %>%
  gather(key = "percentage_catg", value = "percentage", percentage_no_est, percentage_less_est, percentage_great_est) %>%
  mutate(percentage = ifelse(us_region == "West Coast"&catg2 == "Large whales"&percentage_catg == "percentage_great_est", 72, percentage)) %>%
  filter(catg2 %in% c("Large whales", "Small whales", "Dolphins", "Porpoise", "Otarrids", "Phocids")) %>%
  mutate(us_region = factor(us_region, levels = c("Pacific Islands", "Alaska", "West Coast", "Southeast", "New England/Mid-Atlantic")))%>%
  mutate(catg2 = factor(catg2, levels = c("Large whales", "Small whales", "Dolphins", "Porpoise", "Otarrids", "Phocids"))) %>%
  mutate(percentage_catg = case_when(percentage_catg == "percentage_no_est"~"No data",
                                     percentage_catg == "percentage_less_est"~"Less than 7 estimates/10 years",
                                     percentage_catg == "percentage_great_est"~"More than 7 estimates/10 years")) %>%
  mutate(percentage_catg = factor(percentage_catg, levels = c("No data", "Less than 7 estimates/10 years", "More than 7 estimates/10 years"))) %>%
  mutate(label = paste0(percentage, "%")) %>%
  mutate(label_stock = paste0("n=", total_stocks)) %>%
  filter(! label == "0%") %>%
  slice(-c(23,40))
  



base_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))




percentage_type <- c("No data" = "#adb6b6", "Less than 7 estimates/10 years" = "#ffdc91", "More than 7 estimates/10 years" = "#925e9f")


g_coverage <- ggplot(data = data_final_bar, aes(x=percentage, y = us_region, fill = percentage_catg)) +
  facet_grid(catg2~., space = "free_y", scales = "free_y") +
  geom_bar(position = position_stack(), stat = "identity", color = "grey30", lwd = 0.2) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.1, color = "black", fontface = "bold") +
  geom_text(aes(label = label_stock, x=101, y = us_region),hjust = 0, inherit.aes = F, size = 2.2, color = "black") +
  labs(x = "Percentage of stocks", y = "") +
  scale_fill_manual(name = "Category", values = percentage_type) +
  scale_x_continuous(lim = c(0, 110), breaks = seq(0,100,25)) +
  theme_bw() + base_theme



g_coverage  

ggsave(g_coverage, filename=file.path(plotdir, "Fig7_data_gap_bar.png"), 
       width= 8, height=6, units="in", dpi=600)





