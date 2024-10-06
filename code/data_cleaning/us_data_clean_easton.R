
# clean working environment
rm(list = ls())

# read in package
library(tidyverse)

# read in data
data_orig <- readxl::read_excel("data/raw_data/Easton + Zachary/abundance_papers.xlsx")

# clean data

# leave only those that has trend estimates in sar
data_clean <- data_orig %>%
  janitor::clean_names() %>%
  select(-x9,-x10,-x11) %>%
  rename(sar_year = sa_rs_year,
         sar_pop_trend_orig = population_trend_reported_in_sar) %>%
  mutate(sar_pop_trend_clean = ifelse(grepl("Y", sar_pop_trend_orig), "Y", "N")) %>%
  mutate(sar_pop_trend_clean = ifelse(stock%in%c("Harbor porpoise- San Francisco, Russian River", 
                                                 "Killer Whale- Southern resident", 
                                                 "Blue whale -Eastern North Pacific",
                                                 "Gray whale- Eastern North Pacific",
                                                 "Humpback whale- Gulf of Maine",
                                                 "Minke whale- California-Oregon-Washington",
                                                 "Mesoplodont beaked whales- California, Oregon, Washington"), "Y", sar_pop_trend_clean)) %>%
  filter(sar_pop_trend_clean == "Y")
  
