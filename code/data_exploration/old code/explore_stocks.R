
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Read data
data_orig <- readxl::read_excel("data/raw_data/species_key.xlsx")

# Format data
data <- data_orig %>% 
  separate(stock_id, sep="-", into=c("spp_code", "area_code1"), remove = F) %>% 
  mutate(spp_code=str_trim(spp_code))


# Inspect
################################################################################

# Species key
spp_key <- data %>% 
  count(catg1, catg2, common_name, scientific_name, spp_code)
freeR::which_duplicated(spp_key$common_name)
freeR::which_duplicated(spp_key$scientific_name)
freeR::which_duplicated(spp_key$spp_code)

# Area key
area_key <- data_orig %>% 
  count(area, area_code)
freeR::which_duplicated(area_key$area)
freeR::which_duplicated(area_key$area_code)


# Plot data
################################################################################

ggplot(spp_key, aes(y=reorder(common_name, n), x=n)) +
  facet_grid(catg1~., scale="free_y", space="free_y") +
  geom_bar(stat="identity") +
  labs(x="Number of stocks", y="") +
  theme_bw()


