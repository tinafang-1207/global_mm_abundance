
# clean working environment
rm(list = ls())

# read in package
library(tidyverse)

# set plot directory
plotdir <- "figures"

# read in data
data <- read.csv("data/clean_data/U.S_Abundance_Clean.csv")





# clean data 
data_clean <- data %>%
  select(stock, stock_id, common_name, survey_method, abundance_units, year, abundance, sub_area, parent_stock_id, source_type, catg1, catg2) %>%
  mutate(catg1 = ifelse(common_name == "Mesoplodont beaked whale", "Cetaceans", catg1)) %>%
  mutate(catg2 = ifelse(common_name == "Mesoplodont beaked whale", "Small whales", catg2)) %>%
  mutate(catg1 = factor(catg1, levels = c("Cetaceans", "Pinnipeds", "Fissipeds")))


data_survey <- data_clean %>%
  group_by(stock_id, survey_method) %>%
  summarize(year_numbers = n())

data_cetaceans <- data_clean %>%
  filter(catg1 == "Cetaceans")


# make figure 
my_theme <- theme(axis.text=element_text(size=8),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


g1 <- ggplot(data_cetaceans, aes(x=year, y=stock_id, fill=catg1)) +
  geom_raster() +
  facet_grid(survey_method~., scales="free_y", space="free_y") +
  # Labels
  #scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_fill_discrete(guide=F) +
  # Theme
  theme_bw() + my_theme + 
  theme(axis.title=element_blank(), 
        legend.position="bottom")

g1





