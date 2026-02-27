
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
  mutate(catg1 = ifelse(common_name == "Mesoplodont beaked whale", "Cetaceans", catg1)) %>%
  mutate(catg2 = ifelse(common_name == "Mesoplodont beaked whale", "Small whales", catg2)) %>%
  mutate(survey_code = case_when(survey_method == "Other model"~"M",
                                 survey_method == "Line-transect"~"LT",
                                 survey_method == "Count"~"C",
                                 survey_method == "Bayesian analysis"~"BA",
                                 survey_method == "Mark recapture"~"MR",
                                 survey_method == "Aerial survey"~"AS")) %>%
  mutate(stock_survey = paste(stock_id,survey_code, sep="-")) %>%
  filter(!abundance_units %in% c("Total number(Males)", "Total number(Females)")) %>%
  mutate(unit_code = case_when(abundance_units == "Total number"~"TN",
                               abundance_units == "Pup"~"P",
                               abundance_units == "Non-pup"~"NP")) %>%
  mutate(stock_unit = paste(stock_id,unit_code, sep="-")) %>%
  mutate(new_id = ifelse(catg1 == "Cetaceans", stock_survey, stock_unit)) %>%
  mutate(new_id = ifelse(catg1 == "Fissipeds", stock_id, new_id)) %>%
  mutate(catg1 = factor(catg1, levels = c("Cetaceans", "Pinnipeds", "Fissipeds"))) %>%
  select(stock, stock_id, new_id, common_name, survey_method, year, abundance, abundance_units, source_type, catg1, catg2)

# select the survey method with the longest time series within each stock
max_method_lengths <- data_clean %>%
  group_by(stock_id, new_id) %>%
  summarise(n_years = n_distinct(year), .groups = "drop") %>%
  group_by(stock_id) %>%
  slice_max(n_years, with_ties = FALSE) %>%
  ungroup()

# filter the data clean
data_clean_max_length <- data_clean %>%
  semi_join(max_method_lengths, by = c("stock_id", "new_id"))

# Take 7 from Easton et al., 2020
data_survey_out <- data_clean_max_length%>%
  group_by(stock_id) %>%
  summarize(year_number = n()) %>%
  filter(year_number<7)

data_survey_in <- data_clean_max_length %>%
  group_by(stock_id) %>%
  summarize(year_number = n()) %>%
  filter(year_number >= 7)

data_final <- data_clean_max_length %>%
  filter(!stock_id %in% data_survey_out$stock_id) %>%
  left_join(data_survey_in, by = "stock_id") %>%
  mutate(stock_id = fct_reorder(.f = as.factor(stock_id),
                                     .x = desc(year_number))) %>%
  mutate(catg1 = factor(catg1, levels = c("Fissipeds", "Cetaceans", "Pinnipeds"))) %>%
  mutate(catg2 = factor(catg2, levels = c("Large whales", "Small whales", "Dolphins", "Porpoise", "Otarrids", "Phocids"))) %>%
  filter(catg1 != "Fissipeds")


# make figure 
my_theme <- theme(axis.text=element_text(size=6),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Plot timeline

g1 <- ggplot(data_final, aes(x=year, y=stock_id, fill=catg1)) +
  geom_raster() +
  facet_grid(catg2~., scales="free_y") +
  # Labels
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_fill_discrete(guide=F) +
  # Theme
  theme_bw() + my_theme + 
  theme(axis.title=element_blank())

g1

# Histogram

g2 <- ggplot(data_final %>% filter(!duplicated(new_id)), aes(x = year_number, fill = catg1))+
  facet_grid(~catg1)+
  geom_histogram(binwidth = 5) +
  labs(x = "Time series length(year)", y = "Number of populations") +
  theme_bw() + my_theme + theme(legend.position = "none")

g2

# save the time series length figure

ggsave(g1, filename = file.path(plotdir, "Fig5a_U.S_time_series_length.png"),
       width = 7, height = 10, units = "in", dpi = 600)





