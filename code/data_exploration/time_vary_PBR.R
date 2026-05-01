
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)

### read in data ###

# model estimation: time-varying R

catch <- read.csv("data/confidential/input_data/input_final.csv")
output_sl_r_orig <- read.csv("data/confidential/stan_output/California_sea_lion/summary_warmup_50000_iter_1e+05_temp_exp.csv") %>%
  rename(variable = X)
output_hs_r_orig <- read.csv("data/confidential/stan_output/CA_Harbor_seal/summary_warmup_50000_iter_1e+05_temp_r_exp.csv") %>%
  rename(variable = X)
output_es_r_orig <- read.csv("data/confidential/stan_output/Northern_elephant_seal/summary_warmup_50000_iter_1e+05_temp_r_exp.csv") %>%
  rename(variable = X)
output_so_r_orig <- read.csv("data/confidential/stan_output/Southern_sea_otter/summary_warmup_50000_iter_1e+05_temp_r_exp.csv") %>%
  rename(variable = X)

# SAR PBR
sar_orig <- readRDS("data/confidential/input_data/Pacific_SARs_parameters.Rds")

# clean data
sar_clean <- sar_orig %>%
  filter(stock %in% c("California sea lion (U.S.)", "Harbor seal (CA)", "Northern elephant seal (CA Breeding)")) %>%
  select(year, stock, n_min, r_max, rf, pbr) %>%
  rename(n_min_sar = n_min,
         r_max_sar = r_max,
         rf_sar = rf,
         pbr_sar = pbr) 


output_sl_clean <- output_sl_r_orig  %>%
  filter(str_detect(variable, "^(N_med|R_t)\\[")) %>%
  mutate(
    type = str_extract(variable, "^[^\\[]+"),
    index = as.integer(str_extract(variable, "\\d+")),
    year = 1980 + index
  ) %>%
  pivot_longer(
    cols = c(`X2.5.`, `X50.`, `X97.5.`),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    stat = recode(stat,
                  "X2.5." = "lower",
                  "X50." = "median",
                  "X97.5." = "upper")
  ) %>%
  pivot_wider(
    names_from = c(type, stat),
    values_from = value
  ) %>%
  group_by(year) %>%
  summarize(
    N_med = first(na.omit(N_med_median)),
    N_med_lower = first(na.omit(N_med_lower)),
    N_med_upper = first(na.omit(N_med_upper)),
    R_t = first(na.omit(R_t_median)),
    .groups = "drop"
  ) %>%
  arrange(year) %>%
  mutate(stock = "California sea lion (U.S.)") %>%
  rename(n_min_est = N_med_lower,
         r_max_est = R_t
         ) %>%
  select(year, stock, n_min_est, r_max_est)


output_hs_clean <- output_hs_r_orig  %>%
  filter(str_detect(variable, "^(N_med|R_t)\\[")) %>%
  mutate(
    type = str_extract(variable, "^[^\\[]+"),
    index = as.integer(str_extract(variable, "\\d+")),
    year = 1983 + index
  ) %>%
  pivot_longer(
    cols = c(`X2.5.`, `X50.`, `X97.5.`),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    stat = recode(stat,
                  "X2.5." = "lower",
                  "X50." = "median",
                  "X97.5." = "upper")
  ) %>%
  pivot_wider(
    names_from = c(type, stat),
    values_from = value
  ) %>%
  group_by(year) %>%
  summarize(
    N_med = first(na.omit(N_med_median)),
    N_med_lower = first(na.omit(N_med_lower)),
    N_med_upper = first(na.omit(N_med_upper)),
    R_t = first(na.omit(R_t_median)),
    .groups = "drop"
  ) %>%
  arrange(year) %>%
  mutate(stock = "Harbor seal (CA)") %>%
  rename(n_min_est = N_med_lower,
         r_max_est = R_t
  ) %>%
  select(year, stock, n_min_est, r_max_est)

output_es_clean <- output_es_r_orig  %>%
  filter(str_detect(variable, "^(N_med|R_t)\\[")) %>%
  mutate(
    type = str_extract(variable, "^[^\\[]+"),
    index = as.integer(str_extract(variable, "\\d+")),
    year = 1980 + index
  ) %>%
  pivot_longer(
    cols = c(`X2.5.`, `X50.`, `X97.5.`),
    names_to = "stat",
    values_to = "value"
  ) %>%
  mutate(
    stat = recode(stat,
                  "X2.5." = "lower",
                  "X50." = "median",
                  "X97.5." = "upper")
  ) %>%
  pivot_wider(
    names_from = c(type, stat),
    values_from = value
  ) %>%
  group_by(year) %>%
  summarize(
    N_med = first(na.omit(N_med_median)),
    N_med_lower = first(na.omit(N_med_lower)),
    N_med_upper = first(na.omit(N_med_upper)),
    R_t = first(na.omit(R_t_median)),
    .groups = "drop"
  ) %>%
  arrange(year) %>%
  mutate(stock = "Northern elephant seal (CA Breeding)") %>%
  rename(n_min_est = N_med_lower,
         r_max_est = R_t
  ) %>%
  select(year, stock, n_min_est, r_max_est)

catch_clean <- catch %>%
  filter(species %in% c("California_sea_lion", "CA_harbor_seal", "Northern_elephant_seal")) %>%
  select(year, species, catch) %>%
  rename(stock = species) %>%
  mutate(stock = case_when(stock == "California_sea_lion"~"California sea lion (U.S.)",
                           stock == "CA_harbor_seal"~"Harbor seal (CA)",
                           stock == "Northern_elephant_seal"~"Northern elephant seal (CA Breeding)"))

output_clean_final <- bind_rows(output_sl_clean, output_hs_clean, output_es_clean) %>%
  left_join(catch_clean, by = c("year", "stock"))

# join with sar

output_join <- left_join(output_clean_final, sar_clean, by = c("year", "stock")) %>%
  mutate(rf_sar = ifelse(is.na(rf_sar), 1, rf_sar)) %>%
  mutate(pbr_est = n_min_est * 0.5 * r_max_est * rf_sar)


output_long <- output_join %>%
  select(year, stock, pbr_est, pbr_sar, catch) %>%
  pivot_longer(
    cols = c(catch, pbr_sar, pbr_est),
    names_to = "variable",
    values_to = "value"
  ) 

output_long_no_na <- output_long %>%
  filter(year >= 1996, !is.na(value))

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
                    legend.key = element_rect(fill = NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

time_vary_pbr <- ggplot(output_long_no_na,
       aes(x = year, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  facet_wrap(~ stock, scales = "free_y") +
  labs(
    x = "Year",
    y = "Value"
  ) +
  theme_bw() + base_theme

time_vary_pbr 













