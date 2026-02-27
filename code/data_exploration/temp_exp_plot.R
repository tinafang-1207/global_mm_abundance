
### load in library ###
library(tidyverse)

### read in data ###
output_temp_blue <- read.csv("data/confidential/stan_output/Blue_whale/summary_warmup_50000_iter_1e+05_temp_0.5.csv")
output_temp_gray <- read.csv("data/confidential/stan_output/Gray_whale/summary_warmup_50000_iter_1e+05_temp.csv")
output_temp_humpback <- read.csv("data/confidential/stan_output/Humpback_whale/summary_warmup_50000_iter_1e+05_temp.csv")
output_temp_sealion <- read.csv("data/confidential/stan_output/California_sea_lion/summary_warmup_50000_iter_1e+05_temp.csv")
output_temp_hseal <- read.csv("data/confidential/stan_output/CA_Harbor_seal/summary_warmup_50000_iter_1e+05_temp.csv")
output_temp_eseal <- read.csv("data/confidential/stan_output/Northern_elephant_seal/summary_warmup_50000_iter_1e+05_temp.csv")


output_blue <- read.csv("data/confidential/stan_output/Blue_whale/summary_warmup_50000_iter_1e+05.csv")
output_gray <- read.csv("data/confidential/stan_output/Gray_whale/summary_warmup_50000_iter_1e+05.csv")
output_humpback <- read.csv("data/confidential/stan_output/Humpback_whale/summary_warmup_50000_iter_1e+05.csv")
temp <- read.csv("data/confidential/input_data/PDO_temp.csv")

### clean data
output_temp_blue_clean <-  output_temp_blue %>%
  rename(est_variables = X) %>%
  filter(est_variables == "impact_E_1") %>%
  mutate(species = "ENP Blue whale")

output_temp_gray_clean <- output_temp_gray %>%
  rename(est_variables = X) %>%
  filter(est_variables == "impact_E_1") %>%
  mutate(species = "ENP Gray whale")


output_temp_humpback_clean <- output_temp_humpback %>%
  rename(est_variables = X) %>%
  filter(est_variables == "impact_E_1") %>%
  mutate(species = "CA/OR/WA Humpback whale")

output_temp_sealion_clean <- output_temp_sealion %>%
  rename(est_variables = X) %>%
  filter(est_variables == "impact_E_1") %>%
  mutate(species = "California sea lion")

output_temp_hseal_clean <- output_temp_hseal %>%
  rename(est_variables = X) %>%
  filter(est_variables == "impact_E_1") %>%
  mutate(species = "Harbor seal (California)")

output_temp_eseal_clean <- output_temp_eseal %>%
  rename(est_variables = X) %>%
  filter(est_variables == "impact_E_1") %>%
  mutate(species = "Northern elephant seal (California)")

output_temp_all <- rbind(output_temp_blue_clean, 
                         output_temp_gray_clean, 
                         output_temp_humpback_clean, 
                         output_temp_sealion_clean,
                         output_temp_hseal_clean,
                         output_temp_eseal_clean ) %>%
  mutate(sign = ifelse(mean >= 0, "positive", "negative"))



g_temp <- ggplot(output_temp_all, aes(y = species, x = mean, color = sign)) +
  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  # Horizontal error bars
  geom_errorbarh(aes(xmin = X2.5., xmax = X97.5.), height = 0, linewidth = 0.8) +
  # Mean point
  geom_point(size = 3) +
  # Species labels next to points
  geom_text(aes(label = species), hjust = ifelse(output_temp_all$mean >= 0, -0.2, 1.2), vjust = 2, size = 2.5, show.legend = FALSE) +
  # Color scale
  scale_color_manual(values = c("positive" = "blue", "negative" = "red")) +
  labs(x = "Impact of temperature", y = NULL) +
  theme_bw()+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "none")

g_temp

###############################################################################

temp_clean <- temp %>% 
  select(Year, average) %>%
  filter(Year < 2025) %>%
  mutate(average = as.numeric (average)) %>%
  rename(
    year = Year,
    temp_raw = average
  ) %>%
  mutate(
    temp_mean = as.numeric(scale(temp_raw, center = TRUE, scale = FALSE)),
    temp_scaled = as.numeric(scale(temp_raw, center = TRUE, scale = TRUE))
  ) %>%
  select(year, temp_scaled)



r_change <- temp_clean %>%
  mutate(blue_whale = exp(temp_scaled * 0.57) * 0.037,
         humpback_whale = exp(temp_scaled * -0.15) * 0.082,
         gray_whale = exp(temp_scaled*-0.62) * 0.042)

r_change_long <- r_change %>%
  pivot_longer(
    cols = c(blue_whale, humpback_whale, gray_whale),
    names_to = "species",
    values_to = "growth_rate"
  ) 

ref_lines <- tibble(
  species = c("blue_whale", "gray_whale", "humpback_whale"),
  ref = c(0.037, 0.041, 0.082)
)

r_change <- ggplot(r_change_long, aes(x = year, y = growth_rate, color = temp_scaled)) +
  geom_point(size = 1) +
  #geom_line(aes(group = species)) +
  geom_hline(
    data = ref_lines,
    aes(yintercept = ref),
    linetype = "dashed",
    linewidth = 0.7,
    color = "gray40"
  ) +
  facet_wrap(~ species, scales = "free_y") +
  scale_color_gradientn(
    colors = RColorBrewer::brewer.pal(11, "RdYlBu")[c(11:7, 5:1)]  # blue → dark → red
  ) +
  # scale_color_gradient2(
  #   low  = "blue",   # negative temp index
  #   high = "red",    # positive temp index
  #   name = "Temp index"
  # ) +
  labs(
    x = "Year",
    y = "Population growth rate"
  ) +
  
  theme_bw() + theme(legend.position = "none")

r_change

### save figures

plot_dir <- "figures"

ggsave(g_temp, filename=file.path(plot_dir, "temp_exp_PDO.png"), 
       width=5, height=4, units="in", dpi=600)

ggsave(r_change, filename=file.path(plot_dir, "r_exp_PDO.png"), 
       width = 6, height = 5, units = "in", dpi = 600)







