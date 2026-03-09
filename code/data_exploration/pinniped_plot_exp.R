
### load in library ###
library(tidyverse)
library(bayesplot)

### set up input directory ###
input_dir <- "data/confidential/stan_output/"

### import data ###
output_sealion <- read.csv(file.path(input_dir, "California_sea_lion/summary_warmup_50000_iter_1e+05.csv"))
output_hseal <- read.csv(file.path(input_dir, "CA_Harbor_seal/summary_warmup_50000_iter_1e+05.csv"))
output_eseal <- read.csv(file.path(input_dir, "Northern_elephant_seal/summary_warmup_50000_iter_1e+05.csv"))

fit_sealion <- readRDS(file.path(input_dir, "California_sea_lion/fit_warmup_50000_iter_1e+05.rds"))
fit_hseal <- readRDS(file.path(input_dir, "CA_Harbor_seal/fit_warmup_50000_iter_1e+05.rds"))
fit_eseal <- readRDS(file.path(input_dir, "Northern_elephant_seal/fit_warmup_50000_iter_1e+05.rds"))

original_sealion <- read.csv("data/confidential/input_data/input_final.csv") %>%
  filter(species == "California_sea_lion")
original_hseal <- read.csv("data/confidential/input_data/input_final.csv") %>%
  filter(species == "CA_Harbor_seal")
original_eseal <- read.csv("data/confidential/input_data/input_final.csv") %>%
  filter(species == "Northern_elephant_seal")

# check pair plots
pairs(fit_sealion, pars = c("r_1","k_1", "P_initial_1"))
pairs(fit_hseal, pars = c("r_1","k_1", "P_initial_1"))
pairs(fit_eseal, pars = c("r_1","k_1", "P_initial_1"))

# abundance

# CA sealion
output_sealion_clean <- output_sealion %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1975:2014) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(N_over_K = ifelse(estimation_type == "mean", estimation/327278, NA)) %>%
  mutate(species = "California sea lion")

original_sealion_clean <- original_sealion %>%
  select(year, abundance, catch) %>%
  mutate(abundance = ifelse(abundance == "-999", NA, abundance)) %>%
  filter(year <= 2014)

# CA Harbor seal
output_hseal_clean <- output_hseal %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1981:2012) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(N_over_K = ifelse(estimation_type == "mean", estimation/34546, NA)) %>%
  mutate(species = "Harbor seal (California)")

original_hseal_clean <- original_hseal %>%
  select(year, abundance, catch) %>%
  mutate(abundance = ifelse(abundance == "-999", NA, abundance)) %>%
  filter(year <= 2012)


# Northern elephant seal 
output_eseal_clean <- output_eseal %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1958:2013) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(N_over_K = ifelse(estimation_type == "mean", estimation/150475, NA)) %>%
  mutate(species = "Northern elephant seal (California)")

original_eseal_clean <- original_eseal %>%
  select(year, abundance, catch) %>%
  mutate(abundance = ifelse(abundance == "-999", NA, abundance)) %>%
  filter(year <= 2013)

### plot abundance ###

plot_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=8),
                    strip.text = element_text(size=8),
                    plot.title = element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    panel.border = element_rect(colour = "black", fill = NA),
                    axis.line = element_line(colour = "black"),
                    strip.background = element_rect(colour = "black", fill ="grey90"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))


g_abundance_sealion <- ggplot() +
  geom_line(data = output_sealion_clean %>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_point(data = output_sealion_clean %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation, fill = N_over_K, stroke = 0), size = 2, shape = 21) +
  geom_point(data = original_sealion_clean, aes(x = year, y = abundance), color = "blue", size = 2, shape = 1) +
  geom_line(data = original_sealion_clean, aes(x = year, y = catch), color = "red") +
  facet_wrap(.~species) + 
  scale_x_continuous(breaks = seq(1975, 2014, by = 10)) +
  scale_y_continuous(labels = scales::comma) + 
  paletteer::scale_fill_paletteer_c("grDevices::RdYlGn", name = "N / K", limits = c(0, 1)) + 
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme + theme(legend.position = "none")

g_abundance_sealion 

g_abundance_hseal <- ggplot() +
  geom_line(data = output_hseal_clean %>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_point(data = output_hseal_clean %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation, fill = N_over_K, stroke = 0), size = 2, shape = 21) +
  geom_point(data = original_hseal_clean, aes(x = year, y = abundance), color = "blue", size = 2, shape = 1) +
  geom_line(data = original_hseal_clean, aes(x = year, y = catch), color = "red") +
  facet_wrap(.~species) + 
  scale_x_continuous(breaks = seq(1981, 2012, by = 10)) +
  scale_y_continuous(labels = scales::comma) + 
  paletteer::scale_fill_paletteer_c("grDevices::RdYlGn", name = "N / K", limits = c(0, 1)) + 
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme + theme(legend.position = "none")

g_abundance_hseal

g_abundance_eseal <- ggplot() +
  geom_line(data = output_eseal_clean %>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_point(data = output_eseal_clean %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation, fill = N_over_K, stroke = 0), size = 2, shape = 21) +
  geom_point(data = original_eseal_clean, aes(x = year, y = abundance), color = "blue", size = 2, shape = 1) +
  geom_line(data = original_eseal_clean, aes(x = year, y = catch), color = "red") +
  facet_wrap(.~species) + 
  scale_x_continuous(breaks = seq(1958, 2013, by = 10)) +
  scale_y_continuous(labels = scales::comma) + 
  paletteer::scale_fill_paletteer_c("grDevices::RdYlGn", name = "N / K", limits = c(0, 1)) + 
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme + theme(legend.position = "none")

g_abundance_eseal

### save plot ###

plot_dir <- "figures"

ggsave(g_abundance_sealion, filename=file.path(plot_dir, "sealion_abd_est.png"), 
       width=5, height=4, units="in", dpi=600)

ggsave(g_abundance_hseal, filename=file.path(plot_dir, "ca_harbor_seal_abd_est.png"), 
       width=5, height=4, units="in", dpi=600)

ggsave(g_abundance_eseal, filename=file.path(plot_dir, "northern_elephant_seal_abd_est.png"), 
       width=5, height=4, units="in", dpi=600)




