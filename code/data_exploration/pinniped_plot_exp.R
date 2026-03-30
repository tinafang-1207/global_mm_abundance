
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
fit_sealion_temp <- readRDS(file.path(input_dir, "California_sea_lion/fit_warmup_50000_iter_1e+05_temp.rds"))
fit_sealion_habitat <- readRDS(file.path(input_dir, "California_sea_lion/fit_warmup_50000_iter_1e+05_temp_habitat.rds"))
fit_hseal <- readRDS(file.path(input_dir, "CA_Harbor_seal/fit_warmup_50000_iter_1e+05.rds"))
fit_hseal_temp <- readRDS(file.path(input_dir,"CA_Harbor_seal/fit_warmup_50000_iter_1e+05_temp.rds"))
fit_eseal <- readRDS(file.path(input_dir, "Northern_elephant_seal/fit_warmup_50000_iter_1e+05.rds"))
fit_eseal_temp <- readRDS(file.path(input_dir, "Northern_elephant_seal/fit_warmup_50000_iter_1e+05_temp.rds"))
fit_eseal_habitat <- readRDS(file.path(input_dir, "Northern_elephant_seal/fit_warmup_50000_iter_1e+05_temp_habitat.rds"))


data_orig <- read.csv("data/confidential/input_data/input_final.csv")
original_sealion <- read.csv("data/confidential/input_data/input_final.csv") %>%
  filter(species == "California_sea_lion") %>%
  filter(year <= 2014)
original_hseal <- read.csv("data/confidential/input_data/input_final.csv") %>%
  filter(species == "CA_Harbor_seal")
original_eseal <- read.csv("data/confidential/input_data/input_final.csv") %>%
  filter(species == "Northern_elephant_seal")

# read function
source("code/data_exploration/helper_function/model_evaluation.R")
source("code/data_exploration/helper_function/posterior_predictive_check.R")

####################################################################################
# check pair plots
pairs(fit_sealion, pars = c("r_1","k_1", "P_initial_1"))
pairs(fit_sealion_temp, pars = c("r_1", "k_1", "P_initial_1", "impact_E_1"))
pairs(fit_hseal, pars = c("r_1","k_1", "P_initial_1"))
pairs(fit_hseal_temp, pars = c("r_1", "k_1", "P_initial_1", "impact_E_1"))
pairs(fit_eseal, pars = c("r_1","k_1", "P_initial_1"))
pairs(fit_eseal_temp, pars = c("r_1", "k_1", "P_initial_1", "impact_E_1"))


# Model Evaluation (which model is better?)

model_list_sealion <- list(
  null = fit_sealion,
  pdo = fit_sealion_temp,
  habitat = fit_sealion_habitat
)

model_list_eseal <- list(
  null = fit_eseal,
  pdo = fit_eseal_temp,
  habitat = fit_eseal_habitat
)

model_eval_table <- make_model_eval_table(
  model_list = model_list_sealion,
  data_orig = data_orig,
  species_name = "California_sea_lion",
  round_digits = 2
)

model_eval_table

log_lik_mat <- rstan::extract(fit_eseal_habitat, pars = "log_lik")$log_lik
y_obs_full <- data_orig %>% 
  filter(species == "Northern_elephant_seal") %>% 
  filter(year >= 1981 & year <= 2013) %>%
  pull(abundance)
obs_idx <- which(y_obs_full > -1)

log_lik_obs <- log_lik_mat[, obs_idx, drop = FALSE]
loo_res <- loo::loo(log_lik_obs)
pareto_k <- loo::pareto_k_values(loo_res)
pareto_k

# Posterior predictive check (check the fit of the data to the predictive model)

plot_ppc_check(fit_eseal_habitat, "Northern_elephant_seal", min_model_year = 1981, max_model_year = 2013)
plot_ppc_check(fit_sealion_habitat, "California_sea_lion", min_model_year = 1975, max_model_year = 2014)


# abundance trajectory

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
  mutate(est_variables = 1981:2013) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(N_over_K = ifelse(estimation_type == "mean", estimation/190639, NA)) %>%
  mutate(species = "Northern elephant seal (California)")

original_eseal_clean <- original_eseal %>%
  select(year, abundance, catch) %>%
  mutate(abundance = ifelse(abundance == "-999", NA, abundance)) %>%
  filter(year >= 1981 & year <= 2013)

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
  scale_x_continuous(breaks = seq(1981, 2013, by = 10)) +
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




