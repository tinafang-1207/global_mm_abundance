
### load in library ###
library(tidyverse)
library(bayesplot)

### set up input directory ###
input_dir <- "data/confidential/stan_output/"

### import data ###
output_sealion <- read.csv(file.path(input_dir, "California_sea_lion/summary_warmup_50000_iter_1e+05.csv"))
output_sealion_habitat <- read.csv(file.path(input_dir, "California_sea_lion/summary_warmup_50000_iter_1e+05_temp_habitat.csv"))
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

# read function
source("code/data_exploration/helper_function/model_evaluation.R")
source("code/data_exploration/helper_function/posterior_predictive_check.R")
source("code/data_exploration/helper_function/abundance_trajectory.R")

####################################################################################
# check pair plots
pairs(fit_sealion, pars = c("r_1","k_1", "P_initial_1"))
pairs(fit_sealion_temp, pars = c("r_1", "k_1", "P_initial_1", "impact_E_1"))
pairs(fit_hseal, pars = c("r_1","k_1", "P_initial_1"))
pairs(fit_hseal_temp, pars = c("r_1", "k_1", "P_initial_1", "impact_E_1"))
pairs(fit_eseal, pars = c("r_1","k_1", "P_initial_1"))
pairs(fit_eseal_temp, pars = c("r_1", "k_1", "P_initial_1", "impact_E_1"))

####################################################################################
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
  model_list = model_list_eseal,
  data_orig = data_orig,
  species_name = "Northern_elephant_seal",
  round_digits = 2
)

model_eval_table

# log_lik_mat <- rstan::extract(fit_eseal_habitat, pars = "log_lik")$log_lik
# y_obs_full <- data_orig %>% 
#   filter(species == "Northern_elephant_seal") %>% 
#   filter(year >= 1981 & year <= 2013) %>%
#   pull(abundance)
# obs_idx <- which(y_obs_full > -1)
# 
# log_lik_obs <- log_lik_mat[, obs_idx, drop = FALSE]
# loo_res <- loo::loo(log_lik_obs)
# pareto_k <- loo::pareto_k_values(loo_res)
# pareto_k

###############################################################################################
# Posterior predictive check (check the fit of the data to the predictive model)

plot_ppc_check(fit_eseal_habitat, "Northern_elephant_seal", min_model_year = 1981, max_model_year = 2013)
plot_ppc_check(fit_sealion_habitat, "California_sea_lion", min_model_year = 1975, max_model_year = 2014)

###############################################################################################
# abundance trajectory

g_abundance_sealion <- plot_abundance(fit = fit_sealion,
               spp = "California_sea_lion",
               output_spp = output_sealion,
               data_orig = data_orig,
               min_model_year = 1975,
               max_model_year = 2014,
               z_val = 3.93)

plot_abundance(fit = fit_sealion_habitat,
               spp = "California_sea_lion",
               output_spp = output_sealion_habitat,
               data_orig = data_orig,
               min_model_year = 1975,
               max_model_year = 2014,
               z_val = 3.93)


g_abundance_eseal <- plot_abundance(fit = fit_eseal,
               spp = "Northern_elephant_seal",
               output_spp = output_eseal,
               data_orig = data_orig,
               min_model_year = 1981,
               max_model_year = 2013,
               z_val = 1.9)

g_abundance_hseal <- plot_abundance(fit = fit_hseal,
               spp = "CA_harbor_seal",
               output_spp = output_hseal,
               data_orig = data_orig,
               min_model_year = 1981,
               max_model_year = 2012,
               z_val = 10.1)



### save plot ###

plot_dir <- "figures"

ggsave(g_abundance_sealion, filename=file.path(plot_dir, "sealion_abd_est.png"), 
       width=8, height=5, units="in", dpi=600)

ggsave(g_abundance_hseal, filename=file.path(plot_dir, "ca_harbor_seal_abd_est.png"), 
       width=8, height=5, units="in", dpi=600)

ggsave(g_abundance_eseal, filename=file.path(plot_dir, "northern_elephant_seal_abd_est.png"), 
       width=8, height=5, units="in", dpi=600)




