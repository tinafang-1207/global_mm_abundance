
### load in library ###
library(tidyverse)
library(bayesplot)

### set up input directory ###
input_dir <- "data/confidential/stan_output/"

### import data ###
# output_sealion <- read.csv(file.path(input_dir, "California_sea_lion/summary_warmup_50000_iter_1e+05.csv"))
# output_sealion_pdo <- read.csv(file.path(input_dir, "California_sea_lion/summary_warmup_50000_iter_1e+05_temp.csv"))
# output_sealion_pdo_k <- read.csv(file.path(input_dir, "California_sea_lion/summary_warmup_50000_iter_1e+05_temp_k.csv"))
# output_sealion_habitat <- read.csv(file.path(input_dir, "California_sea_lion/summary_warmup_50000_iter_1e+05_temp_habitat.csv"))
output_hseal <- read.csv(file.path(input_dir, "CA_Harbor_seal/summary_warmup_50000_iter_1e+05.csv"))
output_eseal <- read.csv(file.path(input_dir, "Northern_elephant_seal/summary_warmup_50000_iter_1e+05.csv"))
output_blue <- read.csv(file.path(input_dir, "Blue_whale/summary_warmup_50000_iter_1e+05.csv"))


fit_sealion <- readRDS(file.path(input_dir, "California_sea_lion/fit_warmup_50000_iter_1e+05.rds"))
fit_sealion_temp <- readRDS(file.path(input_dir, "California_sea_lion/fit_warmup_50000_iter_1e+05_temp.rds"))
fit_sealion_habitat <- readRDS(file.path(input_dir, "California_sea_lion/fit_warmup_50000_iter_1e+05_temp_habitat.rds"))
fit_sealion_temp_k <- readRDS(file.path(input_dir, "California_sea_lion/fit_warmup_50000_iter_1e+05_temp_k.rds"))


fit_hseal <- readRDS(file.path(input_dir, "CA_Harbor_seal/fit_warmup_50000_iter_1e+05.rds"))
fit_hseal_temp <- readRDS(file.path(input_dir,"CA_Harbor_seal/fit_warmup_50000_iter_1e+05_temp.rds"))
fit_eseal <- readRDS(file.path(input_dir, "Northern_elephant_seal/fit_warmup_50000_iter_1e+05.rds"))
fit_eseal_temp <- readRDS(file.path(input_dir, "Northern_elephant_seal/fit_warmup_50000_iter_1e+05_temp.rds"))
fit_eseal_habitat <- readRDS(file.path(input_dir, "Northern_elephant_seal/fit_warmup_50000_iter_1e+05_temp_habitat.rds"))

# fit blue whale
fit_blue <-readRDS(file.path(input_dir, "Blue_whale/fit_warmup_50000_iter_1e+05.rds"))

# Model experimental data
output_sealion <- read.csv(file.path(input_dir, "California_sea_lion/summary_warmup_50000_iter_1e+05_exp.csv"))
fit_sealion <- readRDS(file.path(input_dir, "California_sea_lion/fit_warmup_50000_iter_1e+05_exp.rds"))
output_sealion_temp <- read.csv(file.path(input_dir, "California_sea_lion/summary_warmup_50000_iter_1e+05_temp_exp.csv"))
fit_sealion_temp <- readRDS(file.path(input_dir, "California_sea_lion/fit_warmup_50000_iter_1e+05_temp_exp.rds"))
output_sealion_temp_k <- read.csv(file.path(input_dir, "California_sea_lion/summary_warmup_50000_iter_1e+05_temp_k_exp.csv"))
fit_sealion_temp_k <- readRDS(file.path(input_dir, "California_sea_lion/fit_warmup_50000_iter_1e+05_temp_k_exp.rds"))

output_hseal <- read.csv(file.path(input_dir, "CA_Harbor_seal/summary_warmup_50000_iter_1e+05_exp.csv"))
fit_hseal <- readRDS(file.path(input_dir, "CA_Harbor_seal/fit_warmup_50000_iter_1e+05_exp.rds"))
output_hseal_temp <- read.csv(file.path(input_dir, "CA_Harbor_seal/summary_warmup_50000_iter_1e+05_temp_r_exp.csv"))
fit_hseal_temp <- readRDS(file.path(input_dir, "CA_Harbor_seal/fit_warmup_50000_iter_1e+05_temp_r_exp.rds"))
output_hseal_temp_k <- read.csv(file.path(input_dir, "CA_Harbor_seal/summary_warmup_50000_iter_1e+05_temp_k_exp.csv"))
fit_hseal_temp_k <- readRDS(file.path(input_dir, "CA_Harbor_seal/fit_warmup_50000_iter_1e+05_temp_k_exp.rds"))

output_eseal <- read.csv(file.path(input_dir, "Northern_elephant_seal/summary_warmup_50000_iter_1e+05_exp.csv"))
fit_eseal <- readRDS(file.path(input_dir, "Northern_elephant_seal/fit_warmup_50000_iter_1e+05_exp.rds"))
output_eseal_temp <- read.csv(file.path(input_dir, "Northern_elephant_seal/summary_warmup_50000_iter_1e+05_temp_r_exp.csv"))
fit_eseal_temp <- readRDS(file.path(input_dir, "Northern_elephant_seal/fit_warmup_50000_iter_1e+05_temp_r_exp.rds"))
output_eseal_temp_k <- read.csv(file.path(input_dir, "Northern_elephant_seal/summary_warmup_50000_iter_1e+05_temp_k_exp.csv"))
fit_eseal_temp_k <- readRDS(file.path(input_dir, "Northern_elephant_seal/fit_warmup_50000_iter_1e+05_temp_k_exp.rds"))





# abundance survey data
data_orig <- read.csv("data/confidential/input_data/input_final.csv")

# read function
source("code/data_exploration/helper_function/model_evaluation.R")
source("code/data_exploration/helper_function/posterior_predictive_check.R")
source("code/data_exploration/helper_function/abundance_trajectory.R")

####################################################################################
# check pair plots
pairs(fit_sealion, pars = c("r_1","k_1", "N_init_1"))
pairs(fit_sealion_temp, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))
pairs(fit_sealion_temp_k, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))


pairs(fit_hseal, pars = c("r_1","k_1", "N_init_1"))
pairs(fit_hseal_temp, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))
pairs(fit_hseal_temp_k, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))

pairs(fit_eseal, pars = c("r_1","k_1", "N_init_1"))
pairs(fit_eseal_temp, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))
pairs(fit_eseal_temp_k, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))

# check trace plots

# California sea lion
sl_posterior_temp <- rstan::extract(fit_sealion_temp, permuted = FALSE)
mcmc_trace(sl_posterior_temp, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))
sl_posterior_temp_k <- rstan::extract(fit_sealion_temp_k, permuted = FALSE)
mcmc_trace(sl_posterior_temp_k, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))

# California harbor seal
hseal_posterior_temp <- rstan::extract(fit_hseal_temp, permuted = FALSE)
mcmc_trace(hseal_posterior_temp,pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))
hseal_posterior_temp_k <- rstan::extract(fit_hseal_temp_k, permuted = FALSE)
mcmc_trace(hseal_posterior_temp_k, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))

# Northern elephant seal
eseal_posterior_temp <- rstan::extract(fit_eseal_temp, permuted = FALSE)
mcmc_trace(eseal_posterior_temp, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))
eseal_posterior_temp_k <- rstan::extract(fit_eseal_temp_k, permuted = FALSE)
mcmc_trace(eseal_posterior_temp_k, pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))




####################################################################################
# Model Evaluation (which model is better?)

model_list_sealion <- list(
  null = fit_sealion,
  pdo = fit_sealion_temp,
  pdo_k = fit_sealion_temp_k
)

model_list_eseal <- list(
  null = fit_eseal,
  pdo = fit_eseal_temp,
  pdo_k = fit_eseal_temp_k
)

model_list_hseal <- list(
  null = fit_hseal,
  pdo = fit_hseal_temp,
  pdo_k = fit_hseal_temp_k
)

model_eval_table <- make_model_eval_table(
  model_list = model_list_sealion,
  data_orig = data_orig,
  species_name = "California_sea_lion",
  min_model_year = 1981,
  max_model_year = 2014,
  round_digits = 2
)

model_eval_table <- make_model_eval_table(
  model_list = model_list_hseal,
  data_orig = data_orig,
  species_name = "CA_harbor_seal",
  min_model_year = 1984,
  max_model_year = 2012,
  round_digits = 2
)

model_eval_table <- make_model_eval_table(
  model_list = model_list_eseal,
  data_orig = data_orig,
  species_name = "Northern_elephant_seal",
  min_model_year = 1981,
  max_model_year = 2013,
  round_digits = 2
)

model_eval_table

# log_lik_mat <- rstan::extract(fit_eseal_habitat, pars = "log_lik")$log_lik
# y_obs_full <- data_orig %>% 
# #   filter(species == "Northern_elephant_seal") %>% 
# #   filter(year >= 1981 & year <= 2013) %>%
# #   pull(abundance)
# # obs_idx <- which(y_obs_full > -1)
# # 
# # log_lik_obs <- log_lik_mat[, obs_idx, drop = FALSE]
# # loo_res <- loo::loo(log_lik_obs)
# # pareto_k <- loo::pareto_k_values(loo_res)
# # pareto_k

###############################################################################################
# Posterior predictive check (check the fit of the data to the predictive model)

plot_ppc_check(fit_eseal_habitat, "Northern_elephant_seal", min_model_year = 1981, max_model_year = 2013)
plot_ppc_check(fit_sealion_habitat, "California_sea_lion", min_model_year = 1975, max_model_year = 2014)

###############################################################################################
# abundance trajectory

# California sea lion

g_abundance_sealion <- plot_abundance(fit = fit_sealion,
               spp = "California_sea_lion",
               output_spp = output_sealion,
               data_orig = data_orig,
               min_model_year = 1981,
               max_model_year = 2014,
               z_val = 3.93)

g_abundance_sealion_temp <- plot_abundance(fit = fit_sealion_temp,
               spp = "California_sea_lion",
               output_spp = output_sealion_temp,
               data_orig = data_orig,
               min_model_year = 1981,
               max_model_year = 2014,
               z_val = 3.93)

g_abundance_sealion_temp_k <- plot_abundance(fit = fit_sealion_temp_k,
                                             spp = "California_sea_lion",
                                             output_spp = output_sealion_temp_k,
                                             data_orig = data_orig,
                                             min_model_year = 1981,
                                             max_model_year = 2014,
                                             z_val = 3.93)


# CA harbor seal

g_abundance_hseal <- plot_abundance(fit = fit_hseal,
               spp = "CA_harbor_seal",
               output_spp = output_hseal,
               data_orig = data_orig,
               min_model_year = 1984,
               max_model_year = 2012,
               z_val = 6.1)

g_abundance_hseal_temp <- plot_abundance(fit = fit_hseal_temp,
                                           spp = "CA_harbor_seal",
                                           output_spp = output_hseal_temp,
                                           data_orig = data_orig,
                                           min_model_year = 1984,
                                           max_model_year = 2012,
                                           z_val = 6.1)

g_abundance_hseal_temp_k <- plot_abundance(fit = fit_hseal_temp_k,
                                           spp = "CA_harbor_seal",
                                           output_spp = output_hseal_temp_k,
                                           data = data_orig,
                                           min_model_year = 1984,
                                           max_model_year = 2012,
                                           z_val = 6.1)

# Northern elephant seal

g_abundance_eseal <- plot_abundance(fit = fit_eseal,
                                    spp = "Northern_elephant_seal",
                                    output_spp = output_eseal,
                                    data_orig = data_orig,
                                    min_model_year = 1981,
                                    max_model_year = 2013,
                                    z_val = 1.9)

g_abundance_eseal_temp <- plot_abundance(fit = fit_eseal_temp,
                                         spp = "Northern_elephant_seal",
                                         output_spp = output_eseal_temp,
                                         data_orig = data_orig,
                                         min_model_year = 1981,
                                         max_model_year = 2013,
                                         z_val = 1.9)

g_abundance_eseal_temp_k <- plot_abundance(fit = fit_eseal_temp_k,
                                           spp = "Northern_elephant_seal",
                                           output_spp = output_eseal_temp_k,
                                           data_orig = data_orig,
                                           min_model_year = 1981,
                                           max_model_year = 2013,
                                           z_val = 1.9)



################################################################
# vary R


# ---- 2. Extract R_t rows ----
R_df <- output_eseal_temp %>%
  filter(grepl("^R_t\\[", X)) %>%   # adjust column name if needed
  mutate(
    year_index = as.numeric(gsub("R_t\\[|\\]", "", X)),
    year = 1981 + year_index - 1
  )

# ---- 3. Plot ----
g_R <- ggplot(R_df, aes(x = year, y = mean)) +
  geom_hline(yintercept = 0.082, 
             color = "gray40", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 0.087, color = "red", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), 
              fill = "steelblue", alpha = 0.3) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    x = "Year",
    y = "Temperature-adjusted growth rate (R)"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

g_R

K_df <- output_eseal_temp_k %>%
  filter(grepl("^K_t\\[", X)) %>%   # adjust column name if needed
  mutate(
    year_index = as.numeric(gsub("K_t\\[|\\]", "", X)),
    year = 1981 + year_index - 1
  )


g_K <- ggplot(K_df, aes(x = year, y = mean)) +
  geom_hline(yintercept = 187138, 
             color = "gray40", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 180341, color = "red", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = X2.5., ymax = X97.5.), 
              fill = "steelblue", alpha = 0.3) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    x = "Year",
    y = "Temperature-adjusted growth rate (K)"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

g_K


### save plot ###

plot_dir <- "figures"

ggsave(g_abundance_sealion, filename=file.path(plot_dir, "sealion_abd_est.png"), 
       width=8, height=5, units="in", dpi=600)
ggsave(g_abundance_sealion, filename = file.path(plot_dir, "sealion_abd_est_exp.png"),
       width = 8, height = 5, units = "in", dpi = 600)
ggsave(g_abundance_sealion_temp, filename=file.path(plot_dir, "sealion_abd_est_temp_exp.png"),
       width=8, height=5, units="in", dpi=600)
ggsave(g_abundance_sealion_temp_k, filename = file.path(plot_dir, "sealion_abd_est_temp_k_exp.png"),
       width=8, height=5, units = "in", dpi=600)


ggsave(g_abundance_hseal, filename = file.path(plot_dir, "hseal_abd_est_exp.png"),
       width = 8, height = 5, units = "in", dpi = 600)
ggsave(g_abundance_hseal_temp, filename=file.path(plot_dir, "hseal_abd_est_temp_exp.png"),
       width=8, height=5, units="in", dpi=600)
ggsave(g_abundance_hseal_temp_k, filename = file.path(plot_dir, "hseal_abd_est_temp_k_exp.png"),
       width=8, height=5, units = "in", dpi=600)

ggsave(g_abundance_eseal, filename = file.path(plot_dir, "eseal_abd_est_exp.png"),
       width = 8, height = 5, units = "in", dpi = 600)
ggsave(g_abundance_eseal_temp, filename=file.path(plot_dir, "eseal_abd_est_temp_exp.png"),
       width=8, height=5, units="in", dpi=600)
ggsave(g_abundance_eseal_temp_k, filename = file.path(plot_dir, "eseal_abd_est_temp_k_exp.png"),
       width=8, height=5, units = "in", dpi=600)

ggsave(g_R, filename=file.path(plot_dir, "Northern_elephant_seal_vary_R_exp.png"), 
       width=5, height=5, units="in", dpi=600)
ggsave(g_K, filename=file.path(plot_dir, "Northern_elephant_seal_vary_K_exp.png"), 
       width=5, height=5, units="in", dpi=600)





