
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)
library(rstan)


### for parallelization
options(mc.cores = parallel::detectCores())

### Compile the stan model
SPM_stan = stan_model(file = "model/vary_p_temp.stan")

# ---- 1. Load DATA ----
all_data <- read.csv("data/confidential/input_data/input_final.csv")
input_df <- subset(all_data, species == "Northern_elephant_seal")

#min_year <- min(input_df$year[input_df$catch >= 0], na.rm = TRUE)
#max_year <- max(input_df$year[input_df$abundance != -999], na.rm = TRUE)
min_year <- 1975
max_year <- 2014
abundance <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$abundance
catch     <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$catch
sigma_true <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$sigma
environment_true <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$pdo_scaled_all_year
z_true <- 3.93

# ---- 2. Stan Data ----
stan_data <- list(
  N_1 = max_year - min_year + 1,
  Abundance_1 = abundance,
  Catch_1 = catch,
  low_r = 0.01,
  high_r = 0.2,
  low_k = 0.8*max(abundance),
  high_k = 3*max(abundance),
  sigma_1 = sigma_true,
  Environment_1 = environment_true,
  z_1 = z_true
)

# ---- 3. Warmup Settings ----
warmup_values <- 50000
samples_per_chain <- 50000
chains <- 3
thin <- 10

fit_SPM_stan <- rstan::sampling(
  object = SPM_stan,
  data = stan_data,
  seed = 1207,
  chains = chains,
  iter = warmup_values + samples_per_chain,
  warmup = warmup_values,
  thin = thin,
  refresh = 200,
  control = list(
    adapt_delta = 0.99,
    max_treedepth = 20
  )
)

pairs(fit_SPM_stan, pars = c("r_1", "k_1", "P_initial_1", "impact_E_1"))
sl_posterior_temp <- rstan::extract(fit_SPM_stan, permuted = FALSE)
bayesplot::mcmc_trace(sl_posterior_temp, pars = c("r_1", "k_1", "P_initial_1", "impact_E_1"))

