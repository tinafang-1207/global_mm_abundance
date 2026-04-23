
# read in libraries

### import library
library(tidyverse)
library(rstan)
library(parallel)

### for parallelization
options(mc.cores = parallel::detectCores())

### Compile the stan model
SPM_stan = stan_model(file = "model/vary_p_no_temp.stan")
# SPM_stan = stan_model(file = "model/vary_p_temp_R.stan)
# SPM_stan = stan_model(file = "model/vary_p_temp_K.stan)

input_df <- read.csv("data/confidential/input_data/sl_sample_data_Tim.csv")

min_year <- 1981 # start with 1981 due to missing catch data from 1975-1981
max_year <- 2014 
abundance = input_df[input_df$year>=min_year&input_df$year<=max_year,]$abundance
catch = input_df[input_df$year>=min_year&input_df$year<=max_year,]$catch
sigma_true <-input_df[input_df$year>=min_year&input_df$year<=max_year,]$sigma # observer error
r_approx = 0.12 # default maximum growth rate of sl 
k_approx = max(abundance) # change depending on the estimated stock status
N_init_approx = abundance[1] 
z_true <- 3.93 # reflection point theta
# for environmental effects model
environment_true <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$pdo_scaled_all_year


stan_data <- list(
  N_1 = max_year - min_year + 1,
  Abundance_1 = abundance,
  Catch_1 = catch,
  N_init_approx = N_init_approx,
  r_approx = r_approx,
  k_approx = k_approx,
  sigma_1 = sigma_true,
  # Environment_1 = environment_true,
  z_1 = z_true
)

warmup_values <- 50000 # change to smaller values to run faster
samples_per_chain <- 50000
chains <- 3
thin <- 10

# fit model

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

# pair plots 

pairs(fit_SPM_stan, pars = c("r_1", "k_1", "N_init_1"))






