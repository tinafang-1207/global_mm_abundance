
### clean working environment
rm(list = ls())

### import library
library(tidyverse)
library(rstan)

### Compile the stan model
SPM_stan = stan_model(file = "model/no_est_catch_simulation.stan")

### import the data frame
input_df <- read.csv("data/clean_data/ca_sealion_input_df.csv") %>%
  filter(total_catch != -999)

### specify data

min_year = min(input_df$year[which(input_df$abundance>0)])
max_year = max(input_df$year[which(input_df$abundance>0)])
abundance = input_df[input_df$year>=min_year&input_df$year<=max_year,]$abundance
catch = input_df[input_df$year>=min_year&input_df$year<=max_year,]$total_catch

# the max abundance estimated here is higher than the estimated carrying capacity (k) in laake et al., 2018

stan_data_0.6k <- list(
  N_1 = max_year-min_year+1,
  Abundance_1 = abundance,
  Catch_1 = catch,
  low_r = 0.01,
  high_r = 0.2,
  low_k = 0.8*max(abundance),
  high_k = 10*max(abundance),
  z_1=2.39
)

stan_data_0.8k <- list(
  N_1 = max_year-min_year+1,
  Abundance_1 = abundance,
  Catch_1 = catch,
  low_r = 0.1,
  high_r = 0.2,
  low_k = max(abundance),
  high_k = 10*max(abundance),
  z_1=11.2
)



chain = 4; iter = 2000; warmup = 1000; thin=1

fit_SPM_stan <- sampling(SPM_stan,
                         data = stan_data_0.8k,
                         chain = chain,
                         iter = iter,
                         warmup = warmup,
                         cores = chain,
                         thin = thin,
                         control = list(adapt_delta = 0.999, max_treedepth = 15))


fit_prior <- sampling(
  SPM_stan,
  algorithm = "Fixed_param",
  data = stan_data,
  chain = chain,
  iter = iter,
  warmup = warmup,
  cores = chain,
  thin = thin,
  control = list(adapt_delta = 0.999, max_treedepth = 15))

### Extract data ###

output <- as.data.frame(summary(fit_SPM_stan)$summary)


df_of_draws <- as.data.frame(fit_SPM_stan)


### prior samples ###
prior <- extract(fit_prior)

prior_df <- data.frame(
  r_1 = prior$r_1_prior_sample,
  k_1 = prior$k_1_prior_sample,
  P_initial_1 = prior$P_initial_1_prior_sample,
  sigma_sq_1 = prior$sigma_sq_1_prior_sample,
  tau_sq_1 = prior$tau_sq_1_prior_sample,
  m_1 = prior$m_1_prior_sample
)

# (Optional) If you also want to include F_1 samples:
# For example, F_1_sample is a matrix with one row per draw and one column per time step
F_1_df <- as.data.frame(prior$F_1_prior_sample)
colnames(F_1_df) <- paste0("F_1_t", seq_len(ncol(F_1_df)))

# Combine everything
prior_df_full <- cbind(prior_df, F_1_df)



### Traceplot ###

traceplot(fit_SPM_stan, pars = c("r_1", "k_1", "P_initial_1", "sigma_sq_1", "tau_sq_1", "m_1"))

saveRDS(fit_SPM_stan, "data/exp_data/fit_ca_sealion_0.6k.rds")
write.csv(output, "data/exp_data/output_ca_sealion_0.6k.csv")
write.csv(df_of_draws, "data/exp_data/posterior_draw_ca_sealion_0.6k.csv")

saveRDS(fit_SPM_stan, "data/exp_data/fit_ca_sealion_0.8k.rds")
write.csv(output, "data/exp_data/output_ca_sealion_0.8k.csv")
write.csv(df_of_draws, "data/exp_data/posterior_draw_ca_sealion_0.8k.csv")
