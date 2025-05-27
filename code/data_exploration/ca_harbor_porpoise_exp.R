
### clean working environment
rm(list = ls())

### import library
library(tidyverse)
library(rstan)

### Compile the stan model
SPM_stan = stan_model(file = "model/ca_sealion_exp.stan")

### import the data frame
input_df <- read.csv("data/clean_data/ca_harbor_porpoise_input_df.csv") %>%
  filter(stock == "San Francisco - Russian River") %>%
  mutate(total_catch = ifelse(total_catch == 0, -999, total_catch))

# Specify data

min_year = min(input_df$year[which(input_df$abundance>0)])
max_year = max(input_df$year[which(input_df$abundance>0)])

abundance = input_df[input_df$year>=min_year&input_df$year<=max_year,]$abundance
catch = input_df[input_df$year>=min_year&input_df$year<=max_year,]$total_catch

stan_data <- list(
  N_1 = max_year-min_year+1,
  Catch_1 = catch,
  Abundance_1 = abundance,
  k_1_prior = c(max(abundance), max(abundance)*2),
  r_1_prior = c(0.01, 0.1)
)


chain = 3; iter = 2000; warmup = 1000; thin=1

fit_SPM_stan <- sampling(SPM_stan,
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

### Traceplot ###

traceplot(fit_SPM_stan, pars = c("r_1", "k_1", "P_initial_1", "sigma_sq_1", "tau_sq_1", "m_1"))

### Examine correlation between r and k ###
posterior_matrix = as.matrix(fit_SPM_stan)

bayesplot::mcmc_scatter(posterior_matrix, pars = c("r_1", "k_1"))

bayesplot::mcmc_pairs(posterior_matrix, pars = c("r_1", "k_1"))

# save data

write.csv(output, "data/exp_data/output_2k_hp_monterey.csv")
write.csv(df_of_draws, "data/exp_data/posterior_draw_2k_hp_morro.csv")









