
### clean working environment
rm(list = ls())

### import library
library(tidyverse)
library(rstan)

### Compile the stan model
SPM_stan = stan_model(file = "model/ca_sealion_exp.stan")

### import the data frame
input_df <- read.csv("data/clean_data/ca_sealion_input_df.csv")

### specify data

#min_year = min(input_df$year[which(input_df$abundance>0)])
#max_year = max(input_df$year[which(input_df$abundance>0)])

min_year = 1975
max_year = 2022
abundance = input_df[input_df$year>=min_year&input_df$year<=max_year,]$abundance
catch = input_df[input_df$year>=min_year&input_df$year<=max_year,]$incidental_mortality

# the max abundance estimated here is higher than the estimated carrying capacity (k) in laake et al., 2018

stan_data <- list(
  N_1 = max_year-min_year+1,
  Catch_1 = catch,
  Abundance_1 = abundance,
  k_1_prior = c(max(abundance)*0.8, max(abundance)*2),
  r_1_prior = c(0.01, 0.25)
)


chain = 3; iter = 2000; warmup = 1000; thin=1

fit_SPM_stan <- sampling(SPM_stan,
                         data = stan_data,
                         chain = chain,
                         iter = iter,
                         warmup = warmup,
                         cores = chain,
                         thin = thin,
                         control = list(adapt_delta = 0.999, max_treedepth = 12))

output <- as.data.frame(summary(fit_SPM_stan)$summary)


df_of_draws <- as.data.frame(fit_SPM_stan)


#save the result
#k = 1.5max(abundance)
write.csv(df_of_draws, "data/exp_data/posterior_draw.csv")
write.csv(output, "data/exp_data/output.csv")

#k = 2max(abundance)
write.csv(df_of_draws, "data/exp_data/posterior_draw_k_2.csv")
write.csv(output, "data/exp_data/output_k_2.csv")

# all years
write.csv(df_of_draws, "data/exp_data/posterior_draw_all_year.csv")
write.csv(output, "data/exp_data/output_all_year.csv")



