
library(rstan)
library(tidyverse)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
input_df <- read.csv("data/confidential/input_data/input_final.csv") %>%
  filter(species == "Northern_elephant_seal")

min_year <- min(input_df$year[input_df$catch >= 0], na.rm = TRUE)
max_year <- max(input_df$year[input_df$abundance != -999], na.rm = TRUE)
abundance = input_df[input_df$year>=min_year&input_df$year<=max_year,]$abundance
catch = input_df[input_df$year>=min_year&input_df$year<=max_year,]$catch
sigma_true <-input_df[input_df$year>=min_year&input_df$year<=max_year,]$sigma
environment_true <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$temp_scaled_all_year
z_true <- 10.1


# Symmetric uniform prior of impact_E
E_min <- min(environment_true, na.rm = TRUE)
E_max <- max(environment_true, na.rm = TRUE)

bound_pos <- log(1.5) / E_max
bound_neg <- abs(log(0.5)) / abs(E_min)

impact_bound <- min(bound_pos, bound_neg)

low_impact  <- -impact_bound
high_impact <-  impact_bound

# Non-symmetric uniform prior of impact_E
E_min <- min(environment_true, na.rm = TRUE)
E_max <- max(environment_true, na.rm = TRUE)

low_impact <- max(log(1.5) / E_min,
                  log(0.5) / E_max)

high_impact <- min(log(1.5) / E_max,
                   log(0.5) / E_min)

c(low_impact, high_impact)






stan_data_prior <- list(
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

mod_prior <- stan_model("model/prior_predictive_temp.stan")

fit_prior <- sampling(
  object = mod_prior,
  data = stan_data_prior,
  iter = 2000,
  chains = 4,
  seed = 123,
  refresh = 100,
  algorithm = "Fixed_param"
)

post <- rstan::extract(fit_prior)

# dimensions
dim(post$N_med)          # iterations × time
dim(post$Abundance_pred)
length(post$impact_E_1)


set.seed(123)
draw_id <- sample(1:nrow(post$N_med), 100)

traj_df <- map_dfr(draw_id, function(i) {
  tibble(
    draw = i,
    time = 1:ncol(post$N_med),
    N_med = post$N_med[i, ]
  )
})

ggplot(traj_df, aes(x = time, y = N_med, group = draw)) +
  geom_line(alpha = 0.25) +
  scale_y_log10() +   # IMPORTANT: use log scale
  theme_bw() +
  labs(title = "Prior predictive trajectories (log scale)")

summarize_prior <- function(post, prior_label, floor = 0.0001) {
  
  # ---- Floor indicator ----
  hit_floor <- apply(post$N_med, 1, function(x) any(x <= floor + 1e-12))
  floor_counts <- apply(post$N_med, 1, function(x) sum(x <= floor + 1e-12))
  floor_by_time <- colMeans(post$N_med <= floor + 1e-12)
  
  # ---- 1. Overall summary (1 row) ----
  summary_df <- tibble(
    prior = prior_label,
    prop_hit_floor = mean(hit_floor),
    mean_years_at_floor = mean(floor_counts),
    median_years_at_floor = median(floor_counts),
    q25_years_at_floor = quantile(floor_counts, 0.25),
    q75_years_at_floor = quantile(floor_counts, 0.75),
    max_years_at_floor = max(floor_counts)
  )
  
  # ---- 2. Time series summary ----
  time_df <- tibble(
    prior = prior_label,
    time = 1:length(floor_by_time),
    prop_floor = floor_by_time
  )
  
  return(list(summary = summary_df, time = time_df))
}

res_norm2 <- summarize_prior(post, prior_label = "normal(0,2)")
summary_norm2 <- res_norm2$summary
time_norm2 <- res_norm2$time

res_norm0.5 <- summarize_prior(post, prior_label = "normal(0,0.5)")
summary_norm0.5 <- res_norm0.5$summary
time_norm0.5 <- res_norm0.5$time

res_norm0.1 <- summarize_prior(post, prior_label = "normal(0,0.1)")
summary_norm0.1 <- res_norm0.1$summary
time_norm0.1 <- res_norm0.1$time


###########################################################

plot_df <- input_df %>%
  select(year, pdo_scaled_all_year, temp_scaled_all_year) %>%
  mutate(pdo_state = ifelse(pdo_scaled_all_year > 0, "warm", "cold")) %>%
  mutate(temp_state = ifelse(temp_scaled_all_year > 0, "warm", "cold"))






