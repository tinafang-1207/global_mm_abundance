
library(rstan)
library(tidyverse)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
input_df <- read.csv("data/confidential/input_data/input_final.csv") %>%
  filter(species == "California_sea_lion")

#min_year <- min(input_df$year[input_df$catch >= 0], na.rm = TRUE)
#max_year <- max(input_df$year[input_df$abundance != -999], na.rm = TRUE)

min_year = 1975
max_year = 2014
abundance = input_df[input_df$year>=min_year&input_df$year<=max_year,]$abundance
catch = input_df[input_df$year>=min_year&input_df$year<=max_year,]$catch
sigma_true <-input_df[input_df$year>=min_year&input_df$year<=max_year,]$sigma
r_approx = 0.12
N_init_approx = abundance[1]
environment_true <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$pdo_scaled_all_year
z_true <- 3.93


# Symmetric uniform prior of impact_E
# E_min <- min(environment_true, na.rm = TRUE)
# E_max <- max(environment_true, na.rm = TRUE)
# 
# bound_pos <- log(1.5) / E_max
# bound_neg <- abs(log(0.5)) / abs(E_min)
# 
# impact_bound <- min(bound_pos, bound_neg)
# 
# low_impact  <- -impact_bound
# high_impact <-  impact_bound
# 
# # Non-symmetric uniform prior of impact_E
# E_min <- min(environment_true, na.rm = TRUE)
# E_max <- max(environment_true, na.rm = TRUE)
# 
# low_impact <- max(log(1.5) / E_min,
#                   log(0.5) / E_max)
# 
# high_impact <- min(log(1.5) / E_max,
#                    log(0.5) / E_min)
# 
# c(low_impact, high_impact)



stan_data_prior <- list(
  N_1 = max_year - min_year + 1,
  Abundance_1 = abundance,
  Catch_1 = catch,
  r_approx = r_approx,
  N_init_approx = N_init_approx,
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
  seed = 1207,
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

res_norm2 <- summarize_prior(post, prior_label = "normal(0,1)")
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

##########################################################

library(ggplot2)

mu <- 0.1
sigma <- sqrt(0.001)

df <- data.frame(x = seq(0, 0.2, length.out = 1000))

ggplot(df, aes(x = x)) +
  stat_function(fun = dnorm,
                args = list(mean = mu, sd = sigma),
                linewidth = 1.2) +
  labs(title = "Prior Distribution: Normal(0.1, 0.001)",
       x = "Value", y = "Density") +
  theme_minimal()
########################################################

# Set seed
set.seed(123)

# Target mean and variance
m <- 0.1
v <- 0.001
sigma <- sqrt(v)

# Convert to log-normal parameters
sigma2_log <- log(1 + v / m^2)
sigma_log <- sqrt(sigma2_log)
mu_log <- log(m) - sigma2_log / 2

# Simulate
n <- 100000
normal_samples <- rnorm(n, mean = m, sd = sigma)
lognormal_samples <- rlnorm(n, meanlog = mu_log, sdlog = sigma_log)

# Means (sanity check)
mean(normal_samples)
mean(lognormal_samples)

# Load ggplot2
library(ggplot2)

# Combine data
df <- data.frame(
  value = c(normal_samples, lognormal_samples),
  distribution = rep(c("Normal", "Log-normal (matched)"), each = n)
)

# Plot (overlapping)
ggplot(df, aes(x = value, fill = distribution)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = m, linetype = "dashed") +
  coord_cartesian(xlim = c(-0.05, 0.3)) +
  labs(title = "Normal vs Log-normal (matched mean & variance)",
       subtitle = "Both have mean = 0.1 and variance = 0.001",
       x = "Value", y = "Density") +
  theme_minimal()

###############################################################
# Create x range
x <- seq(-5, 5, length.out = 1000)

# Normal and Cauchy densities
normal_density <- dnorm(x, mean = 0, sd = 1)
cauchy_density <- dcauchy(x, location = 0, scale = 1)

# Plot
plot(x, normal_density, type = "l", lwd = 2,
     ylim = c(0, max(normal_density)),
     main = "Normal vs Cauchy Distribution",
     xlab = "Value", ylab = "Density")

lines(x, cauchy_density, lwd = 2, lty = 2)

legend("topright",
       legend = c("Normal(0,1)", "Cauchy(0,1)"),
       lty = c(1, 2), lwd = 2)

#################################################################

# Create x range
x <- seq(-10, 10, length.out = 2000)

# Densities
normal <- dnorm(x, mean = 0.12, sd = 1)
cauchy1 <- dcauchy(x, location = 0, scale = 1)
cauchy2 <- dcauchy(x, location = 0, scale = 2.5)

# Combine into dataframe
library(ggplot2)

df <- data.frame(
  x = rep(x, 3),
  density = c(normal, cauchy1, cauchy2),
  distribution = rep(c("Normal(0,1)", "Cauchy(0,1)", "Cauchy(0,2.5)"),
                     each = length(x))
)

# Plot
ggplot(df, aes(x = x, y = density, color = distribution)) +
  geom_line(linewidth = 1) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(0, 0.5)) +
  labs(title = "Normal vs Cauchy Distributions",
       subtitle = "Cauchy has heavier tails; scale controls spread",
       x = "Value", y = "Density") +
  theme_minimal()

##################################################################

library(tidyverse)

# Parameters
meanlog <- log(0.12)
sdlog <- 0.2   # adjust slightly if needed

# Generate x values
x <- seq(0.001, 0.2, length.out = 1000)

# Density
y <- dlnorm(x, meanlog = meanlog, sdlog = sdlog)

# Plot
plot(x, y, type = "l", lwd = 2,
     main = "Lognormal Distribution (center ~0.12)",
     xlab = "x", ylab = "Density")

# Add reference lines
abline(v = 0.12, col = "red", lty = 2)  # center
abline(v = c(0.01, 0.2), col = "blue", lty = 3)  # range

############################################################

# ---- Input (you provide this) ----
N_init_approx <- 88924   # example value (replace with your first-year abundance)

# ---- Gamma parameters ----
shape <- N_init_approx * 0.00005
rate  <- 0.00005

# ---- Generate x range ----
x <- seq(0, N_init_approx * 3, length.out = 1000)

# ---- Density ----
y <- dgamma(x, shape = shape, rate = rate)

# ---- Plot ----
plot(x, y, type = "l", lwd = 2,
     main = "Gamma Prior for N_init",
     xlab = "N_init", ylab = "Density")

# Reference line at N_init_approx
abline(v = N_init_approx, col = "red", lty = 2)

# Optional: mean line
mean_val <- shape / rate
abline(v = mean_val, col = "blue", lty = 3)

legend("topright",
       legend = c("N_init_approx", "Mean"),
       col = c("red", "blue"),
       lty = c(2, 3),
       bty = "n")
######################################################

library(tidyverse)

# Replace with your abundance vector or value
# abundance <- your_data$abundance
k_approx <- max(abundance, na.rm = TRUE)

# Try several SDs on the log scale
sd_log_k1_vals <- c(0.1, 0.25, 0.5, 0.75, 1)

# Use the widest prior to set a sensible x-range
upper_k <- qlnorm(
  0.8,
  meanlog = log(k_approx),
  sdlog = max(sd_log_k1_vals)
)

k_grid <- seq(0, upper_k, length.out = 2000)

# Density curves
prior_k_df <- expand_grid(
  k = k_grid,
  sd_log_k1 = sd_log_k1_vals
) %>%
  mutate(
    density = dlnorm(k, meanlog = log(k_approx), sdlog = sd_log_k1),
    sd_log_k1 = paste0("sd_log_k1 = ", sd_log_k1)
  )

ggplot(prior_k_df, aes(x = k, y = density, color = sd_log_k1)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = k_approx, linetype = "dashed") +
  labs(
    x = expression(k[1]),
    y = "Density",
    title = "Prior distributions for k_1",
    subtitle = paste("log_k_1 ~ Normal(log(k_approx), sd_log_k1),  k_approx =", round(k_approx, 2))
  ) +
  theme_bw()

##############################################################

library(tidyverse)

# your approximate r
r_approx <- 0.12   # change this to your value

# different sd values on log scale
sd_vals <- c(0.1, 0.2, 0.25, 0.5, 0.75, 1)

# grid of r values
r_grid <- seq(0, 0.4, length.out = 2000)  # adjust upper limit if needed

# compute densities
prior_df <- expand_grid(
  r = r_grid,
  sd_r1 = sd_vals
) %>%
  mutate(
    density = dlnorm(r, meanlog = log(r_approx), sdlog = sd_r1),
    sd_r1 = paste0("sd = ", sd_r1)
  )

# plot
ggplot(prior_df, aes(x = r, y = density, color = sd_r1)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = r_approx, linetype = "dashed") +
  labs(
    x = "r",
    y = "Density",
    title = "Prior distribution of r (lognormal)",
    subtitle = paste("Median =", r_approx)
  ) +
  theme_bw()



