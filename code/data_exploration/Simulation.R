
### import library
library(tidyverse)
library(rstan)

### Compile the stan model
SPM_stan = stan_model(file = "model/no_est_catch_repara.stan")

# Set seed for reproducibility
set.seed(123)

# Define known "true" parameter values
N <- 30  # time steps
r_true <- 0.06
k_true <- 1000
P0_true <- 1.0
z_true <- 2.39
sigma_true <- 0.1 
msy_true <- (r_true*k_true*2.39)/(3.39)^(1/2.39 +1)
Catch <- seq(5, 80, length.out = N) 

# Simulate true abundance forward
N_true <- numeric(N)
N_true[1] <- P0_true * k_true  # Initial abundance

for (t in 2:N) {
  N_true[t] <- max(
    N_true[t-1] + r_true * N_true[t-1] * (1 - (N_true[t-1] / k_true)^z_true) - Catch[t-1],
    0.0001
  )
}

# Simulate observed abundance with lognormal noise
Abundance <- rlnorm(N, log(N_true), sigma_true)

# Plot to visualize
plot(1:N, Abundance, type = "b", col = "blue", ylab = "Abundance", main = "Simulated Abundance Data")
lines(1:N, N_true, col = "red", lty = 2)
legend("topright", legend = c("Observed", "True"), col = c("blue", "red"), lty = c(1, 2))

# Package the data for Stan

stan_data <- list(
  N_1 = N,
  Catch_1 = Catch,
  Abundance_1 = Abundance,
  low_r = 0.01,
  high_r = 0.1,
  #lnmsy_1 = log(msy_true),
  #r_1 = r_true,
  sigma_1 = sigma_true,
  low_lnk = log(0.8*max(Abundance)),
  high_lnk = log(2*max(Abundance)),
  #low_k = 0.8*(max(Abundance)),
  #high_k = 2*(max(Abundance)),
  z_1 = z_true
)



chain = 3; iter = 4000; warmup = 3000; thin=1

init_fun <- function() {
  list(
    r_1 = runif(1, 0.055, 0.065),
    #lnmsy_1 = log(msy_true)
    lnmsy_1 = runif(1, log(msy_true)-0.1, log(msy_true)+0.1)
    #sigma_sq_1 = runif(1, 0.009, 0.011)
  )
}

init_fun()

#   z_1 = 2.39
#   r_1 = 0.055
#   lnmsy_1 = log(msy_true)+0.1
#   lnk_1 = log(((z_1 + 1)^((1/z_1) + 1)) *exp(lnmsy_1)/(r_1*z_1))
#   lnk_1
# # 
#   k_1 = exp(lnk_1)
#   k_1

  

fit_SPM_stan <- sampling(SPM_stan,
                         seed = 1207,
                         init = init_fun,
                         data = stan_data,
                         chain = chain,
                         iter = iter,
                         warmup = warmup,
                         cores = chain,
                         thin = thin,
                         control = list(adapt_delta = 0.99, max_treedepth = 12))

# check the pair plot

pairs(fit_SPM_stan, pars = c("r_1", "lnmsy_1", "lnk_1", "k_1"))
pairs(fit_SPM_stan, pars = c("r_1", "k_1", "sigma_sq_1", "sigma_1"))

# see the output

output <- as.data.frame(summary(fit_SPM_stan)$summary)

df_of_draws <- as.data.frame(fit_SPM_stan)


# Extract posterior samples of predicted abundance (Abundance_pred)
pred_samples <- rstan::extract(fit_SPM_stan, pars = "Abundance_pred")$Abundance_pred

# Calculate posterior mean and 95% credible interval for each time step
pred_mean <- apply(pred_samples, 2, mean)
pred_lower <- apply(pred_samples, 2, quantile, probs = 0.025)
pred_upper <- apply(pred_samples, 2, quantile, probs = 0.975)

# Create data frame for plotting
df_plot <- data.frame(
  time = 1:length(Abundance),
  Observed = Abundance,
  Predicted = pred_mean,
  Lower = pred_lower,
  Upper = pred_upper,
  True = N_true
)

# Plot
ggplot(df_plot, aes(x = time)) +
  geom_point(aes(y = Observed), color = "blue", size = 2, alpha = 0.6) +
  geom_point(aes(y = Predicted), color = "red", size = 2, alpha = 0.8) +
  geom_line(aes(y = True), color = "green", linetype = "dashed", size = 1) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "red", alpha = 0.2) +
  labs(title = "Observed, Predicted, and True Abundance",
       x = "Time",
       y = "Abundance") +
  theme_minimal()








##########################################################################

# dinvgamma <- function(x, shape, scale) {
#   ifelse(x > 0,
#          (scale^shape / gamma(shape)) * x^(-shape - 1) * exp(-scale / x),
#          0)
# }
# 
# # Parameters
# shape <- 4
# scale <- 0.01
# 
# # Range of x values (variance)
# x_vals <- seq(0, 0.01, length.out = 1000)
# y_vals <- dinvgamma(x_vals, shape, scale)
# 
# # Plot using ggplot2
# df <- data.frame(x = x_vals, y = y_vals)
# 
# ggplot(df, aes(x, y)) +
#   geom_line(color = "blue", size = 1) +
#   labs(
#     title = "Inverse Gamma(4, 0.01)",
#     x = "x",
#     y = "Density"
#   ) +
#   theme_minimal()
# 
# # Create a sequence of x values between 0 and 1
# x_vals <- seq(0, 1, length.out = 500)
# 
# # Get the density of the Beta(1, 1) distribution at each x
# y_vals <- dbeta(x_vals, shape1 = 1, shape2 = 1)
# 
# # Create a data frame for plotting
# beta_df <- data.frame(x = x_vals, y = y_vals)
# 
# # Plot using ggplot2
# ggplot(beta_df, aes(x = x, y = y)) +
#   geom_line(color = "blue", size = 1) +
#   labs(title = "Beta(1,1) Distribution",
#        x = "x",
#        y = "Density") +
#   theme_minimal()
