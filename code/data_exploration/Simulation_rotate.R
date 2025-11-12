
### import library
library(tidyverse)
library(rstan)

### Compile the stan model
SPM_stan = stan_model(file = "model/no_est_catch_simulation.stan")

# Simulate the data
set.seed(123)

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

# export the dataframe

# --- Create dataframe ---
year <- seq(1900, by = 1, length.out = N)

sim_data <- data.frame(
  year = year,
  abundance = Abundance,
  catch = Catch,
  sigma = rep(sigma_true, N)
)

# --- Export to CSV ---
write.csv(sim_data, file = "data/sim_data/simulated_population_data.csv", row.names = FALSE)

###################################################################################################
# rotate the model 

output_dir <- "data/sim_data_output"

# ---- 1. Load data ----
data <- read.csv("data/sim_data/simulated_population_data.csv")

# Extract variables
N <- nrow(data)
Catch <- data$catch
Abundance <- data$abundance
sigma_true <- unique(data$sigma)
z_true <- 2.39

# ---- 2. Stan data list ----
stan_data <- list(
  N_1 = N,
  Catch_1 = Catch,
  Abundance_1 = Abundance,
  low_r = 0.01,
  high_r = 0.12,
  sigma_1 = sigma_true,
  low_k = 0.8 * max(Abundance),
  high_k = 3 * max(Abundance),
  z_1 = z_true
)


# ---- 4. Run model for multiple warmups ----
warmup_values <- seq(1000, 9000, by = 1000)
samples_per_chain <- 1000
chains <- 3
thin <- 1

# Create log file
warning_log <- data.frame(
  warmup = numeric(),
  iterations = numeric(),
  message = character(),
  stringsAsFactors = FALSE
)

for (w in warmup_values) {
  iter <- w + samples_per_chain
  
  cat("\n============================\n")
  cat("Running model with warmup =", w, "and iter =", iter, "\n")
  cat("============================\n")
  
  # Collect *all* warnings for this run
  run_warnings <- c()
  
  fit_SPM_stan <- tryCatch(
    {
      withCallingHandlers(
        sampling(
          SPM_stan,
          seed = 1207,
          data = stan_data,
          chains = chains,
          iter = iter,
          warmup = w,
          thin = thin,
          refresh = 0,
          control = list(adapt_delta = 0.99, max_treedepth = 20)
        ),
        warning = function(war) {
          # Append all warning messages to the vector
          run_warnings <<- c(run_warnings, conditionMessage(war))
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      cat("❌ Error during model run:", conditionMessage(e), "\n")
      run_warnings <<- c(run_warnings, paste("Error:", conditionMessage(e)))
      return(NULL)
    }
  )
  
  # Skip if model failed
  if (is.null(fit_SPM_stan)) {
    warning_log <- rbind(warning_log,
                         data.frame(warmup = w,
                                    iterations = iter,
                                    message = paste(run_warnings, collapse = " | ")))
    next
  }
  
  # ---- 5. Save summary output ----
  output_summary <- as.data.frame(summary(fit_SPM_stan)$summary)
  output_filename <- paste0("stan_output_warmup_", w, "_iter_", iter, ".csv")
  write.csv(output_summary, file = file.path(output_dir, output_filename), row.names = TRUE)
  
  # save rds
  rds_filename <- paste0("stan_fit_warmup_", w, "_iter_", iter, ".rds")
  saveRDS(fit_SPM_stan, file = file.path(output_dir, rds_filename))
  
  # ---- 6. Store warnings ----
  if (length(run_warnings) == 0) {
    run_warnings <- "No warning"
  }
  
  warning_log <- rbind(warning_log,
                       data.frame(warmup = w,
                                  iterations = iter,
                                  message = paste(run_warnings, collapse = " | ")))
  
  # Save warnings log after each run (so it’s never lost)
  write.csv(warning_log, file = file.path(output_dir,"stan_warnings_summary.csv"), row.names = FALSE)
  
  cat("✅ Finished run with warmup =", w, "iter =", iter, "\n")
}

cat("\nAll runs complete! Summaries and warnings saved.\n")







