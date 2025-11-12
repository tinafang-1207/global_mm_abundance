
### clean working environment ###
rm(list = ls())

### import library
library(tidyverse)
library(rstan)

### Compile the stan model
SPM_stan = stan_model(file = "model/no_est_catch_simulation.stan")

# set up input and output dir
input_dir <- "data/ENP_blue_whale_input"
output_dir <- "data/ENP_blue_whale_output"

### import the data frame
input_df <- read.csv(file.path(input_dir, "ENP_bluewhale_input_df.csv"))

### specify data

min_year = 1905
max_year = 2018
abundance = input_df[input_df$year>=min_year&input_df$year<=max_year,]$abundance
catch = input_df[input_df$year>=min_year&input_df$year<=max_year,]$catch
sigma_true <-input_df[input_df$year>=min_year&input_df$year<=max_year,]$sigma
z_true <- 2.39

stan_data <- list(
  N_1 = max_year-min_year+1,
  Abundance_1 = abundance,
  Catch_1 = catch,
  low_r = 0.01,
  high_r = 0.12,
  sigma_1 = sigma_true,
  low_k = 0.8 * max(abundance),
  high_k = 3 * max(abundance),
  z_1=z_true
)


#####################################################################################
# Rotate the model with different burn-in values until it reaches convergence (Kanaji et al., 2024)

# ---- 4. Run model for multiple warmups ----
warmup_values <- c(50000, 100000, 150000, 300000, 500000, 1000000)
samples_per_chain <- 50000
chains <- 3
thin <- 10

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


