
### clean working environment ###
rm(list = ls())

### import library
library(tidyverse)
library(rstan)
library(parallel)

### for parallelization
options(mc.cores = parallel::detectCores())

### Compile the stan model
SPM_stan = stan_model(file = "model/vary_p_no_temp.stan")

# ================================
# Species List
# ================================
species_list <- c("California_sea_lion")

# Root output directory
root_output <- "data/confidential/stan_output"
dir.create(root_output, showWarnings = FALSE, recursive = TRUE)

# ================================================================
# Function that processes ONE species (this will run on one core)
# ================================================================
run_species <- function(sp_name) {
  
  # ---- 0. Output directory ----
  output_dir <- file.path(root_output, sp_name)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # ---- 0.1 Start log file ----
  cat("\n=====================================\n")
  cat("Starting species:", sp_name, "\n")
  cat("PID:", Sys.getpid(), "\n")
  cat("=====================================\n\n")
  flush.console()
  
  # ---- 1. Load DATA ----
  all_data <- read.csv("data/confidential/input_data/input_final.csv")
  input_df <- subset(all_data, species == sp_name)
  
  min_year = 1975
  max_year = 2014
  abundance = input_df[input_df$year>=min_year&input_df$year<=max_year,]$abundance
  catch = input_df[input_df$year>=min_year&input_df$year<=max_year,]$catch
  sigma_true <-input_df[input_df$year>=min_year&input_df$year<=max_year,]$sigma
  z_true <- 3.9
  
  # ---- 2. Stan Data ----
  stan_data <- list(
    N_1 = max_year-min_year+1,
    Abundance_1 = abundance,
    Catch_1 = catch,
    low_r = 0.01,
    high_r = 0.2,
    low_k = 0.8*max(abundance),
    high_k = 3*max(abundance),
    sigma_1 = sigma_true,
    z_1=z_true
  )
  
  # ---- 3. Warmup Settings ----
  warmup_values <- c(50000, 100000, 150000, 300000, 500000, 1000000)
  samples_per_chain <- 50000
  chains <- 3
  thin <- 10
  
  # ---- 4. Warning log ----
  warning_log <- data.frame(
    warmup = numeric(),
    iterations = numeric(),
    message = character(),
    stringsAsFactors = FALSE
  )
  
  last_rhat <- Inf
  
  # ================================
  # Loop through warmup values
  # ================================
  for (w in warmup_values) {
    
    if (last_rhat < 1.05) {
      cat("\n🎉 Species", sp_name, 
          ": Chains converged (max R-hat =", last_rhat, "). Stopping.\n")
      flush.console()
      break
    }
    
    iter <- w + samples_per_chain
    cat("\n----------------------------------------\n")
    cat("Species:", sp_name, "| warmup:", w, "| iter:", iter, "\n")
    cat("----------------------------------------\n")
    flush.console()
    
    run_warnings <- c()
    
    # ---- 5. Run Stan ----
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
            refresh = 200,
            control = list(adapt_delta = 0.99, max_treedepth = 20)
          ),
          warning = function(war) {
            run_warnings <<- c(run_warnings, conditionMessage(war))
            invokeRestart("muffleWarning")
          }
        )
      },
      error = function(e) {
        cat("❌ ERROR:", conditionMessage(e), "\n")
        flush.console()
        run_warnings <<- c(run_warnings, paste("Error:", conditionMessage(e)))
        return(NULL)
      }
    )
    
    # ---- 6. If model failed ----
    if (is.null(fit_SPM_stan)) {
      warning_log <- rbind(
        warning_log,
        data.frame(
          warmup = w,
          iterations = iter,
          message = paste(run_warnings, collapse = " | ")
        )
      )
      write.csv(warning_log,
                file = file.path(output_dir, "stan_warnings_summary.csv"),
                row.names = FALSE)
      next
    }
    
    # ---- 7. Save outputs ----
    sum_output <- summary(fit_SPM_stan)$summary
    
    write.csv(sum_output,
              file = file.path(output_dir,
                               paste0("summary_warmup_", w, "_iter_", iter, ".csv")),
              row.names = TRUE)
    
    saveRDS(fit_SPM_stan,
            file.path(output_dir,
                      paste0("fit_warmup_", w, "_iter_", iter, ".rds")))
    
    # Warnings log
    if (length(run_warnings) == 0) run_warnings <- "No warning"
    warning_log <- rbind(
      warning_log,
      data.frame(
        warmup = w,
        iterations = iter,
        message = paste(run_warnings, collapse = " | ")
      )
    )
    write.csv(warning_log,
              file = file.path(output_dir, "stan_warnings_summary.csv"),
              row.names = FALSE)
    
    # ---- 8. Compute R-hat ----
    last_rhat <- max(sum_output[, "Rhat"], na.rm = TRUE)
    cat("Max R-hat:", last_rhat, "\n")
    flush.console()
  }
  
  cat("\n🏁 Finished species:", sp_name, "\n")
  flush.console()
  
  return(TRUE)
}

# ================================================================
# RUN ALL SPECIES IN PARALLEL
# ================================================================

for (sp in species_list) {
  run_species(sp)
}
