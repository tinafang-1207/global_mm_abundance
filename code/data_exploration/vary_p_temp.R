
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)
library(rstan)

### for parallelization
options(mc.cores = parallel::detectCores())

### Compile the stan model
SPM_stan = stan_model(file = "model/vary_p_temp_R.stan")
SPM_stan = stan_model(file = "model/vary_p_temp_k.stan")
SPM_stan = stan_model(file = "model/vary_p_temp_K_otter.stan")

# ================================
# Species List
# ================================
species_list <- c("Southern_sea_otter_average")

# Root output directory
root_output <- "data/confidential/stan_output"
dir.create(root_output, showWarnings = FALSE, recursive = TRUE)

###############################################################################
run_species <- function(sp_name) {
  
  # ---- 0. Output directory ----
  output_dir <- file.path(root_output, sp_name)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # ---- 0. Console info ----
  cat("\n=====================================\n")
  cat("Starting species:", sp_name, "\n")
  cat("PID:", Sys.getpid(), "\n")
  cat("=====================================\n\n")
  flush.console()
  
  # ---- 1. Load DATA ----
  all_data <- read.csv("data/confidential/input_data/input_final.csv")
  input_df <- subset(all_data, species == sp_name)
  
  #min_year <- min(input_df$year[input_df$catch >= 0], na.rm = TRUE)
  #max_year <- max(input_df$year[input_df$abundance != -999], na.rm = TRUE)
  min_year <- 1992
  max_year <- 2018
  abundance <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$abundance
  catch     <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$catch
  #sigma_true <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$sigma
  environment_true <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$pdo_scaled_all_year
  r_approx <- 0.2
  k_approx <- 17226
  N_init_approx <- abundance[which(abundance != -999)[1]]
  z_true <- 0.9
  
  # ---- 2. Stan Data ----
  stan_data <- list(
    N_1 = max_year - min_year + 1,
    Abundance_1 = abundance,
    Catch_1 = catch,
    N_init_approx = N_init_approx,
    r_approx = r_approx,
    k_approx = k_approx,
    #sigma_1 = sigma_true,
    Environment_1 = environment_true,
    z_1 = z_true
  )
  
  # ---- 3. Warmup Settings ----
  #warmup_values <- c(50000, 100000, 150000, 300000, 500000, 1000000)
  #samples_per_chain <- 50000
  #chains <- 3
  #thin <- 10
  
  warmup_values <- 50000
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
                file = file.path(output_dir, "stan_warnings_summary_temp_k_exp.csv"),
                row.names = FALSE)
      next
    }
    
    # ---- 7. Save outputs ----
    sum_output <- summary(fit_SPM_stan)$summary
    
    write.csv(sum_output,
              file = file.path(output_dir,
                               paste0("summary_warmup_", w, "_iter_", iter, "_temp_k_exp.csv")),
              row.names = TRUE)
    
    saveRDS(fit_SPM_stan,
            file.path(output_dir,
                      paste0("fit_warmup_", w, "_iter_", iter, "_temp_k_exp.rds")))
    
    # ---- 8. Warning log ----
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
              file = file.path(output_dir, "stan_warnings_summary_temp_k_exp.csv"),
              row.names = FALSE)
    
    # ---- 9. Compute R-hat ----
    last_rhat <- max(sum_output[, "Rhat"], na.rm = TRUE)
    cat("Max R-hat:", last_rhat, "\n")
    flush.console()
  }
  
  cat("\n🏁 Finished species:", sp_name, "\n")
  flush.console()
  
  return(TRUE)
}


###########################################################

for (sp in species_list) {
  run_species(sp)
}

sl_posterior <- readRDS("data/confidential/stan_output/California_sea_lion/fit_warmup_50000_iter_1e+05_temp_k_hab_exp.rds")
sl_posterior_trace <- rstan::extract(sl_posterior, permuted = FALSE)
bayesplot::mcmc_trace(sl_posterior_trace, pars = c("log_k_1", "log_N_init_1", "r_1", "k_1", "N_init_1"))
pairs(sl_posterior,  pars = c("r_1", "k_1", "N_init_1", "impact_E_1", "sig_E"))

