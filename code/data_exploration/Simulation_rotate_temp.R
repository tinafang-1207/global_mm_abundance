
### import library
library(tidyverse)
library(rstan)
library(parallel)
library(future)
library(future.apply)

### Compile the stan model
SPM_stan = stan_model(file = "model/no_est_catch_catch_simulation_temp.stan")

# read in data
temp <- read.csv("data/confidential/input_data/PDO_temp.csv", na.strings = "/") %>%
  select(Year, average) %>%
  rename(Environment_1 = average,
         year = Year)

# ---------------------------------------------------------
# Simulate the data (NOW WITH Environment_1 added!)
# ---------------------------------------------------------
set.seed(123)

# -------------------------
# Load environmental covariate
# Must have at least N rows and a column named Environment_1
# -------------------------
N <- 30
Environment_1 <- temp$Environment_1[1:N]   # <<<<<< NEW

# -------------------------
# Original species parameters (Species 1)
# -------------------------
r_true_1 <- 0.06
k_true_1 <- 1000
P0_true_1 <- 1.0
z_true_1 <- 2.39
sigma_true_1 <- 0.1
Catch <- seq(5, 80, length.out = N)

# -------------------------
# Additional species parameters (Species 2 & 3)
# -------------------------
r_true_2 <- 0.04
k_true_2 <- 800
P0_true_2 <- 1
z_true_2 <- 2.39
sigma_true_2 <- 0.15

r_true_3 <- 0.07
k_true_3 <- 1500
P0_true_3 <- 1
z_true_3 <- 2.39
sigma_true_3 <- 0.12

# -------------------------
# Pack species parameters
# -------------------------
species_params <- list(
  list(name="Species_1", r=r_true_1, k=k_true_1, P0=P0_true_1, z=z_true_1, sigma=sigma_true_1),
  list(name="Species_2", r=r_true_2, k=k_true_2, P0=P0_true_2, z=z_true_2, sigma=sigma_true_2),
  list(name="Species_3", r=r_true_3, k=k_true_3, P0=P0_true_3, z=z_true_3, sigma=sigma_true_3)
)

# ---------------------------------------------------------
# IMPORTANT: Impact coefficient for simulation ONLY
# (Stan will estimate this later; here we just pick a value)
# ---------------------------------------------------------
impact_E_true <- 0.5   # you can modify this

# ---------------------------------------------------------
# Simulation loop for all species
# ---------------------------------------------------------
sim_all <- list()
year <- seq(1854, by = 1, length.out = N)

for (sp in species_params) {
  
  # True abundance path
  N_true <- numeric(N)
  N_true[1] <- sp$P0 * sp$k
  
  for (t in 2:N) {
    N_true[t] <- max(
      N_true[t-1] +
        sp$r * exp(impact_E_true * Environment_1[t-1]) *
        N_true[t-1] * (1 - (N_true[t-1] / sp$k)^sp$z) -
        Catch[t-1],
      0.0001
    )
  }
  
  # Observed abundance with lognormal error
  Abundance <- rlnorm(N, log(N_true), sp$sigma)
  
  # Build final dataframe
  df <- data.frame(
    species        = sp$name,
    year           = year,
    abundance      = Abundance,
    catch          = Catch,
    Environment_1  = Environment_1,        # <<<<<< NEW COLUMN!!!
    sigma          = rep(sp$sigma, N),
    z_true         = rep(sp$z, N)
  )
  
  sim_all[[sp$name]] <- df
}

# -------------------------
# Combine all species
# -------------------------
sim_data <- do.call(rbind, sim_all)
rownames(sim_data) <- NULL

# Output preview
head(sim_data)

write.csv(sim_data, file = "data/sim_data/simulated_population_data_temp.csv", row.names = FALSE)

########################################################################################

# rotate the model 

# ================================
# Species List
# ================================
species_list <- c("Species_1", "Species_2", "Species_3")

# Root output directory
root_output <- "data/sim_data_output"
dir.create(root_output, showWarnings = FALSE, recursive = TRUE)

run_species <- function(sp_name) {
  
  # ---- 0. Output directory ----
  output_dir <- file.path(root_output, sp_name)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # ---- 0. Removed log file + sink() ----
  cat("\n=====================================\n")
  cat("Starting species:", sp_name, "\n")
  cat("PID:", Sys.getpid(), "\n")
  cat("=====================================\n\n")
  flush.console()
  
  # ---- 1. Load DATA ----
  all_data <- read.csv("data/sim_data/simulated_population_data_temp.csv")
  data <- subset(all_data, species == sp_name)
  
  N <- nrow(data)
  Catch <- data$catch
  Abundance <- data$abundance
  Environment <- data$Environment_1
  sigma_true <- rep(data$sigma[1], N)
  z_true <- data$z_true[1]
  
  # ---- 2. Stan Data ----
  stan_data <- list(
    N_1 = N,
    Catch_1 = Catch,
    Abundance_1 = Abundance,
    low_r = 0.01,
    high_r = 0.12,
    sigma_1 = sigma_true,
    low_k = 0.8 * max(Abundance),
    high_k = 3 * max(Abundance),
    z_1 = z_true,
    Environment_1 = Environment
  )
  
  # ---- 3. Warmup Settings ----
  warmup_values <- seq(1000, 9000, by = 1000)
  samples_per_chain <- 1000
  chains <- 3
  thin <- 1
  
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
                               paste0("summary_warmup_", w, "_iter_", iter, "_temp.csv")),
              row.names = TRUE)
    
    saveRDS(fit_SPM_stan,
            file.path(output_dir,
                      paste0("fit_warmup_", w, "_iter_", iter, "_temp.rds")))
    
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


for (sp in species_list) {
  run_species(sp)
}

#########################################################################################


