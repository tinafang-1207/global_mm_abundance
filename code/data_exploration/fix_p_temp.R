
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)
library(rstan)

### for parallelization
options(mc.cores = parallel::detectCores())

### Compile the stan model
SPM_stan = stan_model(file = "model/no_est_catch_catch_simulation_temp.stan")
#SPM_stan = stan_model(file = "model/no_est_catch_simulation_temp_fix_p.stan")
SPM_stan = stan_model(file = "model/new_code_with_temp_fix_p.stan")

# ================================
# Species List
# ================================
species_list <- c("Humpback_whale")

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
  all_data <- read.csv("data/confidential/input_data/wc_hbk_gray_input.csv")
  input_df <- subset(all_data, species == sp_name)
  
  # min_year <- min(input_df$year)
  # max_year <- max(input_df$year)
  min_year <- 1854
  max_year <- 2014
  abundance <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$abundance
  catch     <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$catch
  sigma_true <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$sigma
  environment_true <- input_df[input_df$year >= min_year & input_df$year <= max_year, ]$temp_scaled
  z_true <- 2.39
  
  # ---- 1.1 First observed abundance index (SIMPLE) ----
  t_first_obs <- which(abundance > -1)[1]
  
  # ---- 2. Stan Data ----
  stan_data <- list(
    N_1 = max_year - min_year + 1,
    Abundance_1 = abundance,
    Catch_1 = catch,
    low_r = 0.01,
    high_r = 0.12,
    sigma_1 = sigma_true,
    Environment_1 = environment_true,
    z_1 = z_true,
    t_first_obs = t_first_obs
  )
  
  # ---- Initial value helpers ----
  MSYfun <- function(K, r, z) {
    (r * z * K) / ((z + 1)^((1 / z) + 1))
  }
  
  init_fun <- function() {
    rinit <- runif(1, 0.08, 0.09)
    K_init <- 2500
    msy_init <- MSYfun(K_init, rinit, z_true)
    
    list(
      r_1 = rinit,
      lnmsy_1 = log(msy_init),
      impact_E_1 = runif(1, -0.1, 0.1)
    )
  }
  
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
            refresh = 1,
            init = init_fun,
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
                file = file.path(output_dir, "stan_warnings_summary_temp.csv"),
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
              file = file.path(output_dir, "stan_warnings_summary_temp.csv"),
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

###########################################################
fit_SPM_stan <- readRDS("data/confidential/stan_output/Humpback_whale/fit_warmup_50000_iter_1e+05_temp.rds")

post <- rstan::extract(fit_SPM_stan)

# correlations among sampled parameters
cor_r_impact <- cor(post$r_1, post$impact_E_1, use = "complete.obs")
cor_r_lnmsy  <- cor(post$r_1, post$lnmsy_1, use = "complete.obs")



rstan::check_hmc_diagnostics(fit_SPM_stan)

rstan::traceplot(fit_SPM_stan, pars = "impact_E_1")


s <- summary(fit_SPM_stan, pars = "N_med")$summary
idx_floor <- which(s[, "50%"] <= 1e-4 * 1.01)
idx_floor
length(idx_floor)


years <- as.integer(hbk$year)
Abundance_1 <- hbk$abundance
Catch_1 <- hbk$catch
Environment_1 <- hbk$temp_scaled


data.frame(
  t = idx_floor,
  year = years[idx_floor],
  abundance_obs = Abundance_1[idx_floor],
  catch = Catch_1[idx_floor],
  E = Environment_1[idx_floor]
)








input_df <- read.csv("data/confidential/input_data/wc_hbk_gray_input.csv")

hbk <- input_df %>%
  filter(species == "Humpback_whale") %>%
  arrange(year)

E <- hbk$temp_scaled
C <- hbk$catch 


step_raw <- function(N, r, K, z, E, beta, Catch) {
  N + r * exp(beta * E) * N * (1 - (N / K)^z) - Catch
}

simulate_raw <- function(r = 0.06, K = 5000, z = 2.39, beta = 0.5) {
  N <- numeric(length(E))
  N[1] <- K
  for (t in 2:length(E)) {
    N[t] <- step_raw(N[t-1], r, K, z, E[t-1], beta, C[t-1])
  }
  N
}

N_raw <- simulate_raw(r = 0.06, K = 5000, z = 2.39, beta = 0.5)

range(N_raw, na.rm = TRUE)
min(N_raw, na.rm = TRUE)

#########################################################################

r <- 0.06
K <- 5000
z <- 2.39
beta <- 0.5
floor <- 1e-4

step_raw <- function(N, r, K, z, E, beta, Catch) {
  N + r * exp(beta * E) * N * (1 - (N / K)^z) - Catch
}

softfloor <- function(x, floor = 1e-4, s = 1) {
  z <- (x - floor) / s
  floor + ifelse(
    z > 30,        # avoid exp overflow
    x - floor,     # linear tail
    s * log1p(exp(z))
  )
}

nT <- length(E)

N_raw  <- numeric(nT)
N_fmax <- numeric(nT)
N_soft <- numeric(nT)

N_raw[1]  <- K
N_fmax[1] <- K
N_soft[1] <- K

for (t in 2:nT) {
  
  # raw update
  raw_next <- step_raw(
    N_raw[t-1], r, K, z,
    E[t-1], beta, C[t-1]
  )
  
  # store trajectories
  N_raw[t]  <- raw_next
  N_fmax[t] <- pmax(raw_next, floor)
  N_soft[t] <- softfloor(raw_next, floor)
}

t <- seq_len(nT)

# zoom window where collapse happens
zoom_idx <- which(N_raw < 200 | N_fmax < 200 | N_soft < 200)

t_zoom <- zoom_idx
ylim_zoom <- range(
  c(N_raw[zoom_idx], N_fmax[zoom_idx], N_soft[zoom_idx]),
  na.rm = TRUE
)

plot(
  t_zoom, N_raw[zoom_idx],
  type = "l",
  col = "black",
  lwd = 2,
  ylim = ylim_zoom,
  xlab = "Time step",
  ylab = "Population size",
  main = "Zoom near zero: raw vs fmax vs softfloor"
)

lines(t_zoom, N_fmax[zoom_idx], col = "red",  lwd = 2)
lines(t_zoom, N_soft[zoom_idx], col = "blue", lwd = 2)

abline(h = 1e-4, lty = 2, col = "grey50")

legend(
  "topright",
  legend = c("Raw", "Hard floor (fmax)", "Smooth floor"),
  col = c("black", "red", "blue"),
  lwd = 2,
  bty = "n"
)

################################################

plot(
  t, N_soft - N_fmax,
  type = "l",
  col = "purple",
  lwd = 2,
  xlab = "Time step",
  ylab = "softfloor − fmax",
  main = "Difference between smooth and hard floor"
)

abline(h = 0, lty = 2, col = "grey50")

######################################################

fit_SPM_stan <- readRDS("data/confidential/stan_output/Humpback_whale/fit_warmup_1000_iter_3000_temp.rds")

rstan::check_hmc_diagnostics(fit_SPM_stan)



