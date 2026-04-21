
# Model Evaluation (which model is better?)

make_model_eval_table <- function(model_list,
                                  data_orig,
                                  species_name,
                                  min_model_year = NULL,
                                  max_model_year = NULL,
                                  year_var = "year",
                                  round_digits = NULL) {
  
  library(dplyr)
  library(purrr)
  library(tibble)
  library(rstan)
  library(loo)
  
  # check model list
  if (!is.list(model_list) || length(model_list) == 0) {
    stop("model_list must be a non-empty named list of fitted model objects.")
  }
  
  if (is.null(names(model_list)) || any(names(model_list) == "")) {
    stop("model_list must be a named list, e.g. list(null = fit1, temp = fit2).")
  }
  
  # check year column exists if year filtering is requested
  if ((!is.null(min_model_year) || !is.null(max_model_year)) &&
      !year_var %in% names(data_orig)) {
    stop(paste0("Column '", year_var, "' not found in data_orig."))
  }
  
  # subset to species
  data_sub <- data_orig %>%
    filter(species == species_name) %>%
    filter(year >= min_model_year & year <= max_model_year)
  
  if (nrow(data_sub) == 0) {
    stop("No rows found for the specified species_name.")
  }
  
  # subset to model year window if supplied
  if (!is.null(min_model_year)) {
    data_sub <- data_sub %>% filter(.data[[year_var]] >= min_model_year)
  }
  
  if (!is.null(max_model_year)) {
    data_sub <- data_sub %>% filter(.data[[year_var]] <= max_model_year)
  }
  
  if (nrow(data_sub) == 0) {
    stop("No rows remain after filtering by species_name and model year range.")
  }
  
  # full abundance vector for the model time window
  y_full <- data_sub %>%
    pull(abundance)
  
  # observed indices within the model time window
  obs_idx <- which(y_full > -1)
  
  if (length(obs_idx) == 0) {
    stop("No observed abundance values (> -1) found for this species in the selected model year range.")
  }
  
  # observed abundance values only
  y_obs <- y_full[obs_idx]
  
  # nested Bayesian R2 function
  get_bayes_R2 <- function(fit, obs_idx, y_obs, model_name = NULL) {
    mu_draws <- rstan::extract(fit, pars = "mu_pred")$mu_pred
    
    if (is.null(mu_draws)) {
      stop(paste0("Model '", model_name, "' does not contain mu_pred."))
    }
    
    if (ncol(mu_draws) < max(obs_idx)) {
      stop(
        paste0(
          "Model '", model_name, "' has only ", ncol(mu_draws),
          " columns in mu_pred, but obs_idx goes up to ", max(obs_idx),
          ". Check that the fitted model years match min_model_year/max_model_year."
        )
      )
    }
    
    mu_obs <- mu_draws[, obs_idx, drop = FALSE]
    
    bayes_R2 <- apply(mu_obs, 1, function(mu) {
      var_fit <- var(mu)
      var_res <- var(y_obs - mu)
      var_fit / (var_fit + var_res)
    })
    
    tibble(
      R2_mean   = mean(bayes_R2),
      R2_median = median(bayes_R2),
      R2_lower  = unname(quantile(bayes_R2, 0.025)),
      R2_upper  = unname(quantile(bayes_R2, 0.975))
    )
  }
  
  # evaluate all models
  model_eval_table <- purrr::imap_dfr(model_list, function(fit, model_name) {
    
    log_lik_mat <- rstan::extract(fit, pars = "log_lik")$log_lik
    
    if (is.null(log_lik_mat)) {
      stop(paste0("Model '", model_name, "' does not contain log_lik."))
    }
    
    if (ncol(log_lik_mat) < max(obs_idx)) {
      stop(
        paste0(
          "Model '", model_name, "' has only ", ncol(log_lik_mat),
          " columns in log_lik, but obs_idx goes up to ", max(obs_idx),
          ". Check that the fitted model years match min_model_year/max_model_year."
        )
      )
    }
    
    log_lik_obs <- log_lik_mat[, obs_idx, drop = FALSE]
    
    loo_res  <- loo::loo(log_lik_obs)
    waic_res <- loo::waic(log_lik_obs)
    r2_res   <- get_bayes_R2(fit, obs_idx, y_obs, model_name)
    
    tibble(
      model         = model_name,
      min_year      = ifelse(is.null(min_model_year), NA, min_model_year),
      max_year      = ifelse(is.null(max_model_year), NA, max_model_year),
      n_total_years = length(y_full),
      n_obs_years   = length(obs_idx),
      elpd_loo      = loo_res$estimates["elpd_loo", "Estimate"],
      elpd_loo_se   = loo_res$estimates["elpd_loo", "SE"],
      looic         = loo_res$estimates["looic", "Estimate"],
      looic_se      = loo_res$estimates["looic", "SE"],
      p_loo         = loo_res$estimates["p_loo", "Estimate"],
      elpd_waic     = waic_res$estimates["elpd_waic", "Estimate"],
      elpd_waic_se  = waic_res$estimates["elpd_waic", "SE"],
      waic          = waic_res$estimates["waic", "Estimate"],
      waic_se       = waic_res$estimates["waic", "SE"],
      p_waic        = waic_res$estimates["p_waic", "Estimate"]
    ) %>%
      bind_cols(r2_res)
  })
  
  if (!is.null(round_digits)) {
    model_eval_table <- model_eval_table %>%
      mutate(across(where(is.numeric), ~ round(.x, round_digits)))
  }
  
  return(model_eval_table)
}

