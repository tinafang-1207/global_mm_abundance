
# Model Evaluation (which model is better?)

make_model_eval_table <- function(model_list, data_orig, species_name, round_digits = NULL) {
  
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
  
  # observed abundance for selected species
  y_obs_full <- data_orig %>%
    filter(species == species_name) %>%
    filter(abundance != -999) %>%
    pull(abundance)
  
  if (length(y_obs_full) == 0) {
    stop("No rows found for the specified species_name.")
  }
  
  obs_idx <- which(y_obs_full > -1)
  y_obs <- y_obs_full[obs_idx]
  
  if (length(obs_idx) == 0) {
    stop("No observed abundance values (> -1) found for this species.")
  }
  
  #nested Bayesian R2 function
  get_bayes_R2 <- function(fit, obs_idx, y_obs) {
    mu_draws <- rstan::extract(fit, pars = "mu_pred")$mu_pred
    mu_obs <- mu_draws[, obs_idx, drop = FALSE]

    bayes_R2 <- apply(mu_obs, 1, function(mu) {
      var_fit <- var(mu)
      var_res <- var(y_obs - mu)
      var_fit / (var_fit + var_res)
    })

    tibble(
      R2_mean   = mean(bayes_R2),
      R2_median = median(bayes_R2),
      R2_lower  = quantile(bayes_R2, 0.025),
      R2_upper  = quantile(bayes_R2, 0.975)
    )
  }
  
  # evaluate all models
  model_eval_table <- purrr::imap_dfr(model_list, function(fit, model_name) {
    
    log_lik_mat <- rstan::extract(fit, pars = "log_lik")$log_lik
    log_lik_obs <- log_lik_mat[, obs_idx, drop = FALSE]
    
    loo_res <- loo::loo(log_lik_obs)
    waic_res <- loo::waic(log_lik_obs)
    r2_res <- get_bayes_R2(fit, obs_idx, y_obs)
    
    tibble(
      model         = model_name,
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
