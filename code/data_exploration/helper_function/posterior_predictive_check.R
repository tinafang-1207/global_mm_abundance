
# Posterior predictive plot check

plot_ppc_check <- function(fit, spp, min_model_year, max_model_year) {
  
  library(rstan)
  library(tidyverse)
  
  post <- rstan::extract(fit)
  y_rep <- post$Abundance_pred   # posterior predictive draws
  mu_pred <- post$mu_pred        # fitted latent abundance trajectory
  data_spp <- data_orig %>% filter(species == spp)
  y_obs_full <- data_spp %>% filter(year >= min_model_year & year <= max_model_year) %>% pull(abundance)
  obs_idx <- which(y_obs_full > -1)
  n_iter <- nrow(mu_pred)
  n_time <- ncol(mu_pred)
  years <- seq_len(n_time)
  
  # observed data
  obs_df <- tibble(
    year = years,
    y_obs = y_obs_full
  ) %>% mutate(observed = y_obs > -1)
  
  # fitted values
  mu_df <- as_tibble(mu_pred) %>%
    mutate(draw = row_number()) %>%
    pivot_longer(
      cols = -draw,
      names_to = "year",
      values_to = "mu_pred"
    ) %>%
    mutate(year = as.integer(str_remove(year, "V")))
  
  mu_summary <- mu_df %>%
    group_by(year) %>%
    summarise(
      mu_mean = mean(mu_pred),
      mu_median = median(mu_pred),
      mu_lower = quantile(mu_pred, 0.025),
      mu_upper = quantile(mu_pred, 0.975),
      .groups = "drop"
    )
  
  # posterior predictive draws
  yrep_df <- as_tibble(y_rep) %>%
    mutate(draw = row_number()) %>%
    pivot_longer(
      cols = -draw,
      names_to = "year",
      values_to = "y_rep"
    ) %>%
    mutate(year = as.integer(str_remove(year, "V")))
  
  # Plot theme
  my_theme <-  theme(axis.text=element_text(size=8),
                     axis.title=element_text(size=9),
                     legend.text=element_text(size=8),
                     legend.title=element_text(size=9),
                     strip.text=element_text(size=8),
                     plot.title=element_text(size=9),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.3, "cm"),
                     legend.key = element_rect(fill = NA, color=NA),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Density plot
  obs_dist_df <- obs_df %>%
    filter(observed) %>%
    transmute(value = y_obs)
  
  set.seed = 123
  
  rep_dist_df <- yrep_df %>%
    filter(year %in% obs_idx) %>%
    distinct(draw) %>%
    slice_sample(n = 10) %>%
    inner_join(yrep_df, by = "draw") %>%
    filter(year %in% obs_idx)
  
  g1 <- ggplot() +
    # Posterior predictive densities (10 draws, log scale)
    geom_density(
      data = rep_dist_df,
      aes(x = log(y_rep), group = draw),
      color = "lightblue",
      alpha = 0.4
    ) +
    # Observed density (log scale)
    geom_density(
      data = obs_dist_df,
      aes(x = log(value)),
      color = "black",
      alpha = 0.3
    ) +
    labs(
      x = "log(Abundance)",
      y = "Density"
    ) +
    theme_bw() + my_theme
  
  # Residual plot
  # residual data frame from posterior fitted draws
  mu_draws_df <- mu_df %>%
    filter(year %in% obs_idx) %>%
    left_join(
      obs_df %>% select(year, y_obs),
      by = "year"
    ) %>%
    mutate(
      residual = log(mu_pred) - log(y_obs)
    )
  
  resid_draw_summary <- mu_draws_df %>%
    group_by(draw) %>%
    summarise(
      residual_mean = mean(residual),
      .groups = "drop"
    )
  
  g2 <- ggplot(resid_draw_summary, aes(x = residual_mean)) +
    geom_histogram(bins = 30, fill = "grey80", color = NA) +
    geom_vline(xintercept = 0, linewidth = 0.8) +
    labs(
      x = "log(Residual)",
      y = "Number of draws"
    ) +
    theme_bw() + my_theme
  
  # observed vs. predicted abundance
  plot_df <- mu_summary %>%
    filter(year %in% obs_idx) %>%
    left_join(obs_df, by = "year")
  
  g3 <- ggplot(plot_df, aes(x = y_obs, y = mu_mean)) +
    # 95% interval
    geom_linerange(
      aes(ymin = mu_lower, ymax = mu_upper),
      alpha = 0.5
    ) +
    # points
    geom_point(size = 2, alpha = 0.9) +
    # 1:1 line
    geom_abline(slope = 1, intercept = 0, linewidth = 1) +
    labs(
      title = "Observed vs Predicted Abundance",
      x = "Observed abundance",
      y = "Predicted abundance"
    ) +
    theme_bw() + my_theme
  
  # Total plot
  layout_matrix <- matrix (data = c(1, 2,
                                    3, 3), ncol = 2, byrow = T)
  
  g_total <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix = layout_matrix)
  
  return(g_total)
  
}