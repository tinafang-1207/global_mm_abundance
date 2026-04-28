

plot_abundance <- function(fit, spp, output_spp, data_orig, min_model_year, max_model_year, z_val) {
  library(tidyverse)
  
  min_model_year <- min_model_year
  max_model_year <- max_model_year
  z_val <- z_val
  
  k_mean <- output_spp %>%
    rename(est_variables = X) %>%
    filter(est_variables == "k_1") %>%
    pull(mean)
  
  k_lcl <- output_spp %>%
    rename(est_variables = X) %>%
    filter(est_variables == "k_1") %>%
    pull(X2.5.)
  
  k_hcl <- output_spp %>%
    rename(est_variables = X) %>%
    filter(est_variables == "k_1") %>%
    pull(X97.5.)
  
  k_df <- data.frame(
    est_variables = seq(min_model_year, max_model_year),
    k_mean = round(k_mean, 0),
    k_lcl = round(k_lcl, 0),
    k_hcl = round(k_hcl, 0)
  ) %>%
    mutate(label = paste0("K = ", k_mean))
  
  
  mnpl <- k_mean * (1/(1+z_val))^(1/z_val)
  mnpl_lcl <- k_lcl * (1/(1+z_val))^(1/z_val)
  mnpl_hcl <- k_hcl * (1/(1+z_val))^(1/z_val)
  
  mnpl_df <- data.frame(
    est_variables = seq(min_model_year, max_model_year),
    mnpl_mean = round(mnpl, 0),
    mnpl_lcl = round(mnpl_lcl,0),
    mnpl_hcl = round(mnpl_hcl,0)
  ) %>%
    mutate(label = paste0("MNPL = ", mnpl_mean))
  
  
  output_spp_clean <- output_spp %>%
    rename(est_variables = X) %>%
    filter(str_detect(est_variables, "N_med")) %>%
    mutate(est_variables = seq(min_model_year, max_model_year)) %>%
    pivot_longer(
      cols = c(mean, se_mean, sd, X2.5., X25., X50., X75., X97.5., n_eff, Rhat),
      names_to = "estimation_type",
      values_to = "estimation"
    ) %>%
    mutate(
      est_variables = as.numeric(est_variables),
      N_over_K = if_else(estimation_type == "mean", estimation / k_mean, NA_real_)
    ) %>%
    filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")) %>%
    pivot_wider(
      id_cols = est_variables,
      names_from = estimation_type,
      values_from = estimation
    ) %>%
    arrange(est_variables)
  
  
  original_spp_clean <- data_orig %>%
    filter(species == spp) %>%
    select(year, abundance, lcl, hcl, catch) %>%
    mutate(abundance = ifelse(abundance == "-999", NA, abundance)) %>%
    filter(year >= min_model_year & year <= max_model_year)
  
  # Posterior draws
  
  draws <- as.data.frame(fit)
  
  draws_clean <- draws %>%
    transmute(
      r_1 = r_1,
      k_1 = k_1,
      N_latest = .data[[grep("^N_med\\[", names(draws), value = TRUE) |> tail(1)]]
    ) %>%
    tidyr::pivot_longer(
      cols = c(r_1, k_1, N_latest),
      names_to = "parameter",
      values_to = "value"
    )
  
  ### plot abundance ###
  
  plot_theme <- theme(axis.text=element_text(size=8),
                      axis.title=element_text(size=10),
                      legend.text=element_text(size=8),
                      legend.title=element_text(size=8),
                      strip.text = element_text(size=8),
                      plot.title = element_blank(),
                      # Gridlines
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(), 
                      panel.border = element_rect(colour = "black", fill = NA),
                      axis.line = element_line(colour = "black"),
                      strip.background = element_rect(colour = "black", fill ="grey90"),
                      # Legend
                      legend.background = element_rect(fill=alpha('blue', 0)))
  
  
  g1 <- ggplot() +
    geom_ribbon(data = k_df, aes(x = est_variables, ymin = k_lcl, ymax = k_hcl), fill = "blue",alpha = 0.2) +
    geom_hline(yintercept = k_df$k_mean, linetype = "solid",color = "blue") +
    geom_ribbon(data = mnpl_df, aes(x = est_variables, ymin = mnpl_lcl, ymax = mnpl_hcl), fill = "red",alpha = 0.2) +
    geom_hline(yintercept = mnpl_df$mnpl_mean, linetype = "dashed", color = "red") +
    geom_text(data = k_df, aes(x = min_model_year + 5, y = k_mean, label = label), color = "blue", hjust = 1, vjust = -0.5, size = 3) +
    geom_text(data = mnpl_df, aes(x = min_model_year + 7, y = mnpl_mean, label = label), color = "red", hjust = 1, vjust = -0.5, size = 3) +
    geom_ribbon(data = output_spp_clean, aes(x = est_variables, ymin = `X2.5.`, ymax = `X97.5.`), alpha = 0.3) +
    geom_line(data = output_spp_clean, aes(x = est_variables, y = mean)) +
    geom_point(data = original_spp_clean, aes(x = year, y = abundance), fill = "darkblue", color = "black", size = 2, shape = 21) +
    geom_linerange(data = original_spp_clean, aes(x = year, ymin = lcl, ymax = hcl), alpha = 0.5) +
    scale_x_continuous(limits = c(min_model_year, max_model_year), breaks = seq(min_model_year, max_model_year, by = 10)) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Year", y = "Estimated Abundance") +
    plot_theme +
    theme(legend.position = "none")
  
  g1
  
  
  g2 <- ggplot(original_spp_clean, aes(x = year, y = catch)) +
    geom_col(fill = "darkblue",width = 0.8) +
    scale_x_continuous(
      breaks = seq(min_model_year, max_model_year, by = 10)
    ) +
    coord_cartesian(xlim = c(min_model_year, max_model_year)) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Year", y = "Human-caused mortality") +
    plot_theme
  
  g2
  
  draws <- as.data.frame(fit)
  
  draws_clean <- draws %>%
    transmute(
      r_1 = r_1,
      k_1 = k_1,
      N_latest = .data[[grep("^N_med\\[", names(draws), value = TRUE) |> tail(1)]]
    ) %>%
    tidyr::pivot_longer(
      cols = c(r_1, k_1, N_latest),
      names_to = "parameter",
      values_to = "value"
    )
  
  g3 <- ggplot(draws_clean %>% filter(parameter == "r_1"), aes(x = value)) +
    geom_density(fill = "grey70", alpha = 0.7) +
    labs(x = "r, intrinsic growth rate (1/yr)", y = "Density") +
    plot_theme
  
  g3
  
  g4 <- ggplot(draws_clean %>% filter(parameter == "k_1"), aes(x = value)) +
    geom_density(fill = "grey70", alpha = 0.7) +
    labs(x = "K, carrying capacity", y = "Density") +
    plot_theme
  
  g4
  
  g5 <- ggplot(draws_clean %>% filter(parameter == "N_latest"), aes(x = value)) +
    geom_density(fill = "grey70", alpha = 0.7) +
    labs(x = "Latest abundance (N)", y = "Density") +
    plot_theme
  
  g5 
  
  layout_matrix <- matrix (data = c(1, 1, 3,
                                    1, 1, 4,
                                    2, 2, 5), ncol = 3, byrow = T)
  
  g_total <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, layout_matrix = layout_matrix)
  
  
  return(g_total)
  
}


