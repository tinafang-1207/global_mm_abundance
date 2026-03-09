
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)

### read in data ###
original_df <- read.csv("data/clean_data/ca_harbor_porpoise_input_df.csv")
posterior_draw_monterey <- read.csv("data/exp_data/posterior_draw_2k_hp_monterey.csv")
prior_draw_monterey <- read.csv("data/exp_data/prior_draw_2k_hp_monterey.csv")
output_monterey <- read.csv("data/exp_data/output_2k_hp_monterey.csv")



### posterior
posterior_draw_clean_monterey <- posterior_draw_monterey %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  gather(key = "parameter", value = "value", r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  mutate(draw_type = "Posterior")

posterior_k_monterey <- posterior_draw_clean_monterey %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  filter(parameter == "k_1")

posterior_mnpl_monterey <- posterior_draw_clean_monterey %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  filter(parameter == "MNPL")

posterior_r_monterey <- posterior_draw_clean_monterey %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  filter(parameter == "r_1")

posterior_p_monterey <- posterior_draw_clean_monterey %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  filter(parameter == "P_initial_1")

posterior_m_monterey <- posterior_draw_clean_monterey %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  filter(parameter == "m_1")

posterior_key_parameter_estimates <- bind_rows(posterior_k_monterey, posterior_r_monterey, posterior_p_monterey, posterior_m_monterey) %>%
  mutate(parameter = case_when(parameter == "r_1"~"Growth rate(r)",
                               parameter == "k_1"~"Carrying capacity(k)",
                               parameter == "P_initial_1"~"Initial population proportion(P0)",
                               parameter == "m_1"~"Shape parameter(m)")) %>%
  mutate(label = round(median, 2)) %>%
  mutate(y = case_when(parameter == "Growth rate(r)"~20,
                       parameter == "Carrying capacity(k)"~0.0003,
                       parameter == "Initial population proportion(P0)"~5,
                       parameter == "Shape parameter(m)"~0.03))

### prior

prior_draw_clean_monterey <- prior_draw_monterey %>%
  select(r_1, k_1, P_initial_1, m_1) %>%
  gather(key = "parameter", value = "value", r_1, k_1, P_initial_1, m_1) %>%
  mutate(draw_type = "Prior")

parameter_all <- posterior_draw_clean_monterey %>%
  filter(!parameter%in%c("MNPL", "sigma_sq_1", "tau_sq_1")) %>%
  bind_rows(prior_draw_clean_monterey) %>%
  mutate(parameter = case_when(parameter == "r_1"~"Growth rate(r)",
                               parameter == "k_1"~"Carrying capacity(k)",
                               parameter == "P_initial_1"~"Initial population proportion(P0)",
                               parameter == "m_1"~"Shape parameter(m)")) %>%
  mutate(draw_type = factor(draw_type, levels = c("Prior", "Posterior")))

### make plot of parameter ###
plot_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=8),
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

draw_fill <- c("Posterior" = "#5773cc", "Prior" = "#ffb900")

p_parameter_monterey <- ggplot() + 
  geom_density(data = parameter_all, aes(x = value, fill = draw_type), alpha = 0.7) +
  geom_vline(data = posterior_key_parameter_estimates, aes(xintercept = median), color = "red", linetype = "dashed") +
  geom_text(data = posterior_key_parameter_estimates, aes(x = median, y = y, label = label), color = "red", vjust = 1, hjust = -0.3, size = 3.5) +
  facet_wrap(.~parameter, scales = "free") +
  scale_fill_manual(name = "Type of draw", values = draw_fill) +
  labs(y = "Density", x = "") +
  plot_theme + theme(legend.position = "bottom")
  
p_parameter_monterey






### abundance

output_clean_monterey <- output_monterey %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1986:2013) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(stock = "Harbor Porpoise - Monterey Bay") %>%
  filter(estimation_type %in% c("mean", "X2.5.", "X97.5." )) %>%
  mutate(estimation_type = case_when(estimation_type == "X2.5."~"low_ci",
                                     estimation_type == "X97.5."~"high_ci",
                                     .default = estimation_type)) %>%
  rename(year = est_variables, abundance = estimation) %>%
  pivot_wider(names_from = estimation_type, values_from = abundance)


original_df_clean_monterey <- original_df %>%
  filter(stock == "Monterey Bay") %>%
  mutate(stock = "Harbor porpoise - Monterey Bay") %>%
  mutate(estimation_type = "original") %>%
  filter(year >= 1986 & year <=2013) %>%
  select(year, estimation_type, abundance) %>%
  pivot_wider(names_from = estimation_type, values_from = abundance)

data_monterey_final <- left_join(output_clean_monterey, original_df_clean_monterey, by = "year") %>%
  rename(est_abundance = mean,
         org_abundance = original)


posterior_k_monterey_abd <- posterior_draw_clean_monterey %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  filter(parameter == "k_1") %>%
  mutate(label = "Carrying capacity (k)") %>%
  mutate(x = 1995)

posterior_mnpl_monterey_abd <- posterior_draw_clean_monterey %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  filter(parameter == "MNPL") %>%
  mutate(label = "Maximum net productivity level (MNPL)") %>%
  mutate(x = 1995)

posterior_abd_all <- bind_rows(posterior_k_monterey_abd, posterior_mnpl_monterey_abd)
  

g_abundance <- ggplot() +
  geom_ribbon(data = data_monterey_final, aes(x = year, ymin = low_ci, ymax = high_ci), fill = "lightblue", alpha = 0.7) +
  geom_point(data = data_monterey_final,aes(x = year, y = org_abundance),color = "red" ,size = 2, shape = 1) +
  geom_point(data = data_monterey_final, aes(x = year, y = est_abundance), color = "darkgreen",size = 2, shape = 1) +
  geom_line(data = data_monterey_final,mapping = aes(x = year, y = est_abundance), linetype = 1) +
  geom_hline(yintercept = posterior_k_monterey_abd$median, color = "blue", linetype = 2 ) +
  geom_hline(yintercept = posterior_mnpl_monterey_abd$median, color = "red", linetype = 2) +
  geom_text(data = posterior_abd_all, aes(x = x, y = median, label = label), color = "black", vjust = 1.5, size = 3.5) +
  geom_text() +
  facet_wrap(.~stock) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme

g_abundance

### catch
output_clean_monterey_catch <- output_monterey %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "C_pred")) %>%
  mutate(est_variables = 1986:2013) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(stock = "Harbor Porpoise - Monterey Bay") %>%
  filter(estimation_type %in% c("mean", "X2.5.", "X97.5." )) %>%
  mutate(estimation_type = case_when(estimation_type == "X2.5."~"low_ci",
                                     estimation_type == "X97.5."~"high_ci",
                                     .default = estimation_type)) %>%
  rename(year = est_variables, catch = estimation) %>%
  pivot_wider(names_from = estimation_type, values_from = catch)

original_df_clean_monterey_catch <- original_df %>%
  filter(stock == "Monterey Bay") %>%
  mutate(stock = "Harbor porpoise - Monterey Bay") %>%
  mutate(estimation_type = "original") %>%
  filter(year >= 1986 & year <=2013) %>%
  select(year, estimation_type, total_catch) %>%
  pivot_wider(names_from = estimation_type, values_from = total_catch)

data_monterey_final_catch <- left_join(output_clean_monterey_catch, original_df_clean_monterey_catch, by = "year") %>%
  rename(est_catch = mean,
         org_catch = original)


g_catch <- ggplot() +
  # geom_ribbon(data = data_monterey_final_catch, aes(x = year, ymin = low_ci, ymax = high_ci), fill = "lightblue", alpha = 0.7) +
  geom_point(data = data_monterey_final_catch, aes(x = year, y = org_catch+0.01),color = "red" ,size = 2, shape = 1) +
  geom_point(data = data_monterey_final_catch, aes(x = year, y = est_catch+0.01), color = "darkgreen",size = 2, shape = 1) +
  geom_line(data = data_monterey_final_catch, mapping = aes(x = year, y = est_catch+0.01), linetype = 1) +
  facet_wrap(.~stock) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  scale_y_continuous(trans="log10", 
                     breaks=c(1, 10, 100, 1000), 
                     labels=c("1", "10", "100", "1000")) +
  labs(x = "Year", y = "Estimated Catch") +
  plot_theme

g_catch


ggsave(g_abundance, filename=file.path("figures/monterey_bay_Schmidt.png"), 
       width=5, height=4.5, units="in", dpi=600)

ggsave(p_parameter_monterey, filename = file.path("figures/monterey_bay_Schmidt_parameter.png"),
       width = 5, height = 4.5, units = "in", dpi = 600)


