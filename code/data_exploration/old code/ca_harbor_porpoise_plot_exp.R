
### clean working environment
rm(list = ls())

### read in package
library(tidyverse)

### read in data
original_df <- read.csv("data/clean_data/ca_harbor_porpoise_input_df.csv")
posterior_draw_sf <- read.csv("data/exp_data/posterior_draw_hbporpoise_sf_0.6k.csv")
output_sf <- read.csv("data/exp_data/output_hbporpoise_sf_0.6k.csv")


posterior_draw_monterey <- read.csv("data/exp_data/posterior_draw_hbporpoise_mtbay_0.6k.csv")
output_monterey <- read.csv("data/exp_data/output_hbporpoise_mtbay_0.6k.csv")


posterior_draw_morro <- read.csv("data/exp_data/posterior_draw_hbporpoise_mrbay_0.6k.csv")
output_morro <- read.csv("data/exp_data/output_hbporpoise_mrbay_0.6k.csv")

original_df_sl <- read.csv("data/clean_data/ca_sealion_input_df.csv")
posterior_draw_sl <- read.csv("data/exp_data/posterior_draw_ca_sealion_0.6k.csv")
output_sl <- read.csv("data/exp_data/output_ca_sealion_0.6k.csv")

###############################################################
# San Francisco - Russian River

### posterior
posterior_draw_clean_sf <- posterior_draw_sf %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1) %>%
  gather(key = "Parameter", value = "Value", r_1, k_1, P_initial_1, sigma_sq_1)

posterior_k_sf <- posterior_draw_clean_sf %>%
  group_by(Parameter) %>%
  summarize(median = median(Value)) %>%
  filter(Parameter == "k_1")

p_posterior_sf <- ggplot(posterior_draw_clean_sf, aes(x=Value))+
  geom_density() +
  facet_wrap(.~Parameter, scales = "free")+
  theme_bw()

p_posterior_sf 

### abundance

output_clean_sf <- output_sf %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1986:2017) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables))


original_df_clean_sf <- original_df %>%
  filter(stock == "San Francisco - Russian River") %>%
  filter(year >= 1986 & year <=2017)

output_k_sf <- output_sf %>%
  select(X, mean) %>%
  filter(X == "k_1")

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



g_abundance_sf <- ggplot() +
  geom_point(data = output_clean_sf %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation), color = "red", size = 2, shape = 1) +
  geom_point(data = original_df_clean_sf, aes(x = year, y = abundance), color = "darkgreen", size = 2, shape = 1) +
  facet_wrap(.~stock) +
  geom_line(data = output_clean_sf%>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_hline(yintercept = output_k_sf$mean, color = "blue", linetype = 2 ) +
  geom_hline(yintercept = 0.6*(output_k_sf$mean), color = "red", linetype = 2) +
  scale_x_continuous(breaks = seq(1985, 2020, by = 5)) +
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme + theme(legend.position = "none")

g_abundance_sf

############################################################################

# Monterey Bay

### posterior
posterior_draw_clean_monterey <- posterior_draw_monterey %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1) %>%
  gather(key = "Parameter", value = "Value", r_1, k_1, P_initial_1, sigma_sq_1)


p_posterior_monterey <- ggplot(posterior_draw_clean_monterey, aes(x=Value))+
  geom_density() +
  facet_wrap(.~Parameter, scales = "free")+
  theme_bw()

p_posterior_monterey

mt_fit <- readRDS("data/exp_data/fit_hbporpoise_mtbay_0.6k.rds")

pairs(mt_fit, pars = c("r_1", "k_1", "P_initial_1", "sigma_sq_1", "sigma_1"))

### abundance

output_clean_mt <- output_monterey %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1986:2013) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables))


original_df_clean_mt <- original_df %>%
  filter(stock == "Monterey Bay") %>%
  filter(year >= 1986 & year <=2013)

output_k_mt <- output_monterey %>%
  select(X, mean) %>%
  filter(X == "k_1")

g_abundance_mt <- ggplot() +
  geom_point(data = output_clean_mt %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation), color = "red", size = 2, shape = 1) +
  geom_point(data = original_df_clean_mt, aes(x = year, y = abundance), color = "darkgreen", size = 2, shape = 1) +
  facet_wrap(.~stock) +
  geom_line(data = output_clean_mt %>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_hline(yintercept = output_k_mt$mean, color = "blue", linetype = 2 ) +
  geom_hline(yintercept = 0.6*(output_k_mt$mean), color = "red", linetype = 2) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme + theme(legend.position = "none")

g_abundance_mt

####################################################################
# Morro Bay

### posterior
posterior_draw_clean_morro <- posterior_draw_morro %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1) %>%
  gather(key = "Parameter", value = "Value", r_1, k_1, P_initial_1, sigma_sq_1)


p_posterior_morro <- ggplot(posterior_draw_clean_morro, aes(x=Value))+
  geom_density() +
  facet_wrap(.~Parameter, scales = "free")+
  theme_bw()

p_posterior_morro 

### abundance

output_clean_morro <- output_morro %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1986:2011) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables))


original_df_clean_morro <- original_df %>%
  filter(stock == "Morro Bay") %>%
  filter(year >= 1986 & year <=2011)

output_k_mr <- output_morro %>%
  select(X, mean) %>%
  filter(X == "k_1")


g_abundance_mr <- ggplot() +
  geom_point(data = output_clean_morro %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation), color = "red", size = 2, shape = 1) +
  geom_point(data = original_df_clean_morro, aes(x = year, y = abundance), color = "darkgreen", size = 2, shape = 1) +
  facet_wrap(.~stock) +
  geom_line(data = output_clean_morro %>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_hline(yintercept = output_k_mr$mean, color = "blue", linetype = 2 ) +
  geom_hline(yintercept = 0.6*(output_k_mr$mean), color = "red", linetype = 2) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme + theme(legend.position = "none")

g_abundance_mr

############################################################################
# sealion

### posterior
posterior_draw_clean_sl <- posterior_draw_sl %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1) %>%
  gather(key = "Parameter", value = "Value", r_1, k_1, P_initial_1, sigma_sq_1)


p_posterior_sl <- ggplot(posterior_draw_clean_sl, aes(x=Value))+
  geom_density() +
  facet_wrap(.~Parameter, scales = "free")+
  theme_bw()

p_posterior_sl 

sl_fit <- readRDS("data/exp_data/fit_ca_sealion_0.6k.rds")

pairs(sl_fit, pars = c("r_1", "k_1", "P_initial_1", "sigma_sq_1", "sigma_1"))
### abundance

output_clean_sl <- output_sl %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1981:2014) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables))


original_df_clean_sl<- original_df_sl %>%
  filter(year >= 1981 & year <=2014) %>%
  mutate(stock = "California sea lion")

output_k_sl <- output_sl %>%
  select(X, mean) %>%
  filter(X == "k_1")

g_abundance_sl <- ggplot() +
  geom_point(data = output_clean_sl %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation), color = "red", size = 2, shape = 1) +
  geom_point(data = original_df_clean_sl, aes(x = year, y = abundance), color = "darkgreen", size = 2, shape = 1) +
  facet_wrap(.~stock) +
  geom_line(data = output_clean_sl %>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_hline(yintercept = output_k_sl$mean, color = "blue", linetype = 2 ) +
  geom_hline(yintercept = 0.6*(output_k_sl$mean), color = "red", linetype = 2) +
  scale_x_continuous(breaks = seq(1981, 2015, by = 5)) +
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme + theme(legend.position = "none")

g_abundance_sl

g_abundance_total <- gridExtra::grid.arrange(g_abundance_sf, g_abundance_mt, g_abundance_mr, g_abundance_sl, nrow = 2, ncol = 2)

ggsave(g_abundance_total, filename=file.path("figures/meeting_Trevor/abundance_update.png"), 
       width=6, height=4.5, units="in", dpi=600)

