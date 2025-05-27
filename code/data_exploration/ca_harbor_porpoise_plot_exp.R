
### clean working environment
rm(list = ls())

### read in package
library(tidyverse)

### read in data
original_df <- read.csv("data/clean_data/ca_harbor_porpoise_input_df.csv")
posterior_draw_sf <- read.csv("data/exp_data/posterior_draw_2k_hp_sf.csv")
output_sf <- read.csv("data/exp_data/output_2k_hp_sf.csv")
posterior_draw_monterey <- read.csv("data/exp_data/posterior_draw_2k_hp_monterey.csv")
output_monterey <- read.csv("data/exp_data/output_2k_hp_monterey.csv")
posterior_draw_morro <- read.csv("data/exp_data/posterior_draw_2k_hp_morro.csv")
output_morro <- read.csv("data/exp_data/output_2k_hp_morro.csv")

###############################################################
# San Francisco - Russian River


### prior
r1 <- 0.01
r2 <- 0.1
x <- seq(0,1, by = 0.01)
y <- dlnorm(x, meanlog=log((r1+r2)/2), sdlog=log( ((r1+r2)/2) - log(r1)) /2 )
plot(y ~ x, type="line")


### posterior
posterior_draw_clean_sf <- posterior_draw_sf %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  gather(key = "Parameter", value = "Value", r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL)

posterior_k_sf <- posterior_draw_clean_sf %>%
  group_by(Parameter) %>%
  summarize(median = median(Value)) %>%
  filter(Parameter == "k_1")

posterior_mnpl_sf <- posterior_draw_clean_sf %>%
  group_by(Parameter) %>%
  summarize(median = median(Value)) %>%
  filter(Parameter == "MNPL")


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



g_abundance <- ggplot() +
  geom_point(data = output_clean_sf %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation), color = "red", size = 2, shape = 1) +
  geom_point(data = original_df_clean_sf, aes(x = year, y = abundance), color = "darkgreen", size = 2, shape = 1) +
  geom_line(data = output_clean_sf%>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_hline(yintercept = posterior_k_sf$median, color = "blue", linetype = 2 ) +
  geom_hline(yintercept = posterior_mnpl_sf$median, color = "red", linetype = 2) +
  scale_x_continuous(breaks = seq(1985, 2020, by = 5)) +
  labs(x = "Year", y = "Estimated Abundance") +
  theme_bw()

g_abundance

############################################################################

# Monterey Bay

### posterior
posterior_draw_clean_monterey <- posterior_draw_monterey %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  gather(key = "Parameter", value = "Value", r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL)

posterior_k_monterey <- posterior_draw_clean_monterey %>%
  group_by(Parameter) %>%
  summarize(median = median(Value)) %>%
  filter(Parameter == "k_1")

posterior_mnpl_monterey <- posterior_draw_clean_monterey %>%
  group_by(Parameter) %>%
  summarize(median = median(Value)) %>%
  filter(Parameter == "MNPL")

p_posterior_monterey <- ggplot(posterior_draw_clean_monterey, aes(x=Value))+
  geom_density() +
  facet_wrap(.~Parameter, scales = "free")+
  theme_bw()

p_posterior_monterey 

### abundance

output_clean_monterey <- output_monterey %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1986:2013) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables))


original_df_clean_monterey <- original_df %>%
  filter(stock == "Monterey Bay") %>%
  filter(year >= 1986 & year <=2013)



g_abundance <- ggplot() +
  geom_point(data = output_clean_monterey %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation), color = "red", size = 2, shape = 1) +
  geom_point(data = original_df_clean_monterey, aes(x = year, y = abundance), color = "darkgreen", size = 2, shape = 1) +
  geom_line(data = output_clean_monterey %>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_hline(yintercept = posterior_k_monterey$median, color = "blue", linetype = 2 ) +
  geom_hline(yintercept = posterior_mnpl_monterey$median, color = "red", linetype = 2) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  labs(x = "Year", y = "Estimated Abundance") +
  theme_bw()

g_abundance

####################################################################
# Morro Bay

### posterior
posterior_draw_clean_morro <- posterior_draw_morro %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  gather(key = "Parameter", value = "Value", r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL)

posterior_k_morro <- posterior_draw_clean_morro %>%
  group_by(Parameter) %>%
  summarize(median = median(Value)) %>%
  filter(Parameter == "k_1")

posterior_mnpl_morro <- posterior_draw_clean_morro %>%
  group_by(Parameter) %>%
  summarize(median = median(Value)) %>%
  filter(Parameter == "MNPL")

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



g_abundance <- ggplot() +
  geom_point(data = output_clean_morro %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation), color = "red", size = 2, shape = 1) +
  geom_point(data = original_df_clean_morro, aes(x = year, y = abundance), color = "darkgreen", size = 2, shape = 1) +
  geom_line(data = output_clean_morro %>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_hline(yintercept = posterior_k_morro$median, color = "blue", linetype = 2 ) +
  geom_hline(yintercept = posterior_mnpl_morro$median, color = "red", linetype = 2) +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5)) +
  labs(x = "Year", y = "Estimated Abundance") +
  theme_bw()

g_abundance



