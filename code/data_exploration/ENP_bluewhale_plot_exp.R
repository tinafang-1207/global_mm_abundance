
### load in library ###
library(tidyverse)
library(bayesplot)

### set up input directory ###
input_dir <- "data/ENP_blue_whale_output"

### read in data ###
output <- read.csv(file.path(input_dir, "stan_output_warmup_50000_iter_1e+05.csv"))
fit <- readRDS(file.path(input_dir, "stan_fit_warmup_50000_iter_1e+05.rds"))
# original abundance data
original <- read.csv("data/ENP_blue_whale_input/ENP_bluewhale_input_df.csv")

### make plot ###

# check pair plots
pairs(fit, pars = c("r_1", "k_1", "P_initial_1"))

# check traceplot
posterior <- rstan::extract(fit, permuted = FALSE)
mcmc_trace(posterior, pars = c("r_1", "k_1", "P_initial_1"))


# posterior
draws <- as.data.frame(fit)

draws_clean <- draws %>%
  select(r_1, k_1, P_initial_1) %>%
  gather(key = "parameter",  value = "value", r_1, k_1, P_initial_1)

p_posterior <- ggplot(draws_clean, aes(x=value))+
  geom_density() +
  facet_wrap(.~parameter, scales = "free")+
  theme_bw()

p_posterior

# P_initial_1 has two peaks but lean towards at carrying capacity
# Maybe the range of P_initial_1 is too wide? 
# If we have confidence the population is near carrying capacity before whaling start

# abundance
output_clean <- output %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1905:2018) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables))

original_clean <- original %>%
  select(year, abundance) %>%
  filter(year >= 1989 & year <= 2018)

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


g_abundance <- ggplot() +
  geom_point(data = output_clean %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation), color = "red", size = 1, shape = 1) +
  geom_point(data = original_clean, aes(x = year, y = abundance), color = "darkgreen", size = 1, shape = 1) +
  geom_line(data = output_clean %>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_hline(yintercept = 2210, color = "blue", linetype = 2 ) +
  geom_hline(yintercept = 3532, color = "red", linetype = 2) +
  scale_x_continuous(breaks = seq(1905, 2018, by = 10)) +
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme + theme(legend.position = "none")


g_abundance





