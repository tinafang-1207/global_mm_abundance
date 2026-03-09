
### load in library ###
library(tidyverse)
library(bayesplot)

### set up input directory ###
input_dir <- "data/confidential/stan_output/Humpback_whale"

### read in data ###
output <- read.csv(file.path(input_dir, "summary_warmup_50000_iter_1e+05.csv"))
fit <- readRDS(file.path(input_dir, "fit_warmup_50000_iter_1e+05.rds"))
# original abundance data
original <- read.csv("data/confidential/input_data/wc_hbk_gray_input.csv") %>%
  filter(species == "Humpback_whale")

### make plot ###

# check pair plots
pairs(fit, pars = c("r_1", "lnmsy_1", "lnk_1", "k_1", "msy_1"))

# check traceplot
posterior <- rstan::extract(fit, permuted = FALSE)
mcmc_trace(posterior, pars = c("r_1", "lnmsy_1", "lnk_1", "k_1", "msy_1"))


# posterior
draws <- as.data.frame(fit)

draws_clean <- draws %>%
  select(r_1, lnmsy_1, lnk_1, k_1, msy_1) %>%
  gather(key = "parameter",  value = "value", r_1, lnmsy_1, lnk_1, k_1, msy_1)

p_posterior <- ggplot(draws_clean, aes(x=value))+
  geom_density() +
  facet_wrap(.~parameter, scales = "free")+
  theme_bw()

p_posterior

# abundance
output_clean <- output %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1854:2018) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(N_over_K = ifelse(estimation_type == "mean", estimation/2450, NA)) %>%
  mutate(species = "CA/OR/WA Humpback whale") %>%
  filter(est_variables <= 2014)

original_clean <- original %>%
  select(year, abundance, catch) %>%
  mutate(abundance = ifelse(abundance == "-999", NA, abundance)) %>%
  filter(year <= 2014)

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
  geom_line(data = output_clean %>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_point(data = output_clean %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation, fill = N_over_K, stroke = 0), size = 2, shape = 21) +
  geom_point(data = original_clean, aes(x = year, y = abundance), color = "blue", size = 2, shape = 1) +
  geom_line(data = original_clean, aes(x = year, y = catch), color = "red") +
  facet_wrap(.~species) + 
  scale_x_continuous(breaks = seq(1854, 2014, by = 20)) +
  paletteer::scale_fill_paletteer_c("grDevices::RdYlGn", name = "N / K", limits = c(0, 1)) + 
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme + theme(legend.position = "none")


g_abundance

### save the figure ###
plot_dir <- "figures"

ggsave(g_abundance, filename=file.path(plot_dir, "ENP_humpbackwhale_abd_est.png"), 
       width=5, height=4, units="in", dpi=600)


