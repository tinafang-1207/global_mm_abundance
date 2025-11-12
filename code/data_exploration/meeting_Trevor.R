
### clean working environment ###
rm(list = ls())

### load in library ###
library(tidyverse)

### read in data ###

# original df
original_df_hp <- read.csv("data/clean_data/ca_harbor_porpoise_input_df.csv")
original_df_sl <- read.csv("data/clean_data/ca_sealion_input_df.csv")

# posterior & prior output

# Monterey
posterior_draw_monterey <- read.csv("data/meeting_Trevor/posterior_draw_hp_monterey.csv")
prior_draw_monterey <- read.csv("data/meeting_Trevor/prior_draw_hp_monterey.csv")
output_monterey <- read.csv("data/meeting_Trevor/posterior_hp_monterey.csv")

# San Francisco
posterior_draw_sf <- read.csv("data/meeting_Trevor/posterior_draw_hp_sf.csv")
prior_draw_sf <- read.csv("data/meeting_Trevor/prior_draw_hp_sf.csv")
output_sf <- read.csv("data/meeting_Trevor/posterior_hp_sf.csv")

# Morro Bay
posterior_draw_morro <- read.csv("data/meeting_Trevor/posterior_draw_hp_morro.csv")
prior_draw_morro <- read.csv("data/meeting_Trevor/prior_draw_hp_morro.csv")
output_morro <- read.csv("data/meeting_Trevor/posterior_hp_morro.csv")

# CA sea lion
posterior_draw_sl <- read.csv("data/meeting_Trevor/posterior_draw_2k_ca_sealion.csv")
prior_draw_sl <- read.csv("data/meeting_Trevor/prior_draw_2k_ca_sealion.csv")
output_sl <- read.csv("data/meeting_Trevor/output_2k_ca_sealion.csv")


###################################################################################
# clean data abundance

# Monterey
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

# SF
output_clean_sf <- output_sf %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1986:2017) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(stock = "Harbor Porpoise - SF/Russian River") %>%
  filter(estimation_type %in% c("mean", "X2.5.", "X97.5." )) %>%
  mutate(estimation_type = case_when(estimation_type == "X2.5."~"low_ci",
                                     estimation_type == "X97.5."~"high_ci",
                                     .default = estimation_type)) %>%
  rename(year = est_variables, abundance = estimation) %>%
  pivot_wider(names_from = estimation_type, values_from = abundance)

# Morro bay 
output_clean_morro <- output_morro %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1986:2011) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(stock = "Harbor Porpoise - Morro Bay") %>%
  filter(estimation_type %in% c("mean", "X2.5.", "X97.5." )) %>%
  mutate(estimation_type = case_when(estimation_type == "X2.5."~"low_ci",
                                     estimation_type == "X97.5."~"high_ci",
                                     .default = estimation_type)) %>%
  rename(year = est_variables, abundance = estimation) %>%
  pivot_wider(names_from = estimation_type, values_from = abundance)

# CA sealion
output_clean_sl <- output_sl %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1975:2014) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(stock = "California Sealion (U.S.)") %>%
  filter(estimation_type %in% c("mean", "X2.5.", "X97.5." )) %>%
  mutate(estimation_type = case_when(estimation_type == "X2.5."~"low_ci",
                                     estimation_type == "X97.5."~"high_ci",
                                     .default = estimation_type)) %>%
  rename(year = est_variables, abundance = estimation) %>%
  pivot_wider(names_from = estimation_type, values_from = abundance)

abundance_clean_all <- rbind(output_clean_monterey, output_clean_sf, output_clean_morro, output_clean_sl)

# original abundance
sl_orig_clean <- original_df_sl %>%
  mutate(stock = "California Sealion (U.S.)")

hp_orig_clean <- original_df_hp %>%
  mutate(stock = case_when(stock == "Monterey Bay"~"Harbor Porpoise - Monterey Bay",
                           stock == "San Francisco - Russian River"~"Harbor Porpoise - SF/Russian River",
                           stock == "Morro Bay"~"Harbor Porpoise - Morro Bay"))

abundance_orig_clean_all <- rbind(hp_orig_clean, sl_orig_clean) %>%
  drop_na() %>%
  filter(abundance != -999)

#################################################################
# clean data catch
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

output_clean_sf_catch <- output_sf %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "C_pred")) %>%
  mutate(est_variables = 1986:2017) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(stock = "Harbor Porpoise - SF/Russian River") %>%
  filter(estimation_type %in% c("mean", "X2.5.", "X97.5." )) %>%
  mutate(estimation_type = case_when(estimation_type == "X2.5."~"low_ci",
                                     estimation_type == "X97.5."~"high_ci",
                                     .default = estimation_type)) %>%
  rename(year = est_variables, abundance = estimation) %>%
  pivot_wider(names_from = estimation_type, values_from = abundance)

output_clean_morro_catch <- output_morro %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "C_pred")) %>%
  mutate(est_variables = 1986:2011) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(stock = "Harbor Porpoise - Morro Bay") %>%
  filter(estimation_type %in% c("mean", "X2.5.", "X97.5." )) %>%
  mutate(estimation_type = case_when(estimation_type == "X2.5."~"low_ci",
                                     estimation_type == "X97.5."~"high_ci",
                                     .default = estimation_type)) %>%
  rename(year = est_variables, abundance = estimation) %>%
  pivot_wider(names_from = estimation_type, values_from = abundance)

output_clean_sl_catch <-  output_sl %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "C_pred")) %>%
  mutate(est_variables = 1975:2014) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables)) %>%
  mutate(stock = "California Sealion (U.S.)") %>%
  filter(estimation_type %in% c("mean", "X2.5.", "X97.5." )) %>%
  mutate(estimation_type = case_when(estimation_type == "X2.5."~"low_ci",
                                     estimation_type == "X97.5."~"high_ci",
                                     .default = estimation_type)) %>%
  rename(year = est_variables, abundance = estimation) %>%
  pivot_wider(names_from = estimation_type, values_from = abundance)

catch_clean_all <- rbind(output_clean_monterey_catch, output_clean_sf_catch, output_clean_morro_catch, output_clean_sl_catch)

catch_orig_clean_all <-  rbind(hp_orig_clean, sl_orig_clean) %>%
  drop_na() %>%
  filter(abundance != -999) %>%
  mutate(total_catch = ifelse(total_catch == -999, NA, total_catch))







##################################################################
# clean data posterior
posterior_draw_clean_monterey <- posterior_draw_monterey %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  gather(key = "parameter", value = "value", r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  mutate(draw_type = "Posterior")

posterior_draw_clean_sf <- posterior_draw_sf %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  gather(key = "parameter", value = "value", r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  mutate(draw_type = "Posterior")

posterior_draw_clean_morro <- posterior_draw_morro %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  gather(key = "parameter", value = "value", r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  mutate(draw_type = "Posterior")

posterior_draw_clean_sl <- posterior_draw_sl %>%
  select(-X) %>%
  select(r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  gather(key = "parameter", value = "value", r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  mutate(draw_type = "Posterior")


# median posterior estimates
monterey_md <- posterior_draw_clean_monterey %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  mutate(stock = "Harbor Porpoise - Monterey Bay")

sf_md <- posterior_draw_clean_sf %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  mutate(stock = "Harbor Porpoise - SF/Russian River")

morro_md <- posterior_draw_clean_morro %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  mutate(stock = "Harbor Porpoise - Morro Bay")

sl_md <- posterior_draw_clean_sl %>%
  group_by(parameter) %>%
  summarize(median = median(value)) %>%
  mutate(stock = "California Sealion (U.S.)")

md_clean_all <- rbind(monterey_md, sf_md, morro_md, sl_md)

md_clean_k <- md_clean_all %>%
  filter(parameter == "k_1")

md_clean_mnpl <- md_clean_all %>%
  filter(parameter == "MNPL")

###################################################
#clean data prior
prior_draw_clean_monterey <- prior_draw_monterey %>%
  select(r_1, k_1, P_initial_1, m_1, sigma_sq_1, tau_sq_1) %>%
  gather(key = "parameter", value = "value", r_1, k_1, P_initial_1, m_1, sigma_sq_1, tau_sq_1) %>%
  mutate(draw_type = "Prior")

prior_draw_clean_sf <- prior_draw_sf %>%
  select(r_1, k_1, P_initial_1, m_1, sigma_sq_1, tau_sq_1) %>%
  gather(key = "parameter", value = "value", r_1, k_1, P_initial_1, m_1, sigma_sq_1, tau_sq_1) %>%
  mutate(draw_type = "Prior")

prior_draw_clean_morro <- prior_draw_morro %>%
  select(r_1, k_1, P_initial_1, m_1, sigma_sq_1, tau_sq_1) %>%
  gather(key = "parameter", value = "value", r_1, k_1, P_initial_1, m_1, sigma_sq_1, tau_sq_1) %>%
  mutate(draw_type = "Prior")

prior_draw_clean_sl <- prior_draw_sl %>%
  select(r_1, k_1, P_initial_1, m_1, sigma_sq_1, tau_sq_1) %>%
  gather(key = "parameter", value = "value", r_1, k_1, P_initial_1, m_1, sigma_sq_1, tau_sq_1) %>%
  mutate(draw_type = "Prior")


parameter_clean_monterey <- posterior_draw_clean_monterey %>%
  filter(!parameter == "MNPL") %>%
  bind_rows(prior_draw_clean_monterey) %>%
  mutate(draw_type = factor(draw_type, levels = c("Prior", "Posterior")))

parameter_clean_sf <- posterior_draw_clean_sf %>%
  filter(!parameter == "MNPL") %>%
  bind_rows(prior_draw_clean_sf) %>%
  mutate(draw_type = factor(draw_type, levels = c("Prior", "Posterior")))

parameter_clean_morro <- posterior_draw_clean_morro %>%
  filter(!parameter == "MNPL") %>%
  bind_rows(prior_draw_clean_morro) %>%
  mutate(draw_type = factor(draw_type, levels = c("Prior", "Posterior")))


parameter_clean_sl <- posterior_draw_clean_sl %>%
  filter(!parameter == "MNPL") %>%
  bind_rows(prior_draw_clean_sl) %>%
  mutate(draw_type = factor(draw_type, levels = c("Prior", "Posterior")))




#####################################################################

# make figure
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
  geom_ribbon(data = abundance_clean_all, aes(x = year, ymin = low_ci, ymax = high_ci), fill = "lightblue", alpha = 0.7) +
  geom_point(data = abundance_clean_all,aes(x = year, y = mean),color = "red" ,size = 2, shape = 1) +
  geom_point(data = abundance_orig_clean_all, aes(x = year, y = abundance), color = "darkgreen",size = 2, shape = 1) +
  geom_line(data = abundance_clean_all,mapping = aes(x = year, y = mean), linetype = 1) +
  #geom_hline(data = md_clean_all %>% filter(parameter == "k_1"), aes(yintercept = median), color = "blue", linetype = 2 ) +
  #geom_hline(data = md_clean_all %>% filter(parameter == "MNPL"), aes(yintercept = median), color = "red", linetype = 2) +
  facet_wrap(.~stock, scales = "free") +
  labs(x = "Year", y = "Estimated Abundance") +
  plot_theme

g_abundance

g_catch <- ggplot() +
  geom_point(data = catch_clean_all, aes(x = year, y = mean+0.01),color = "red" ,size = 2, shape = 1) +
  geom_point(data = catch_orig_clean_all, aes(x = year, y = total_catch+0.01), color = "darkgreen",size = 2, shape = 1) +
  geom_line(data = catch_clean_all, mapping = aes(x = year, y = mean+0.01), linetype = 1) +
  facet_wrap(.~stock, scales = "free") +
  scale_y_continuous(trans="log10", 
                     breaks=c(1, 10, 100, 1000, 10000), 
                     labels=c("1", "10", "100", "1000", "10000")) +
  labs(x = "Year", y = "Estimated Catch (log10 transform)") +
  plot_theme

g_catch

#######################################################

draw_fill <- c("Posterior" = "#5773cc", "Prior" = "#ffb900")

g_parameter_monterey <- ggplot() + 
  geom_density(data = parameter_clean_monterey, aes(x = value, fill = draw_type), alpha = 0.7) +
  geom_vline(data = md_clean_all %>% filter(stock == "Harbor Porpoise - Monterey Bay") %>% filter(parameter != "MNPL"), aes(xintercept = median), color = "red", linetype = "dashed") +
  facet_wrap(.~parameter, scales = "free") +
  scale_fill_manual(name = "Type of draw", values = draw_fill) +
  labs(y = "Density", x = "") +
  plot_theme + theme(legend.position = "bottom")

g_parameter_monterey

g_parameter_sf <- ggplot() + 
  geom_density(data = parameter_clean_sf, aes(x = value, fill = draw_type), alpha = 0.7) +
  geom_vline(data = md_clean_all %>% filter(stock == "Harbor Porpoise - SF/Russian River") %>% filter(parameter != "MNPL"), aes(xintercept = median), color = "red", linetype = "dashed") +
  facet_wrap(.~parameter, scales = "free") +
  scale_fill_manual(name = "Type of draw", values = draw_fill) +
  labs(y = "Density", x = "") +
  plot_theme + theme(legend.position = "bottom")

g_parameter_sf

g_parameter_morro <- ggplot() + 
  geom_density(data = parameter_clean_morro, aes(x = value, fill = draw_type), alpha = 0.7) +
  geom_vline(data = md_clean_all %>% filter(stock == "Harbor Porpoise - Morro Bay") %>% filter(parameter != "MNPL"), aes(xintercept = median), color = "red", linetype = "dashed") +
  facet_wrap(.~parameter, scales = "free") +
  scale_fill_manual(name = "Type of draw", values = draw_fill) +
  labs(y = "Density", x = "") +
  plot_theme + theme(legend.position = "bottom")

g_parameter_morro

g_parameter_sl <- ggplot() + 
  geom_density(data = parameter_clean_sl, aes(x = value, fill = draw_type), alpha = 0.7) +
  geom_vline(data = md_clean_all %>% filter(stock == "California Sealion (U.S.)") %>% filter(parameter != "MNPL"), aes(xintercept = median), color = "red", linetype = "dashed") +
  facet_wrap(.~parameter, scales = "free") +
  scale_fill_manual(name = "Type of draw", values = draw_fill) +
  labs(y = "Density", x = "") +
  plot_theme + theme(legend.position = "bottom")

g_parameter_sl


ggsave(g_abundance, filename=file.path("figures/meeting_Trevor/abundance.png"), 
              width=6, height=4.5, units="in", dpi=600)

ggsave(g_catch, filename=file.path("figures/meeting_Trevor/catch.png"), 
       width=6, height=4.5, units="in", dpi=600)

ggsave(g_parameter_monterey, filename=file.path("figures/meeting_Trevor/p_monterey.png"), 
       width=6, height=4.5, units="in", dpi=600)

ggsave(g_parameter_sf, filename=file.path("figures/meeting_Trevor/p_sf.png"), 
       width=6, height=4.5, units="in", dpi=600)

ggsave(g_parameter_morro, filename=file.path("figures/meeting_Trevor/p_morro.png"), 
       width=6, height=4.5, units="in", dpi=600)

ggsave(g_parameter_sl, filename=file.path("figures/meeting_Trevor/p_sealion.png"), 
       width=6, height=4.5, units="in", dpi=600)






