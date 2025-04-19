
### clean working environment
rm(list = ls())

### read in package
library(tidyverse)

### read in data
original_df <- read.csv("data/clean_data/ca_sealion_input_df.csv")
posterior_draw <- read.csv("data/exp_data/posterior_draw_2k.csv")
output <- read.csv("data/exp_data/output_2k.csv")

### prior

# r
r1 <- 0.01
r2 <- 0.25
x <- seq(0,3, by = 0.01)
y <- dlnorm(x, meanlog=log((r1+r2)/2), sdlog=log( ((r1+r2)/2) - log(r1)) /2 )
plot(y ~ x, type="line")
# 
# # k
k1<- 306220
k2 <-3062200
x<-seq(0, 3062200, 100000)
y <- dlnorm(x, meanlog=log((k1+k2)/2), sdlog=log( ((k1+k2)/2) - log(k1)) /2 )
plot(y ~ x, type="line")
# 
# # F
x<-seq(0,3, by = 0.001)
y<-dexp(x, rate = 1)
plot(y ~ x, type="line")
# 
# P_initial
x <- seq(0, 1, 0.001)
y <- dbeta(x, shape1=1, shape2=1)
plot(y ~ x)
# 
# sigma and tau
x<- seq(0, 1, 0.001)
y<-invgamma::dinvgamma(x, 4, 0.01)
plot(y~x)

# shape parameter m
x<-seq(-1,1,length.out = 1000)
y <-sn::dsn(x, xi = -0.5, omega = 1, alpha = 10)
plot(y~x)

x_m1 <- exp(x)
plot(y~x_m1)

### posterior
posterior_draw_clean <- posterior_draw%>%
  select(r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL) %>%
  gather(key = "Parameter", value = "Value", r_1, k_1, P_initial_1, sigma_sq_1, tau_sq_1, m_1, MNPL)

posterior_k <- posterior_draw_clean %>%
  group_by(Parameter) %>%
  summarize(median = median(Value)) %>%
  filter(Parameter == "k_1")

posterior_mnpl <- posterior_draw_clean %>%
  group_by(Parameter) %>%
  summarize(median = median(Value)) %>%
  filter(Parameter == "MNPL")


p_posterior <- ggplot(posterior_draw_clean, aes(x=Value))+
  geom_density() +
  facet_wrap(.~Parameter, scales = "free")+
  theme_bw()

p_posterior

### abundance



output_clean <- output %>%
  rename(est_variables = X) %>%
  filter(str_detect(est_variables, "N_med")) %>%
  mutate(est_variables = 1975:2014) %>%
  gather(key = "estimation_type", value = "estimation", mean, se_mean, sd, X2.5., X25., X50.,X75.,X97.5., n_eff, Rhat) %>%
  mutate(est_variables = as.numeric(est_variables))

original_df_clean <- original_df %>%
  filter(year <=2014)



g_abundance <- ggplot() +
  geom_point(data = output_clean %>% filter(estimation_type == "mean"),
             aes(x = est_variables, y = estimation), color = "red", size = 2, shape = 1) +
  geom_point(data = original_df_clean, aes(x = year, y = abundance), color = "darkgreen", size = 2, shape = 1) +
  geom_line(data = output_clean%>% filter(estimation_type %in% c("mean", "X2.5.", "X97.5.")), mapping = aes(x = est_variables, y = estimation, group = estimation_type, linetype = estimation_type)) +
  geom_hline(yintercept = posterior_k$median, color = "blue", linetype = 2 ) +
  geom_hline(yintercept = posterior_mnpl$median, color = "red", linetype = 2) +
  geom_hline(yintercept = 275298, color = "blue") +
  geom_hline(yintercept = 183481, color = "red") +
  scale_x_continuous(breaks = seq(1975, 2014, by = 5)) +
  labs(x = "Year", y = "Estimated Abundance") +
  theme_bw()

g_abundance

#######################################################################

# Surplus production

output_clean_surplus <- output %>%
  rename(est_variable = X) %>%
  filter(est_variable %in% c("k_1", "m_1", "r_1") | str_detect(est_variable, "N_med|P_med")) %>%
  select(est_variable, mean) %>%
  gather(key = "estimation_type", value = "estimation", mean) %>%
  select(-estimation_type)

k_1 <- output_clean_surplus %>% filter(est_variable == "k_1") %>% pull(estimation)
r_1 <- output_clean_surplus %>% filter(est_variable == "r_1") %>% pull(estimation)
m_1 <- output_clean_surplus %>% filter(est_variable == "m_1") %>% pull(estimation)

output_surplus_final <- output_clean_surplus %>%
  mutate(surplus_production = if_else(str_starts(est_variable, "P_med"),
                           r_1*estimation*k_1*(1-estimation^m_1),
                           NA_real_)) %>%
  filter(!is.na(surplus_production)) %>%
  select(surplus_production) %>%
  mutate(year = 1975:2014) %>%
  select(year, surplus_production)

output_abundance_final <- output_clean %>%
  filter(estimation_type == "mean") %>%
  select(est_variables, estimation) %>%
  rename(year = est_variables,
         abundance = estimation) 

output_surplus_abundance <- left_join(output_abundance_final, output_surplus_final, by = "year")

ggplot(data = output_surplus_abundance, aes(x = abundance, y = surplus_production)) +
  geom_point()


ggsave(g_abundance, filename=file.path("figures/exp_sealion_trend.png"), 
       width=8, height=4.5, units="in", dpi=600)





