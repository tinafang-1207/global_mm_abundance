//

functions {
  real N_PT_temp(real N, real r_mod, real z, real k, real Catch) {
    return fmax(
      N + r_mod * N * (1 - pow(N / k, z)) - Catch,
      0.0001
    );
  }
}

data {
  int<lower=1> N_1;
  vector[N_1] Catch_1;
  vector[N_1] Abundance_1;          // observed abundance; use -999 for missing
  vector[N_1] Environment_1;        // temperature/environment covariate
  real<lower=0> r_approx;
  real<lower=0> k_approx;
  real<lower=0> N_init_approx;
  real<lower=0> z_1;
}

parameters {
  real<lower=0> r_1;                // baseline intrinsic growth rate
  real log_k_1;
  real log_N_init_1;
  real impact_E_1;                  // temperature effect on growth rate
  vector[N_1] eps_t;                // raw annual process error
  real<lower=0> sig_E;              // SD of annual process error
  real<lower=0> sigma_obs;          // estimated observer error
}

transformed parameters {
  real<lower=0> k_1 = exp(log_k_1);
  real<lower=0> N_init_1 = exp(log_N_init_1);

  vector[N_1] eps_proc;
  vector<lower=0>[N_1] R_t;
  vector<lower=0>[N_1] N_med;

  // Bias-corrected lognormal process error
  eps_proc = eps_t * sig_E - 0.5 * square(sig_E);

  // Temperature-modified annual growth rate
  for (t in 1:N_1) {
    R_t[t] = r_1 * exp(impact_E_1 * Environment_1[t] + eps_proc[t]);
  }

  // Population process
  N_med[1] = N_init_1;

  for (t in 1:(N_1 - 1)) {
    N_med[t + 1] = N_PT_temp(
      N_med[t],
      R_t[t],
      z_1,
      k_1,
      Catch_1[t]
    );
  }
}

model {
  // Priors from base otter model
  r_1 ~ lognormal(log(r_approx), 0.05);
  log_k_1 ~ normal(log(k_approx), 0.1);
  log_N_init_1 ~ normal(log(N_init_approx), 0.1);

  // Temperature effect on growth
  impact_E_1 ~ normal(0, 0.5);

  // Annual process error
  eps_t ~ normal(0, 1);
  sig_E ~ cauchy(0, 0.1);

  // Estimated observer error
  sigma_obs ~ cauchy(0, 1);

  // Observation model
  for (t in 1:N_1) {
    if (Abundance_1[t] > -1) {
      Abundance_1[t] ~ lognormal(log(N_med[t]), sigma_obs);
    }
  }
}

generated quantities {
  vector[N_1] Abundance_pred;
  vector[N_1] log_lik;
  vector[N_1] mu_pred;
  vector[N_1] R_pred;

  for (t in 1:N_1) {
    mu_pred[t] = N_med[t];
    R_pred[t] = R_t[t];

    Abundance_pred[t] = lognormal_rng(log(N_med[t]), sigma_obs);

    if (Abundance_1[t] > -1) {
      log_lik[t] = lognormal_lpdf(Abundance_1[t] | log(N_med[t]), sigma_obs);
    } else {
      log_lik[t] = 0;
    }
  }
}



