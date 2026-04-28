//

functions {
  real N_PT(real N, real r, real z, real k, real Catch) {
    return fmax(N + r * N * (1 - pow(N / k, z)) - Catch, 0.0001);
  }
}

data {
  int<lower=1> N_1;
  vector[N_1] Catch_1;
  vector[N_1] Abundance_1;          // observed abundance; use -999 for missing
  real<lower=0> r_approx;
  real<lower=0> k_approx;
  real<lower=0> N_init_approx;
  real<lower=0> z_1;
}

parameters {
  real<lower=0> r_1;
  real log_k_1;
  real log_N_init_1;
  // Estimated observer error for Southern sea otter
  real<lower=0> sigma_obs;
}

transformed parameters {
  real<lower=0> k_1 = exp(log_k_1);
  real<lower=0> N_init_1 = exp(log_N_init_1);
  vector<lower=0>[N_1] N_med;

  N_med[1] = N_init_1;

  for (t in 2:N_1) {
    N_med[t] = N_PT(N_med[t-1], r_1, z_1, k_1, Catch_1[t-1]);
  }
}

model {
  // Priors
  r_1 ~ lognormal(log(r_approx), 0.05);
  log_k_1 ~ normal(log(k_approx), 0.1);
  log_N_init_1 ~ normal(log(N_init_approx), 0.1);
  // Prior for observer error
  // Because sigma_obs has lower=0, this is a half-cauchy prior
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

  for (t in 1:N_1) {
    mu_pred[t] = N_med[t];

    Abundance_pred[t] = lognormal_rng(log(N_med[t]), sigma_obs);

    if (Abundance_1[t] > -1) {
      log_lik[t] = lognormal_lpdf(Abundance_1[t] | log(N_med[t]), sigma_obs);
    } else {
      log_lik[t] = 0;
    }
  }
}


