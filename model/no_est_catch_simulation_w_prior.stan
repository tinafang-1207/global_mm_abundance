
functions {
  real N_PT(real N, real r, real z, real k, real Catch) {
    return fmax(N + r * N * (1 - pow(N / k, z)) - Catch, 0.0001);
  }

  real MNPL_PT(real k, real z) {
    real PMNPL = pow(1 + z, -1 / z);
    return PMNPL * k;
  }
}

data {
  int<lower=1> N_1;                 // Number of time steps
  vector[N_1] Catch_1;             // Observed catch (in abundance units)
  vector[N_1] Abundance_1;         // Observed abundance
  real<lower=0> low_r;
  real<lower=0> high_r;
  real<lower=0> low_k;
  real<lower=0> high_k;
  real<lower=0> z_1;               // Shape parameter
}

parameters {
  real<lower=low_r, upper=high_r> r_1;
  real<lower=low_k, upper=high_k> k_1;
  real<lower=0.01, upper=0.99> P_initial_1;     // Now interpreted as proportion of K (i.e. N_initial_prop)
  real<lower=0> sigma_sq_1;
}

transformed parameters {
  real<lower=0> sigma_1;
  vector<lower=0>[N_1] N_med;     // Estimated abundance (formerly N_med from generated quantities)
  sigma_1 = sqrt(sigma_sq_1);

  N_med[1] = P_initial_1 * k_1;
  for (t in 2:N_1) {
    N_med[t] = N_PT(N_med[t-1], r_1, z_1, k_1, Catch_1[t-1]);
  }
}

model {
  // Priors
  r_1 ~ uniform(low_r, high_r);
  k_1 ~ uniform(low_k, high_k);
  P_initial_1 ~ beta(1, 1);     // Uniform prior on initial abundance as proportion of K
  sigma_sq_1 ~ inv_gamma(4, 0.01);

  // Likelihood
  for (t in 1:N_1) {
    if (Abundance_1[t] > -1) {
      Abundance_1[t] ~ lognormal(log(N_med[t]), sigma_1);
    }
  }
}

generated quantities {
    // priors
  real<lower=low_r, upper = high_r> r_1_prior_sample = uniform_rng(low_r, high_r);
  real<lower=low_k, upper=high_k> k_1_prior_sample = uniform_rng(low_k, high_k);
  real<lower=0.01, upper=0.99> P_initial_1_prior_sample = beta_rng(1, 1);
  real<lower=0> sigma_sq_1_prior_sample = inv_gamma_rng(4, 0.01);
  
  real MNPL = MNPL_PT(k_1, z_1);
  vector[N_1] Abundance_pred;

  for (t in 1:N_1) {
    Abundance_pred[t] = lognormal_rng(log(N_med[t]), sigma_1);
  }
}
