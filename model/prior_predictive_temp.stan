

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
  vector[N_1] Environment_1;
  real<lower=0> low_k;
  real<lower=0> high_k;
  real<lower=0> z_1;
  real<lower=0> r_approx;
  real<lower=0> N_init_approx;
  vector<lower=0>[N_1] sigma_1;
}

generated quantities {
  // prior draws
  real<lower=0> r_1;
  real<lower=low_k, upper=high_k> k_1;
  real<lower=0> N_init_1;
  real impact_E_1;
  real<lower=0> sig_E;
  vector[N_1] eps_t;
  vector[N_1] eps_proc;
  vector<lower=0>[N_1] R_t;
  vector<lower=0>[N_1] N_med;
  vector<lower=0>[N_1] Abundance_pred;

  // draw from priors
  r_1 = lognormal_rng(log(r_approx), 0.4);
  k_1 = uniform_rng(low_k, high_k);
  N_init_1 = gamma_rng(N_init_approx * 0.0005, 0.0005);

  // change this line to test different priors for impact_E_1
  impact_E_1 = normal_rng(0, 2);

  // same scale prior as your fitted model
  sig_E = fabs(cauchy_rng(0, 0.1));

  // yearly stochasticity
  for (t in 1:N_1) {
    eps_t[t] = normal_rng(0, 1);
    eps_proc[t] = eps_t[t] * sig_E - 0.5 * square(sig_E);
  }

  // yearly modified growth rate
  for (t in 1:N_1) {
    R_t[t] = r_1 * exp(impact_E_1 * Environment_1[t] + eps_proc[t]);
  }

  // latent abundance trajectory
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

  // simulated observations
  for (t in 1:N_1) {
    Abundance_pred[t] = lognormal_rng(log(N_med[t]), sigma_1[t]);
  }
}



