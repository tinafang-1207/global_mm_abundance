
functions {
  real N_PT_temp(real N, real r, real z, real k,
                 real E, real impact_E, real Catch) {
    return fmax(
      N + r * exp(impact_E * E) * N * (1 - pow(N / k, z)) - Catch,
      0.0001
    );
  }
}

data {
  int<lower=1> N_1;
  vector[N_1] Catch_1;
  vector[N_1] Environment_1;
  real<lower=0> low_r;
  real<lower=0> high_r;
  real<lower=0> low_k;
  real<lower=0> high_k;
  real<lower=0> z_1;
  vector<lower=0>[N_1] sigma_1;
}

generated quantities {
  real<lower=low_r, upper=high_r> r_1;
  real<lower=low_k, upper=high_k> k_1;
  real<lower=0, upper=1> P_initial_1;
  real impact_E_1;

  vector<lower=0>[N_1] N_med;
  vector<lower=0>[N_1] Abundance_pred;

  // draw from priors
  r_1 = uniform_rng(low_r, high_r);
  k_1 = uniform_rng(low_k, high_k);
  P_initial_1 = beta_rng(1, 1);
  impact_E_1 = uniform_rng(-1, 1);

  // initial abundance
  N_med[1] = k_1 * P_initial_1;

  // state process
  for (t in 2:N_1) {
    N_med[t] = N_PT_temp(
      N_med[t-1],
      r_1, z_1, k_1,
      Environment_1[t-1],
      impact_E_1,
      Catch_1[t-1]
    );
  }

  // observation model simulation
  for (t in 1:N_1) {
    Abundance_pred[t] = lognormal_rng(log(N_med[t]), sigma_1[t]);
  }
}




