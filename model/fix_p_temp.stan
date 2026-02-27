
functions {

  // Population dynamics WITH temperature
  real N_PT_temp(real N, real r, real z, real k,
                 real E, real impact_E, real Catch) {
    return fmax(
      N + r * exp(impact_E * E) * N * (1 - pow(N / k, z)) - Catch,
      0.0001
    );
  }

  // Population dynamics WITHOUT temperature
  real N_PT_notemp(real N, real r, real z, real k, real Catch) {
    return fmax(
      N + r * N * (1 - pow(N / k, z)) - Catch,
      0.0001
    );
  }
}

data {
  int<lower=1> N_1;                 
  vector[N_1] Catch_1;             
  vector[N_1] Abundance_1;         // -999 indicates missing
  vector[N_1] Environment_1;
  real<lower=0> low_r;
  real<lower=0> high_r;
  real<lower=0> z_1;
  vector<lower=0>[N_1] sigma_1;    // EXACTLY as in your original model
  int<lower=1, upper=N_1> t_first_obs;
}

parameters {
  real<lower=low_r, upper=high_r> r_1;
  real lnmsy_1;
  real impact_E_1;
}

transformed parameters {
  vector<lower=0>[N_1] N_med;
  real lnk_1;
  real<lower=0> k_1;
  real<lower=0> msy_1;

  // MSY parameterization (unchanged)
  lnk_1 = log(((z_1 + 1)^((1 / z_1) + 1)) * exp(lnmsy_1) / (r_1 * z_1));
  k_1 = exp(lnk_1);
  msy_1 = (r_1 * z_1 * k_1) / ((z_1 + 1)^((1 / z_1) + 1));

  // Initial abundance (unchanged)
  N_med[1] = k_1;

  // State process
  for (t in 2:N_1) {
    if (t >= t_first_obs) {
      // Temperature ON from first observed year onward
      N_med[t] = N_PT_temp(
        N_med[t-1],
        r_1, z_1, k_1,
        Environment_1[t-1],
        impact_E_1,
        Catch_1[t-1]
      );
    } else {
      // Before first observed year: catch-only
      N_med[t] = N_PT_notemp(
        N_med[t-1],
        r_1, z_1, k_1,
        Catch_1[t-1]
      );
    }
  }
}

model {
  // Priors (unchanged)
  r_1~ uniform(low_r, high_r);
  lnk_1~uniform(7, 9);
  impact_E_1 ~ normal(0, 0.5);

  // Observation model (UNCHANGED)
  for (t in 1:N_1) {
    if (Abundance_1[t] > -1) {
      Abundance_1[t] ~ lognormal(log(N_med[t]), sigma_1[t]);
    }
  }
}

generated quantities {
  vector[N_1] Abundance_pred;

  for (t in 1:N_1) {
    Abundance_pred[t] =
      lognormal_rng(log(N_med[t]), sigma_1[t]);
  }
}
