

functions {
  real N_PT(real N, real r, real z, real k, real Catch) {
    return fmax(N + r * N * (1 - pow(N / k, z)) - Catch, 0.0001);
  }
}

data {
  int<lower=1> N_1;                 // Number of time steps
  vector[N_1] Catch_1;             // Observed catch (in abundance units)
  vector[N_1] Abundance_1;         // Observed abundance
  real<lower=0> low_r;
  real<lower=0> high_r;
  real<lower=0> z_1;               // Shape parameter
  vector<lower=0>[N_1] sigma_1;
}

parameters {
  real<lower=low_r, upper=high_r> r_1;
  real lnmsy_1; // replace k_1 as a primary parameter
}

transformed parameters {
  vector<lower=0>[N_1] N_med;
  real lnk_1;
  real<lower=0> k_1;
  real<lower=0> msy_1;
  
  //convert from log-scale to normal-scale
  lnk_1 = log(((z_1 + 1)^((1/z_1) + 1)) *exp(lnmsy_1)/(r_1*z_1));
  k_1 = exp(lnk_1);
  msy_1 = (r_1*z_1*k_1)/((z_1 + 1)^((1/z_1)+1));

  // Hard-coded initial proportion = 1
  N_med[1] = k_1;

  for (t in 2:N_1) {
    N_med[t] = N_PT(N_med[t-1], r_1, z_1, k_1, Catch_1[t-1]);
  }
}

model {
  // Priors
  r_1 ~ uniform(low_r, high_r);
  lnk_1~uniform(7, 9);

  // Likelihood
  for (t in 1:N_1) {
    if (Abundance_1[t] > -1) {
      Abundance_1[t] ~ lognormal(log(N_med[t]), sigma_1[t]);
    }
  }
}

generated quantities {
  vector[N_1] Abundance_pred;

  for (t in 1:N_1) {
    Abundance_pred[t] = lognormal_rng(log(N_med[t]), sigma_1[t]);
  }
}
