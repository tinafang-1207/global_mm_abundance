

functions {
  real N_PT(real N, real r, real z, real k, real Catch) {
  return fmax(N + r * N * (1 - pow(N / k, z)) - Catch, 0.0001);
  }
  
}

data {
  int<lower=1> N_1;                 // Number of time steps
  vector[N_1] Catch_1;             // Observed catch (in abundance units)
  vector[N_1] Abundance_1; // Observed abundance
  real<lower=0> low_r;
  real<lower=0> high_r;
  //real<lower=0> r_1;
  real<lower=0> sigma_1;
  //real<lower=0> lnmsy_1;
  real<lower=0> low_lnk;
  real<lower=0> high_lnk;
  real<lower=0> z_1;               // Shape parameter
}

parameters {
  real<lower=low_r, upper=high_r> r_1;
  real<lower=0> lnmsy_1;
  //real<lower=0> sigma_sq_1;
}

transformed parameters {
  //real<lower=0> sigma_1;
  real<lower=0> lnk_1;
  //real<lower=0> msy_1;
  real<lower=0> k_1;
  vector<lower=0>[N_1] N_med;     // Estimated abundance (formerly N_med from generated quantities)
  //sigma_1 = sqrt(sigma_sq_1);
  // reparameterized k
  lnk_1 = log(((z_1 + 1)^((1/z_1) + 1)) *exp(lnmsy_1)/(r_1*z_1));
  k_1 = exp(lnk_1);
  //msy_1 = (r_1*k_1*z_1)/(pow(z_1 + 1, 1.0 / (z_1 + 1)));
  
  N_med[1] = k_1;
  for (t in 2:N_1) {
    N_med[t] = N_PT(N_med[t-1], r_1, z_1, k_1, Catch_1[t-1]);
  }
}

model {
  // Priors
  r_1 ~ uniform(low_r, high_r);
  lnk_1 ~ uniform(low_lnk, high_lnk);
  //sigma_sq_1 ~ inv_gamma(4, 0.01);

  // Likelihood
  for (t in 1:N_1) {
    if (Abundance_1[t] > -1) {
      Abundance_1[t] ~ lognormal(log(N_med[t]), sigma_1);
    }
  }
}

generated quantities {
  vector[N_1] Abundance_pred;

  for (t in 1:N_1) {
    Abundance_pred[t] = lognormal_rng(log(N_med[t]), sigma_1);
  }
}
