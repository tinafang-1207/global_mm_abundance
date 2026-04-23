
// The temperature effects apply to K 

functions {
  real N_PT_temp(real N, real r, real z, real K_mod, real Catch) {
    return fmax(
      N + r * N * (1 - pow(N / K_mod, z)) - Catch,
      0.0001
    );
  }
}

data {
  int<lower=1> N_1;
  vector[N_1] Catch_1;
  vector[N_1] Abundance_1;          // observed abundance, -999 = missing
  vector[N_1] Environment_1;        // environmental covariate for all years
  real<lower=0> r_approx; // mean of r in prior lognormal distribution
  real<lower=0> k_approx; // mean of k in prior lognormal distribution
  real<lower=0> N_init_approx; // mean of N_init in prior lognormal distribution
  real<lower=0> z_1;
  vector<lower=0>[N_1] sigma_1;     // observation SD on log scale
}

parameters {
  real<lower=0> r_1;                // intrinsic growth rate
  real log_k_1;                     // carrying capacity (in log scale)
  real log_N_init_1;                // initial abundance (in log scale)
  real impact_E_1;                  // environmental effect on carrying capacity
  vector[N_1] eps_t;                // raw yearly process error
  real<lower=0> sig_E;              // SD of yearly process error
}

transformed parameters {
  real<lower=0> k_1 = exp(log_k_1);
  real<lower=0> N_init_1 = exp(log_N_init_1);
  vector[N_1] eps_proc;             
  vector<lower=0>[N_1] K_t;         // yearly environment-modified carrying capacity
  vector<lower=0>[N_1] N_med;       // latent abundance trajectory

  // log-scale process error with mean correction so E[exp(error)] = 1
  eps_proc = eps_t * sig_E - 0.5 * square(sig_E);

  // yearly modified carrying capacity
  for (t in 1:N_1) {
    K_t[t] = k_1 * exp(impact_E_1 * Environment_1[t] + eps_proc[t]);
  }

  // latent abundance process
  N_med[1] = N_init_1;

  for (t in 1:(N_1 - 1)) {
    N_med[t + 1] = N_PT_temp(
      N_med[t],
      r_1,
      z_1,
      K_t[t],
      Catch_1[t]
    );
  }
}

model {
  // Priors
  r_1 ~ lognormal(log(r_approx), 0.25);
  log_k_1 ~ normal(log(k_approx), 0.5);
  log_N_init_1 ~ normal(log(N_init_approx), 0.5);
  impact_E_1 ~ normal(0, 2); //could assign a wider prior
  eps_t ~ normal(0, 1);
  sig_E ~ cauchy(0, 0.1);   // half-Cauchy because constrained > 0

  // Observation model
  for (t in 1:N_1) {
    if (Abundance_1[t] > -1) {
      Abundance_1[t] ~ lognormal(log(N_med[t]), sigma_1[t]);
    }
  }
}

generated quantities {
  vector[N_1] Abundance_pred;
  vector[N_1] mu_pred;
  vector[N_1] log_lik;

  for (t in 1:N_1) {
    mu_pred[t] = N_med[t];
    Abundance_pred[t] = lognormal_rng(log(N_med[t]), sigma_1[t]);

    if (Abundance_1[t] > -1) {
      log_lik[t] = lognormal_lpdf(Abundance_1[t] | log(N_med[t]), sigma_1[t]);
    } else {
      log_lik[t] = 0;
    }
  }
}




