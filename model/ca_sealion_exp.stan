//This is the experimental population dynamic model for California sea lion

//Reparameterization of N to P = N/K improve the performance of model fitting
//SP_PT: population changes/year (whether increase or decrease)
//catch_PT: numbers of incidental mortality per year
//P_PT: real population status/year minus the incidental mortality
//N_PT: represents real absolute abundance, which is P*k
functions{
  real SP_PT(real P, real r, real k, real m) {
  return r/(m-1)*P*k*(1-P^(m-1));
  }
  
  real catch_PT(real P, real k, real F){
  return P*k*(1-exp(-F));
  }
  
  real P_PT(real P, real r, real k, real m, real F){
  return P+r/(m-1)*P*(1-P^(m-1))-P*(1-exp(-F));
  }
  
  real N_PT(real P, real k){
  return P*k;
  }
}


//define data input in the model
//N_1 defines number of years for specific stock
//Catch_1 represents incidental mortality
//Abundance_1 represents absolute abundance of the stock/year

data {
  int N_1;
  vector[N_1] Catch_1;
  vector[N_1] Abundance_1;
  vector[2] k_1_prior;
  vector[2] r_1_prior;
}

// Defines the parameters estimated in the model
//sigma and tau represents observation errors for absolute abundance and incidental mortality respectively
//m is the shape parameter (model parameter)
//F is the instantaneous mortality (this even exists for marine mammal?)

parameters {
  real <lower=0> r_1;
  real <lower=0> k_1;
  real <lower=0> P_initial_1;
  real <lower=0> sigma_sq_1;
  real <lower=0> tau_sq_1;
  real <lower=0> m_1;
  vector <lower=0> [N_1] F_1;
}

transformed parameters {
  real <lower=0> sigma_1;
  real <lower=0> tau_1;
  vector <lower=0> [N_1] P_med_1;
  vector <lower=0> [N_1] C_pred_1;
  sigma_1 = sqrt(sigma_sq_1);
  tau_1 = sqrt(tau_sq_1);
  P_med_1[1] = P_initial_1;
  C_pred_1[1] = catch_PT(P_med_1[1], k_1, F_1[1]);
  for (t in 2:N_1){
    P_med_1[t] = P_PT(P_med_1[t-1], r_1, k_1, m_1, F_1[t-1]);
    C_pred_1[t] = catch_PT(P_med_1[t], k_1, F_1[t]);
  }
}

//Define the model

model {
 // Specify priors
 r_1~lognormal(log((r_1_prior[1]+r_1_prior[2])/2),(log((r_1_prior[1]+r_1_prior[2])/2)-log(r_1_prior[1]))/2);
 k_1~lognormal(log((k_1_prior[1]+k_1_prior[2])/2),(log((k_1_prior[1]+k_1_prior[2])/2)-log(k_1_prior[1]))/2);
 sigma_sq_1~inv_gamma(4, 0.01);
 tau_sq_1~inv_gamma(4, 0.01);
 F_1~exponential(1);
 P_initial_1~beta(1,1);
 log(m_1)~skew_normal(-0.5,1,10);
 target += -log(m_1);
 
 //observation of absolute abundance and incidental mortality
 //don't think we need catchability (q) here because the original model is in CPUE as biomass index rather than absolute abundance
 for(t in 1:N_1){
   if(Abundance_1[t]>(-1)){
   Abundance_1[t]~lognormal(log(k_1*P_med_1[t]), sigma_1);
   }
   if(Catch_1[t]>(-1)){
   Catch_1[t]~lognormal(log(C_pred_1[t]), tau_1); 
   }
 }
}

// specify generated quantities - want to estimate the absolute abundance
// why is it 2 inside MNPL_PT(), Shouldn't it be PMSY?

generated quantities {
  vector[N_1] N_med;
  
  for (t in 1:N_1){
    N_med[t] = N_PT(P_med_1[t], k_1);
  }
}











