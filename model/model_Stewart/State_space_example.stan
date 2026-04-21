//
// This Stan program defines a simple state space model of
// density dependent population growth, for a population with
// multiple semi-independent stocks (e.g. harbour seals)
//
// Input data
data {
  int<lower=0> Nstk;           // Number of stocks (sub-populations) 
  int<lower=0> Nyrs;           // Number of years population dynamics
  int<lower=0> Nsrv;           // Number of population surveys (in total, all stocks)  
  array[Nsrv] int<lower=0> yr ;// Year of each survey
  array[Nsrv] int<lower=0> st ;// Stock ID of each survey  
  array[Nsrv] int<lower=0> Obs;// Observed population count for each survey  
  // array[Nsrv] real<lower=0> Obs_se ;// Design-based estimates of observer error, if available 
  int<lower=0> MaxN;           // Maximum feasible abundance at K for any stock
  vector<lower=0>[Nstk] Area ; // Habitat area of each stock (km2)
}
//
transformed data {
  // Nvals vector used to calculate MNPL value for each stock   
  vector<lower=0>[MaxN*10] Nvals = linspaced_vector(MaxN*10, 0.1, MaxN ) ;
}
// The parameters accepted by the model. 
parameters {
  real<lower=0> rmax;           // maximum growth rate
  real<lower=0> z;              // growth rate inflection (theta of theta-logistic) 
  real log_Kdns_mn ;            // log of mean K density overall
  vector[Nstk] log_Kdns ;       // log density at K for each stock
  vector<lower=0>[Nstk] log_N0 ;// initial log abundance for each stock at year 1
  real<lower=0> sigma_K ;       // variation in log K densiy among stocks
  real<lower=0> sigma_e ;       // stochastic variation in population growth rate (env. stochasticity)
  array[Nstk] vector[Nyrs-1] eps ; // stopchastic effects by stock and year 
  real<lower=0> phi ;           // inverse scale param for negastive binom: observer error 
}
// Transformed and derived parameters
transformed parameters {
  matrix[Nstk,Nyrs] N ;         // estimated abundance
  vector<lower=0>[Nstk] K_dens ;// density at K by stock 
  vector<lower=0>[Nstk] K ;     // mean abundance at K by stock
  K_dens = exp(log_Kdns) ;   
  K = K_dens .* Area ;
  for(i in 1:Nstk){
    N[i,1] = exp(log_N0[i]) ;
    for(t in 2:Nyrs){
        real lambda ;
        // Option 1: env stochasticity is density-independent
        lambda = exp(rmax * (1 - pow( N[i,t-1] / K[i] , z) ) + sigma_e * eps[i,t-1] ) ;
        // Option 2: env stochasticity is density-dependent (i.e. "variable annual K")
        // lambda = exp(rmax * (1 - pow( N[i,t-1] / (K[i] * exp(sigma_e * eps[i,t-1] - square(sigma_e)/2 )) , z) ) ) ;
        N[i,t] = N[i,t-1] * lambda ;
    }
  }
}
// The model to be estimated. 
model {
  // Observed data:  
  // (neg binom distribution for discreet counts: alternatively, for continuous
  //    non-integer values, could use log-normal or gamma distribtion,
  //    and could use design-based std. error estimates for observation error )
  for(j in 1:Nsrv){
    Obs[j] ~ neg_binomial_2( N[st[j] , yr[j]] , phi) ;
  }
  // Priors:
  rmax ~ rnorm(.2,.025) ;         // informed prior for r
  z ~ gamma(1,1) ;
  log_Kdns_mn ~ normal(1,2.5) ;   // adjust mean as necessary: weakly informed mean log K density
  log_N0 ~ cauchy(0,1) ;          // initial abundance of eqch stock, uninformative haf-cauchy
  sigma_K ~ cauchy(0,.1) ;
  sigma_e ~ cauchy(0,.1) ;
  phi ~ cauchy(0,1) ;
  // Random effects:
  log_Kdns ~ normal(log_Kdns_mn, sigma_K) ; // log K density by stock
  for(i in 1:Nstk){
    eps[i] ~ normal(0,1) ;
  }
}
// Post-fitting derived values 
generated quantities{
  vector[Nstk] MNPL ;
  // NOTE: very close approximation: MNPL = pow(K,0.9945) * pow((z + 1),(-1/z)) ;
  for(i in 1:Nstk){
    vector[MaxN*10] GR ;
    GR[1] = 0.1 * exp(rmax * (1 - pow( 0.1 / K[i] , z) ) ) - 0.1 ;
    MNPL[i] = 0.1 ;
    for(j in 2:(MaxN*10)){
       real n = Nvals[j] ;
       GR[j] = n * exp(rmax * (1 - pow( n / K[i] , z) ) ) - n ;
       MNPL[i] = GR[j] >= GR[j-1] ? n : MNPL[i] ;
    }
  }
}
