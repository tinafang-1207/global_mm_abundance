
functions {
   vector zcalc(vector y_init, vector t, array[] real x_r, array[] int x_i){
   vector[1] y;
   y[1] = t[1]-(1/pow(y_init[1]+1,1/y_init[1]));
   return y;
  }
}
data{
  int<lower=0> Nd; //Total number of projection year
  array[Nd] int<lower=0> Catch; //Catch statistcs  
  int<lower=0> Nab; //Total number of abundance estimates
  array[Nab] int<lower=0> NYab;  //Number years of abundance estimates
  array[Nab] real<lower=0> Abund; //Abundance
  array[Nab] real<lower=0> sigma; //sigma
  vector<lower=0,upper=35>[1] THETA_INIT;
}
transformed data{
  array[0] real x_r;
  array[0] int x_i;
}
parameters{
  real<lower=1000,upper=100000> K;
  real<lower=0.01,upper=0.08> r;
  real<lower=0.001,upper=0.9> P0K;
  real<lower=0.38,upper=0.9> Nk1;
}
transformed parameters {
  vector[1] z0;
  vector[1] Nk;
  real z;
  vector[Nd] P;
  Nk[1]=Nk1;
  z0 = algebra_solver(zcalc, THETA_INIT, Nk, x_r, x_i); 
  z = z0[1];
  //Population dynamics
  P[1] = K*P0K;
  for(i in 2:Nd){
    P[i] = fmax(P[i-1]+r*P[i-1]*(1-pow(P[i-1]/K,z))-Catch[i-1],0.0001);
  }
}
model{
  for(j in 1:Nab){
    Abund[j] ~ lognormal(log(P[NYab[j]]),sigma[j]);
  }
} 
generated quantities {
  vector[Nab] LL;  
  vector[Nd] Px;
  Px[1] = K*P0K;
  for(i in 2:Nd){
    Px[i] = fmax(Px[i-1]+r*Px[i-1]*(1-pow(Px[i-1]/K,z))-Catch[i-1],0.0001);
  }
  for(j in 1:Nab){
    LL[j] = lognormal_lpdf(Abund[j]|log(Px[NYab[j]]),sigma[j]);  
  }
}

