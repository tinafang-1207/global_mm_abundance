
data{
  int<lower=0> Nd; //Total number of projection year
  array[Nd] int<lower=0> Catch; //Catch statistcs  
  int<lower=0> Nab; //Total number of abundance estimates
  array[Nab] int<lower=0> NYab;  //Number years of abundance estimates
  array[Nab] real<lower=0> Abund; //Abundance
  array[Nab] real<lower=0> sigma; //sigma
}
parameters{
  real<lower=1000,upper=100000> K;
  real<lower=0.01,upper=0.08> r;
  real<lower=0.001,upper=0.9> P0K;
}
transformed parameters {
  vector[Nd] P;
  //Population dynamics
  P[1] = K*P0K;
  for(i in 2:Nd){
    P[i] = fmax(P[i-1]+r*P[i-1]*(1-pow(P[i-1]/K,2.39))-Catch[i-1],0.0001);
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
    Px[i] = fmax(Px[i-1]+r*Px[i-1]*(1-pow(Px[i-1]/K,2.39))-Catch[i-1],0.0001);
  }
  for(j in 1:Nab){
    LL[j] = lognormal_lpdf(Abund[j]|log(Px[NYab[j]]),sigma[j]);  
  }
}

