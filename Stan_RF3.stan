// Runoff Forecast Model #3 for Snow Drought Forecast Uncertainty Project
// Eli Boardman copyright 2023

// The input data are vectors of length N
data {
  int<lower=0> N;
  vector[N] SWEP;
  vector[N] SeasP;
  vector[N] LastYrQ;
  vector[N] SeasQ;
  vector[N] Flights;
  vector[N] FutureQ; // response
}

// The parameters accepted by the model
parameters {
  real<lower=0> a;
  real<lower=0,upper=1> b;
  real<lower=0> sigma;
}

// The model to be estimated
model {
  // Model calculation
  vector[N] L = a + b*SeasP;
  vector[N] Q = SWEP - ((SWEP)./SeasP).*L;
  
   // Likelihood
   for (i in 1:N) {
     target += (1/Flights[i])*normal_lpdf((Q[i]-FutureQ[i])/SeasQ[i] | 0,sigma);
   }
}

