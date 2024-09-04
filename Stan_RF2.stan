// Runoff Forecast Model #2 for Snow Drought Forecast Uncertainty Project
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
  real<lower=0,upper=1> b;
  real<lower=0> sigma;
}

// The model to be estimated
model {
  // Model calculation
  vector[N] Q = SWEP - (SWEP)*b;
  
   // Likelihood
   for (i in 1:N) {
     target += (1/Flights[i])*normal_lpdf((Q[i]-FutureQ[i])/SeasQ[i] | 0,sigma);
   }
}

