# Fit Bayesian regression models for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

library(rstan)

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

WatershedNames = c("Tuolumne","Merced")

################################################################################
# Bayesian sampling with ASO data
################################################################################

# Stan setup
ParamSamples = 1000
WarmupSamples = 5000
nChains = 2
ParameterSamples = ParamSamples * nChains

# For parallel MCMC sampling
options(mc.cores = parallel::detectCores())

for (WatershedName in WatershedNames){
  # Get data
  WatershedData = read.csv(paste0("Data/CompiledData_",WatershedName,".csv"))
  WatershedData$Date = as.Date(WatershedData$Date)
  head(WatershedData)
  plot(WatershedData$Date, WatershedData$SWE_m, type="l", main=WatershedName)
  
  ModelData = list(N=length(WatershedData$Year),
                   SWEP=(WatershedData$SWE_m + WatershedData$FutureP_m),
                   SeasP=WatershedData$SeasP_m,
                   LastYrQ=WatershedData$LastYrRunoff_m,
                   SeasQ=WatershedData$SeasRunoff_m,
                   Flights=WatershedData$FlightsBeforeJuly,
                   FutureQ=WatershedData$FutureRunoff_m)
  
  ##########
  # Bayesian sampling
  
  StanFit1 = stan(file="BayesianModels/Stan_RF1.stan",
                  data=ModelData,
                  chains=nChains,
                  iter=WarmupSamples + ParamSamples,
                  warmup=WarmupSamples,
                  control=list(adapt_delta=0.999,stepsize=0.01,max_treedepth=15))
  print(StanFit1)
  
  StanFit2 = stan(file="BayesianModels/Stan_RF2.stan",
                  data=ModelData,
                  chains=nChains,
                  iter=WarmupSamples + ParamSamples,
                  warmup=WarmupSamples,
                  control=list(adapt_delta=0.999,stepsize=0.01,max_treedepth=15))
  print(StanFit2)
  
  StanFit3 = stan(file="BayesianModels/Stan_RF3.stan",
                  data=ModelData,
                  chains=nChains,
                  iter=WarmupSamples + ParamSamples,
                  warmup=WarmupSamples,
                  control=list(adapt_delta=0.999,stepsize=0.01,max_treedepth=15))
  print(StanFit3)
  
  StanFit4 = stan(file="BayesianModels/Stan_RF4.stan",
                  data=ModelData,
                  chains=nChains,
                  iter=WarmupSamples + ParamSamples,
                  warmup=WarmupSamples,
                  control=list(adapt_delta=0.999,stepsize=0.01,max_treedepth=15))
  print(StanFit4)
  
  ##########
  # Extract parameter samples for each model
  
  Pars1 = rstan::extract(StanFit1, pars=c("a","sigma"), inc_warmup=FALSE)
  Pars2 = rstan::extract(StanFit2, pars=c("b","sigma"), inc_warmup=FALSE)
  Pars3 = rstan::extract(StanFit3, pars=c("a","b","sigma"), inc_warmup=FALSE)
  Pars4 = rstan::extract(StanFit4, pars=c("a","b","c","sigma"), inc_warmup=FALSE)
  
  write.csv(Pars1,paste0("BayesianModels/",WatershedName,"ASO_Pars1.csv"))
  write.csv(Pars2,paste0("BayesianModels/",WatershedName,"ASO_Pars2.csv"))
  write.csv(Pars3,paste0("BayesianModels/",WatershedName,"ASO_Pars3.csv"))
  write.csv(Pars4,paste0("BayesianModels/",WatershedName,"ASO_Pars4.csv"))
}

################################################################################
# Bayesian sampling with snow pillow station data
################################################################################

# Stan setup
ParamSamples = 1000
WarmupSamples = 5000
nChains = 2
ParameterSamples = ParamSamples * nChains

# For parallel MCMC sampling
options(mc.cores = parallel::detectCores())

for (WatershedName in WatershedNames){
  # Get data
  WatershedData = read.csv(paste0("Data/CompiledData_",WatershedName,".csv"))
  WatershedData$Date = as.Date(WatershedData$Date)
  head(WatershedData)
  plot(WatershedData$Date, WatershedData$SWE_m, type="l", main=WatershedName)
  
  # Different values for SWEP (station SWE) and SeasP (station regularization)
  ModelData = list(N=length(WatershedData$Year),
                   SWEP=(WatershedData$AvgStationSWE_m + WatershedData$FutureP_m),
                   SeasP=WatershedData$StationSeasP_m,
                   LastYrQ=WatershedData$LastYrRunoff_m,
                   SeasQ=WatershedData$SeasRunoff_m,
                   Flights=WatershedData$FlightsBeforeJuly,
                   FutureQ=WatershedData$FutureRunoff_m)
  
  ##########
  # Bayesian sampling
  
  StanFit1 = stan(file="BayesianModels/Stan_RF1.stan",
                  data=ModelData,
                  chains=nChains,
                  iter=WarmupSamples + ParamSamples,
                  warmup=WarmupSamples,
                  control=list(adapt_delta=0.999,stepsize=0.01,max_treedepth=15))
  print(StanFit1)
  
  StanFit2 = stan(file="BayesianModels/Stan_RF2.stan",
                  data=ModelData,
                  chains=nChains,
                  iter=WarmupSamples + ParamSamples,
                  warmup=WarmupSamples,
                  control=list(adapt_delta=0.999,stepsize=0.01,max_treedepth=15))
  print(StanFit2)
  
  StanFit3 = stan(file="BayesianModels/Stan_RF3.stan",
                  data=ModelData,
                  chains=nChains,
                  iter=WarmupSamples + ParamSamples,
                  warmup=WarmupSamples,
                  control=list(adapt_delta=0.999,stepsize=0.01,max_treedepth=15))
  print(StanFit3)
  
  StanFit4 = stan(file="BayesianModels/Stan_RF4.stan",
                  data=ModelData,
                  chains=nChains,
                  iter=WarmupSamples + ParamSamples,
                  warmup=WarmupSamples,
                  control=list(adapt_delta=0.999,stepsize=0.01,max_treedepth=15))
  print(StanFit4)
  
  ##########
  # Extract parameter samples for each model
  
  Pars1 = rstan::extract(StanFit1, pars=c("a","sigma"), inc_warmup=FALSE)
  Pars2 = rstan::extract(StanFit2, pars=c("b","sigma"), inc_warmup=FALSE)
  Pars3 = rstan::extract(StanFit3, pars=c("a","b","sigma"), inc_warmup=FALSE)
  Pars4 = rstan::extract(StanFit4, pars=c("a","b","c","sigma"), inc_warmup=FALSE)
  
  write.csv(Pars1,paste0("BayesianModels/",WatershedName,"Station_Pars1.csv"))
  write.csv(Pars2,paste0("BayesianModels/",WatershedName,"Station_Pars2.csv"))
  write.csv(Pars3,paste0("BayesianModels/",WatershedName,"Station_Pars3.csv"))
  write.csv(Pars4,paste0("BayesianModels/",WatershedName,"Station_Pars4.csv"))
}
