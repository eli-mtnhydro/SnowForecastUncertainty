# Run forecast simulations for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

################################################################################
# Forecast functions

nModels = 4

RunoffForecast1 = function(W,M,a){
  L = a
  Q = W - (W/M)*L
  Q = pmax(Q,0)
  return(Q)
}

RunoffForecast2 = function(W,M,b){
  L = b*M
  Q = W - (W/M)*L
  Q = pmax(Q,0)
  return(Q)
}

RunoffForecast3 = function(W,M,a,b){
  L = a + b*M
  Q = W - (W/M)*L
  Q = pmax(Q,0)
  return(Q)
}

RunoffForecast4 = function(W,M,N,a,b,c){
  L = a + b*M + c*N
  Q = W - (W/M)*L
  Q = pmax(Q,0)
  return(Q)
}

################################################################################
# Apply models to Tuolumne and Merced using the Bayesian parameter samples

# Read watershed data
TuolumneData = read.csv("Data/CompiledData_Tuolumne.csv")
MercedData = read.csv("Data/CompiledData_Merced.csv")

# Prepare possible "last year" runoff values
QlastYrTuolumne = unique(TuolumneData$LastYrRunoff_m)
QlastYrMerced = unique(MercedData$LastYrRunoff_m)

# Read Bayesian samples
TuolumnePars1 = read.csv("BayesianModels/TuolumneASO_Pars1.csv")
TuolumnePars2 = read.csv("BayesianModels/TuolumneASO_Pars2.csv")
TuolumnePars3 = read.csv("BayesianModels/TuolumneASO_Pars3.csv")
TuolumnePars4 = read.csv("BayesianModels/TuolumneASO_Pars4.csv")
MercedPars1 = read.csv("BayesianModels/MercedASO_Pars1.csv")
MercedPars2 = read.csv("BayesianModels/MercedASO_Pars2.csv")
MercedPars3 = read.csv("BayesianModels/MercedASO_Pars3.csv")
MercedPars4 = read.csv("BayesianModels/MercedASO_Pars4.csv")

TuolumnePars1Station = read.csv("BayesianModels/TuolumneStation_Pars1.csv")
TuolumnePars2Station = read.csv("BayesianModels/TuolumneStation_Pars2.csv")
TuolumnePars3Station = read.csv("BayesianModels/TuolumneStation_Pars3.csv")
TuolumnePars4Station = read.csv("BayesianModels/TuolumneStation_Pars4.csv")
MercedPars1Station = read.csv("BayesianModels/MercedStation_Pars1.csv")
MercedPars2Station = read.csv("BayesianModels/MercedStation_Pars2.csv")
MercedPars3Station = read.csv("BayesianModels/MercedStation_Pars3.csv")
MercedPars4Station = read.csv("BayesianModels/MercedStation_Pars4.csv")

ParameterSamples = length(TuolumnePars1$a)

################################################################################

########## Expectation value for forecast accuracy
# The median is our "forecast." The samples represent possible "truths."
# The implied "error" of any particular sample is its absolute deviation from the median (relative to the median).
# The implied "accuracy" of any sample is one minus its relative error.
# The expected accuracy of the distribution is the mean of the accuracy of all the samples.

ExpectedAccuracy = function(samples){
  
  med = median(samples)
  err = abs(samples-med) / med
  skill = mean(1 - err)
  
  skill = max(skill, 0)
  if (!is.finite(skill)){
    skill = 0
  }
  
  return(skill)
}

# Read precip stats
PrecipStats = read.csv("Data/April1st_PrecipStats_TUM.csv")[,-1]

# Setup
WeatherSamples = length(PrecipStats$WaterYear)
DroughtQuantiles = c(0.25,1) # The "real winter/spring" conditions are at the driest X% of the precip distribution
xSnow = c(0.1,0.3,0.5,0.7,0.9) # Fraction of winter precipitation remaining as SWE

########## Tuolumne data
# Actual winter, actual spring
BackcastModeTuolumne = array(0, dim=c(nModels,ParameterSamples))
BackcastAccuracyTuolumne = array(0, dim = c(length(xSnow),nModels,WeatherSamples,WeatherSamples))

# Actual winter, distribution of possible springs
ForecastModeTuolumne = array(0, dim=c(nModels,WeatherSamples,ParameterSamples))
ForecastAccuracyTuolumne = array(0, dim = c(length(xSnow),nModels,WeatherSamples,WeatherSamples))
StationModeTuolumne = array(0, dim=c(nModels,WeatherSamples,ParameterSamples))
StationAccuracyTuolumne = array(0, dim = c(length(xSnow),nModels,WeatherSamples,WeatherSamples))

# Position 1: forecast, Position 2: backcast, Position 3: station
AccuracyResultsTuolumne = array(0, dim=c(length(DroughtQuantiles),3,nModels,length(xSnow)))

########## Merced data
# Actual winter, actual spring
BackcastModeMerced = array(0, dim=c(nModels,ParameterSamples))
BackcastAccuracyMerced = array(0, dim = c(length(xSnow),nModels,WeatherSamples,WeatherSamples))

# Actual winter, distribution of possible springs
ForecastModeMerced = array(0, dim=c(nModels,WeatherSamples,ParameterSamples))
ForecastAccuracyMerced = array(0, dim = c(length(xSnow),nModels,WeatherSamples,WeatherSamples))
StationModeMerced = array(0, dim=c(nModels,WeatherSamples,ParameterSamples))
StationAccuracyMerced = array(0, dim = c(length(xSnow),nModels,WeatherSamples,WeatherSamples))

# Position 1: forecast, Position 2: backcast, Position 3: station
AccuracyResultsMerced = array(0, dim=c(length(DroughtQuantiles),3,nModels,length(xSnow)))

set.seed(42)

for (d in 1:length(DroughtQuantiles)){
  
  # Select the current "actual year" precip conditions
  DroughtQuantile = DroughtQuantiles[d]
  
  for (x in 1:length(xSnow)){
    
    # Generate ACTUAL winter weather samples, keeping ONLY those scenarios below the chosen drought threshold
    if (DroughtQuantile == 1){
      Pwinter = sample(PrecipStats$Oct1ToApr1_Precip_m)
    } else {
      Psamples = round(WeatherSamples * DroughtQuantile)
      Pwinter = sort(PrecipStats$Oct1ToApr1_Precip_m)[1:Psamples]
      
      # Re-randomize order after sort and increase number of samples
      Pwinter = sample(Pwinter, WeatherSamples, replace=TRUE)
    }
    
    for (winter in 1:WeatherSamples){
      
      # Generate ACTUAL spring weather samples, keeping ONLY those scenarios below the chosen drought threshold
      if (DroughtQuantile == 1){
        Pspring = sample(PrecipStats$Apr1ToJul31_Precip_m)
      } else {
        Psamples = round(WeatherSamples * DroughtQuantile)
        Pspring = sort(PrecipStats$Apr1ToJul31_Precip_m)[1:Psamples]
        
        # Re-randomize order after sort and increase number of samples
        Pspring = sample(Pspring, WeatherSamples, replace=TRUE)
      }
      
      for (spring in 1:WeatherSamples){
        
        # Combo of "real winter" and "real spring" defines a current year
        # Pick single value for last year's runoff
        Nvalmod4Tuolumne = sample(QlastYrTuolumne,1)
        Nvalmod4Merced = sample(QlastYrMerced,1)
        
        #################### Run forecast mode ####################
        # Generate FORECAST spring weather samples, MAY OR MAY NOT BE DROUGHT
        
        Pforecast = sample(PrecipStats$Apr1ToJul31_Precip_m)
        
        # Propagate spring Accuracy in ForecastMode
        for (forecastspring in 1:WeatherSamples){
          
          Mval = Pwinter[winter] + Pforecast[forecastspring]
          Wval = (xSnow[x] * Pwinter[winter]) + Pforecast[forecastspring]
          
          ########## Tuolumne ASO
          
          ForecastModeTuolumne[1,forecastspring,] = RunoffForecast1(Wval,
                                                                    Mval,
                                                                    TuolumnePars1$a) +
            rnorm(ParameterSamples,0,1) * TuolumnePars1$sigma * RunoffForecast1(Mval,
                                                                                Mval,
                                                                                TuolumnePars1$a)
          
          ForecastModeTuolumne[2,forecastspring,] = RunoffForecast2(Wval,
                                                                    Mval,
                                                                    TuolumnePars2$b) +
            rnorm(ParameterSamples,0,1) * TuolumnePars2$sigma * RunoffForecast2(Mval,
                                                                                Mval,
                                                                                TuolumnePars2$b)
          
          ForecastModeTuolumne[3,forecastspring,] = RunoffForecast3(Wval,
                                                                    Mval,
                                                                    TuolumnePars3$a,
                                                                    TuolumnePars3$b) +
            rnorm(ParameterSamples,0,1) * TuolumnePars3$sigma * RunoffForecast3(Mval,
                                                                                Mval,
                                                                                TuolumnePars3$a,
                                                                                TuolumnePars3$b)
          
          ForecastModeTuolumne[4,forecastspring,] = RunoffForecast4(Wval,
                                                                    Mval,
                                                                    Nvalmod4Tuolumne,
                                                                    TuolumnePars4$a,
                                                                    TuolumnePars4$b,
                                                                    TuolumnePars4$c) +
            rnorm(ParameterSamples,0,1) * TuolumnePars4$sigma * RunoffForecast4(Mval,
                                                                                Mval,
                                                                                Nvalmod4Tuolumne,
                                                                                TuolumnePars4$a,
                                                                                TuolumnePars4$b,
                                                                                TuolumnePars4$c)
          
          ########## Tuolumne Station
          
          StationModeTuolumne[1,forecastspring,] = RunoffForecast1(Wval,
                                                                   Mval,
                                                                   TuolumnePars1Station$a) +
            rnorm(ParameterSamples,0,1) * TuolumnePars1Station$sigma * RunoffForecast1(Mval,
                                                                                       Mval,
                                                                                       TuolumnePars1Station$a)
          
          StationModeTuolumne[2,forecastspring,] = RunoffForecast2(Wval,
                                                                   Mval,
                                                                   TuolumnePars2Station$b) +
            rnorm(ParameterSamples,0,1) * TuolumnePars2Station$sigma * RunoffForecast2(Mval,
                                                                                       Mval,
                                                                                       TuolumnePars2Station$b)
          
          StationModeTuolumne[3,forecastspring,] = RunoffForecast3(Wval,
                                                                   Mval,
                                                                   TuolumnePars3Station$a,
                                                                   TuolumnePars3Station$b) +
            rnorm(ParameterSamples,0,1) * TuolumnePars3Station$sigma * RunoffForecast3(Mval,
                                                                                       Mval,
                                                                                       TuolumnePars3Station$a,
                                                                                       TuolumnePars3Station$b)
          
          StationModeTuolumne[4,forecastspring,] = RunoffForecast4(Wval,
                                                                   Mval,
                                                                   Nvalmod4Tuolumne,
                                                                   TuolumnePars4Station$a,
                                                                   TuolumnePars4Station$b,
                                                                   TuolumnePars4Station$c) +
            rnorm(ParameterSamples,0,1) * TuolumnePars4Station$sigma * RunoffForecast4(Mval,
                                                                                       Mval,
                                                                                       Nvalmod4Tuolumne,
                                                                                       TuolumnePars4Station$a,
                                                                                       TuolumnePars4Station$b,
                                                                                       TuolumnePars4Station$c)
          
          ########## Merced ASO
          
          ForecastModeMerced[1,forecastspring,] = RunoffForecast1(Wval,
                                                                  Mval,
                                                                  MercedPars1$a) +
            rnorm(ParameterSamples,0,1) * MercedPars1$sigma * RunoffForecast1(Mval,
                                                                              Mval,
                                                                              MercedPars1$a)
          
          ForecastModeMerced[2,forecastspring,] = RunoffForecast2(Wval,
                                                                  Mval,
                                                                  MercedPars2$b) +
            rnorm(ParameterSamples,0,1) * MercedPars2$sigma * RunoffForecast2(Mval,
                                                                              Mval,
                                                                              MercedPars2$b)
          
          ForecastModeMerced[3,forecastspring,] = RunoffForecast3(Wval,
                                                                  Mval,
                                                                  MercedPars3$a,
                                                                  MercedPars3$b) +
            rnorm(ParameterSamples,0,1) * MercedPars3$sigma * RunoffForecast3(Mval,
                                                                              Mval,
                                                                              MercedPars3$a,
                                                                              MercedPars3$b)
          
          ForecastModeMerced[4,forecastspring,] = RunoffForecast4(Wval,
                                                                  Mval,
                                                                  Nvalmod4Merced,
                                                                  MercedPars4$a,
                                                                  MercedPars4$b,
                                                                  MercedPars4$c) +
            rnorm(ParameterSamples,0,1) * MercedPars4$sigma * RunoffForecast4(Mval,
                                                                              Mval,
                                                                              Nvalmod4Merced,
                                                                              MercedPars4$a,
                                                                              MercedPars4$b,
                                                                              MercedPars4$c)
          
          ########## Merced Station
          
          StationModeMerced[1,forecastspring,] = RunoffForecast1(Wval,
                                                                 Mval,
                                                                 MercedPars1Station$a) +
            rnorm(ParameterSamples,0,1) * MercedPars1Station$sigma * RunoffForecast1(Mval,
                                                                                     Mval,
                                                                                     MercedPars1Station$a)
          
          StationModeMerced[2,forecastspring,] = RunoffForecast2(Wval,
                                                                 Mval,
                                                                 MercedPars2Station$b) +
            rnorm(ParameterSamples,0,1) * MercedPars2Station$sigma * RunoffForecast2(Mval,
                                                                                     Mval,
                                                                                     MercedPars2Station$b)
          
          StationModeMerced[3,forecastspring,] = RunoffForecast3(Wval,
                                                                 Mval,
                                                                 MercedPars3Station$a,
                                                                 MercedPars3Station$b) +
            rnorm(ParameterSamples,0,1) * MercedPars3Station$sigma * RunoffForecast3(Mval,
                                                                                     Mval,
                                                                                     MercedPars3Station$a,
                                                                                     MercedPars3Station$b)
          
          StationModeMerced[4,forecastspring,] = RunoffForecast4(Wval,
                                                                 Mval,
                                                                 Nvalmod4Merced,
                                                                 MercedPars4Station$a,
                                                                 MercedPars4Station$b,
                                                                 MercedPars4Station$c) +
            rnorm(ParameterSamples,0,1) * MercedPars4Station$sigma * RunoffForecast4(Mval,
                                                                                     Mval,
                                                                                     Nvalmod4Merced,
                                                                                     MercedPars4Station$a,
                                                                                     MercedPars4Station$b,
                                                                                     MercedPars4Station$c)
          
        } # End of forecast spring loop
        
        #################### Run backcast mode ####################
        
        Mval = Pwinter[winter] + Pspring[spring]
        Wval = (xSnow[x] * Pwinter[winter]) + Pspring[spring]
        
        ########## Tuolumne ASO
        
        BackcastModeTuolumne[1,] = RunoffForecast1(Wval,
                                                   Mval,
                                                   TuolumnePars1$a) +
          rnorm(ParameterSamples,0,1) * TuolumnePars1$sigma * RunoffForecast1(Mval,
                                                                              Mval,
                                                                              TuolumnePars1$a)
        
        BackcastModeTuolumne[2,] = RunoffForecast2(Wval,
                                                   Mval,
                                                   TuolumnePars2$b) +
          rnorm(ParameterSamples,0,1) * TuolumnePars2$sigma * RunoffForecast2(Mval,
                                                                              Mval,
                                                                              TuolumnePars2$b)
        
        BackcastModeTuolumne[3,] = RunoffForecast3(Wval,
                                                   Mval,
                                                   TuolumnePars3$a,
                                                   TuolumnePars3$b) +
          rnorm(ParameterSamples,0,1) * TuolumnePars3$sigma * RunoffForecast3(Mval,
                                                                              Mval,
                                                                              TuolumnePars3$a,
                                                                              TuolumnePars3$b)
        
        BackcastModeTuolumne[4,] = RunoffForecast4(Wval,
                                                   Mval,
                                                   Nvalmod4Tuolumne,
                                                   TuolumnePars4$a,
                                                   TuolumnePars4$b,
                                                   TuolumnePars4$c) +
          rnorm(ParameterSamples,0,1) * TuolumnePars4$sigma * RunoffForecast4(Mval,
                                                                              Mval,
                                                                              Nvalmod4Tuolumne,
                                                                              TuolumnePars4$a,
                                                                              TuolumnePars4$b,
                                                                              TuolumnePars4$c)
        
        ########## Merced ASO
        
        BackcastModeMerced[1,] = RunoffForecast1(Wval,
                                                 Mval,
                                                 MercedPars1$a) +
          rnorm(ParameterSamples,0,1) * MercedPars1$sigma * RunoffForecast1(Mval,
                                                                            Mval,
                                                                            MercedPars1$a)
        
        BackcastModeMerced[2,] = RunoffForecast2(Wval,
                                                 Mval,
                                                 MercedPars2$b) +
          rnorm(ParameterSamples,0,1) * MercedPars2$sigma * RunoffForecast2(Mval,
                                                                            Mval,
                                                                            MercedPars2$b)
        
        BackcastModeMerced[3,] = RunoffForecast3(Wval,
                                                 Mval,
                                                 MercedPars3$a,
                                                 MercedPars3$b) +
          rnorm(ParameterSamples,0,1) * MercedPars3$sigma * RunoffForecast3(Mval,
                                                                            Mval,
                                                                            MercedPars3$a,
                                                                            MercedPars3$b)
        
        BackcastModeMerced[4,] = RunoffForecast4(Wval,
                                                 Mval,
                                                 Nvalmod4Merced,
                                                 MercedPars4$a,
                                                 MercedPars4$b,
                                                 MercedPars4$c) +
          rnorm(ParameterSamples,0,1) * MercedPars4$sigma * RunoffForecast4(Mval,
                                                                            Mval,
                                                                            Nvalmod4Merced,
                                                                            MercedPars4$a,
                                                                            MercedPars4$b,
                                                                            MercedPars4$c)
        
        ########################################################################
        # Prevent negative runoff
        # In extreme cases, rnorm(0,1) * SeasQ could be negative and larger magnitude than Qmod
        
        ForecastModeTuolumne = pmax(ForecastModeTuolumne,0)
        StationModeTuolumne = pmax(StationModeTuolumne,0)
        BackcastModeTuolumne = pmax(BackcastModeTuolumne,0)
        
        ForecastModeMerced = pmax(ForecastModeMerced,0)
        StationModeMerced = pmax(StationModeMerced,0)
        BackcastModeMerced = pmax(BackcastModeMerced,0)
        
        
        ########################################################################
        # Calculate the expected accuracy
        
        for (mod in 1:nModels){
          
          ForecastAccuracyTuolumne[x,mod,winter,spring] = ExpectedAccuracy(ForecastModeTuolumne[mod,,])
          StationAccuracyTuolumne[x,mod,winter,spring] = ExpectedAccuracy(StationModeTuolumne[mod,,])
          BackcastAccuracyTuolumne[x,mod,winter,spring] = ExpectedAccuracy(BackcastModeTuolumne[mod,])
          
          ForecastAccuracyMerced[x,mod,winter,spring] = ExpectedAccuracy(ForecastModeMerced[mod,,])
          StationAccuracyMerced[x,mod,winter,spring] = ExpectedAccuracy(StationModeMerced[mod,,])
          BackcastAccuracyMerced[x,mod,winter,spring] = ExpectedAccuracy(BackcastModeMerced[mod,])
        }
        
      } # End of actual spring loop
      print(paste0(round(100*winter/WeatherSamples),"% done with SWE frac. ",xSnow[x]," under drought quantile ",DroughtQuantile))
      
    } # End of winter loop
    
    # For each xSnow, calculate the average forecast and backcast expected accuracy over all winters and actual springs
    for (mod in 1:nModels){
      
      AccuracyResultsTuolumne[d,1,mod,x] = mean(ForecastAccuracyTuolumne[x,mod,,])
      AccuracyResultsTuolumne[d,2,mod,x] = mean(BackcastAccuracyTuolumne[x,mod,,])
      AccuracyResultsTuolumne[d,3,mod,x] = mean(StationAccuracyTuolumne[x,mod,,])
      
      AccuracyResultsMerced[d,1,mod,x] = mean(ForecastAccuracyMerced[x,mod,,])
      AccuracyResultsMerced[d,2,mod,x] = mean(BackcastAccuracyMerced[x,mod,,])
      AccuracyResultsMerced[d,3,mod,x] = mean(StationAccuracyMerced[x,mod,,])
    }
    
    print("*********************************************************************")
    print(paste0(100*x/length(xSnow),"% done with simulation under drought quantile ",DroughtQuantile))
    print("*********************************************************************")
  }
}

################################################################################
# Combine all these results into something usable

JoinedSkillValues = c()
IncrementalSkillValues = c()

for (d in 1:length(DroughtQuantiles)){
  
  SkillSetModel1 = data.frame(xSnow=xSnow,
                              TuolumneForecast=AccuracyResultsTuolumne[d,1,1,],
                              TuolumneBackcast=AccuracyResultsTuolumne[d,2,1,],
                              TuolumneStation=AccuracyResultsTuolumne[d,3,1,],
                              MercedForecast=AccuracyResultsMerced[d,1,1,],
                              MercedBackcast=AccuracyResultsMerced[d,2,1,],
                              MercedStation=AccuracyResultsMerced[d,3,1,])
  
  SkillSetModel2 = data.frame(xSnow=xSnow,
                              TuolumneForecast=AccuracyResultsTuolumne[d,1,2,],
                              TuolumneBackcast=AccuracyResultsTuolumne[d,2,2,],
                              TuolumneStation=AccuracyResultsTuolumne[d,3,2,],
                              MercedForecast=AccuracyResultsMerced[d,1,2,],
                              MercedBackcast=AccuracyResultsMerced[d,2,2,],
                              MercedStation=AccuracyResultsMerced[d,3,2,])
  
  SkillSetModel3 = data.frame(xSnow=xSnow,
                              TuolumneForecast=AccuracyResultsTuolumne[d,1,3,],
                              TuolumneBackcast=AccuracyResultsTuolumne[d,2,3,],
                              TuolumneStation=AccuracyResultsTuolumne[d,3,3,],
                              MercedForecast=AccuracyResultsMerced[d,1,3,],
                              MercedBackcast=AccuracyResultsMerced[d,2,3,],
                              MercedStation=AccuracyResultsMerced[d,3,3,])
  
  SkillSetModel4 = data.frame(xSnow=xSnow,
                              TuolumneForecast=AccuracyResultsTuolumne[d,1,4,],
                              TuolumneBackcast=AccuracyResultsTuolumne[d,2,4,],
                              TuolumneStation=AccuracyResultsTuolumne[d,3,4,],
                              MercedForecast=AccuracyResultsMerced[d,1,4,],
                              MercedBackcast=AccuracyResultsMerced[d,2,4,],
                              MercedStation=AccuracyResultsMerced[d,3,4,])
  
  JoinedSkillValues = c(JoinedSkillValues,
                        
                        SkillSetModel1$TuolumneStation,
                        SkillSetModel1$TuolumneForecast,
                        SkillSetModel1$TuolumneBackcast,
                        
                        SkillSetModel2$TuolumneStation,
                        SkillSetModel2$TuolumneForecast,
                        SkillSetModel2$TuolumneBackcast,
                        
                        SkillSetModel3$TuolumneStation,
                        SkillSetModel3$TuolumneForecast,
                        SkillSetModel3$TuolumneBackcast,
                        
                        SkillSetModel4$TuolumneStation,
                        SkillSetModel4$TuolumneForecast,
                        SkillSetModel4$TuolumneBackcast,
                        
                        SkillSetModel1$MercedStation,
                        SkillSetModel1$MercedForecast,
                        SkillSetModel1$MercedBackcast,
                        
                        SkillSetModel2$MercedStation,
                        SkillSetModel2$MercedForecast,
                        SkillSetModel2$MercedBackcast,
                        
                        SkillSetModel3$MercedStation,
                        SkillSetModel3$MercedForecast,
                        SkillSetModel3$MercedBackcast,
                        
                        SkillSetModel4$MercedStation,
                        SkillSetModel4$MercedForecast,
                        SkillSetModel4$MercedBackcast)
  
  IncrementalSkillTuolumne1 = c(SkillSetModel1$TuolumneStation,
                                SkillSetModel1$TuolumneForecast - SkillSetModel1$TuolumneStation,
                                SkillSetModel1$TuolumneBackcast - SkillSetModel1$TuolumneForecast)
  
  IncrementalSkillTuolumne2 = c(SkillSetModel2$TuolumneStation,
                                SkillSetModel2$TuolumneForecast - SkillSetModel2$TuolumneStation,
                                SkillSetModel2$TuolumneBackcast - SkillSetModel2$TuolumneForecast)
  
  IncrementalSkillTuolumne3 = c(SkillSetModel3$TuolumneStation,
                                SkillSetModel3$TuolumneForecast - SkillSetModel3$TuolumneStation,
                                SkillSetModel3$TuolumneBackcast - SkillSetModel3$TuolumneForecast)
  
  IncrementalSkillTuolumne4 = c(SkillSetModel4$TuolumneStation,
                                SkillSetModel4$TuolumneForecast - SkillSetModel4$TuolumneStation,
                                SkillSetModel4$TuolumneBackcast - SkillSetModel4$TuolumneForecast)
  
  IncrementalSkillMerced1 = c(SkillSetModel1$MercedStation,
                              SkillSetModel1$MercedForecast - SkillSetModel1$MercedStation,
                              SkillSetModel1$MercedBackcast - SkillSetModel1$MercedForecast)
  
  IncrementalSkillMerced2 = c(SkillSetModel2$MercedStation,
                              SkillSetModel2$MercedForecast - SkillSetModel2$MercedStation,
                              SkillSetModel2$MercedBackcast - SkillSetModel2$MercedForecast)
  
  IncrementalSkillMerced3 = c(SkillSetModel3$MercedStation,
                              SkillSetModel3$MercedForecast - SkillSetModel3$MercedStation,
                              SkillSetModel3$MercedBackcast - SkillSetModel3$MercedForecast)
  
  IncrementalSkillMerced4 = c(SkillSetModel4$MercedStation,
                              SkillSetModel4$MercedForecast - SkillSetModel4$MercedStation,
                              SkillSetModel4$MercedBackcast - SkillSetModel4$MercedForecast)
  
  IncrementalSkillValues = c(IncrementalSkillValues,
                             
                             IncrementalSkillTuolumne1,
                             IncrementalSkillTuolumne2,
                             IncrementalSkillTuolumne3,
                             IncrementalSkillTuolumne4,
                             
                             IncrementalSkillMerced1,
                             IncrementalSkillMerced2,
                             IncrementalSkillMerced3,
                             IncrementalSkillMerced4)
}

########## Create labels describing organization of skill data
# From fastest-changing to slowest-changing category:
# 5x snow fraction, 3x modes, 4x models, 2x basins, 2x drought

nSnowFrac = length(xSnow)
nModes = 3 # Station forecast, ASO forecast, ASO backcast
nBasins = 2 # Tuolumne and Merced
nDroughtQuant = length(DroughtQuantiles)

SnowFrac = rep(xSnow,
               nModes * nModels * nBasins * nDroughtQuant)

Mode = rep(c(rep("StationForecast",
                 nSnowFrac),
             rep("ASOForecast",
                 nSnowFrac),
             rep("ASOBackcast",
                 nSnowFrac)),
           nModels * nBasins * nDroughtQuant)

ModelNum = rep(c(rep(1,
                     nSnowFrac * nModes),
                 rep(2,
                     nSnowFrac * nModes),
                 rep(3,
                     nSnowFrac * nModes),
                 rep(4,
                     nSnowFrac * nModes)),
               nBasins * nDroughtQuant)

Watershed = rep(c(rep("Tuolumne",
                      nSnowFrac * nModes * nModels),
                  rep("Merced",
                      nSnowFrac * nModes * nModels)),
                nDroughtQuant)

Drought = sort(rep(DroughtQuantiles,
                   nSnowFrac * nModes * nModels * nBasins))

SkillSet = data.frame(SnowFrac,
                      Mode,
                      ModelNum,
                      Watershed,
                      Drought,
                      Skill=JoinedSkillValues,
                      IncrementalSkill=IncrementalSkillValues)

write.csv(SkillSet,"SnowDroughtForecastUncertainty_ModelSkillResults.csv")
