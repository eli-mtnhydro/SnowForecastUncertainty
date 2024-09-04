# Prepare data for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

library(raster)
library(rgdal)
library(ggplot2)
library(DiceKriging)

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

################################################################################
# Prepare precip timeseries from TUM station

PrecipTimeseries = read.csv("Data/DailyPrecipTimeseries_TUM.csv")[,-1]
PrecipTimeseries$Date = as.Date(paste0(PrecipTimeseries$Date),format="%Y%m%d")
PrecipTimeseries$Precip_m = PrecipTimeseries$Precip_in / (12 * 3.28084)

plot(PrecipTimeseries$Date, PrecipTimeseries$Precip_m, type="l")
# Potential outlier on 8/1/2018 does not matter for this analysis (outisde 10/1-7/31 window)

################################################################################
# Prepare snow pillow timeseries from 8 stations

SnowStationTimeseries = read.csv("Data/DailySnowPillowTimeseries_8stations.csv")[,-1]
SnowStationTimeseries$Date = as.Date(paste0(SnowStationTimeseries$Date),format="%Y%m%d")
SnowStationTimeseries$SWE_m = SnowStationTimeseries$SWE_in / (12 * 3.28084)
SnowStationTimeseries$Station = factor(SnowStationTimeseries$Station)

plot(SnowStationTimeseries$Date, SnowStationTimeseries$SWE_m, type="l")
points(WatershedData$Date, WatershedData$SWE_m, col="red", pch=19) # Must be generated first

ggplot(data=SnowStationTimeseries, aes(x=Date, y=SWE_m)) +
  geom_line(linewidth=1, aes(color=Station)) +
  facet_wrap("Station", ncol=2)

################################################################################
# Tuolumne River
################################################################################

# Basin outline and area from processing in QGIS
BasinOutline = readOGR("Data/BasinDelineation_Tuolumne/Upslope_HetchHetchy")
BasinArea_m = area(BasinOutline)

ASOmapNames = sub(".tif","",list.files("Data/ASO_SWE/Tuolumne","*.tif"))
ASOflightDates = as.Date(ASOmapNames,format="%Y%m%d")

WatershedData = data.frame(Date=ASOflightDates,
                           Year=as.numeric(format(ASOflightDates,"%Y")),
                           Month=as.numeric(format(ASOflightDates,"%m")),
                           Day=as.numeric(format(ASOflightDates,"%d")),
                           DayOfYear=as.numeric(format(ASOflightDates,"%j")),
                           FlightsBeforeJuly=NA,
                           SWE_m=NA,
                           FutureP_m=NA,
                           SeasP_m=NA,
                           LastYrRunoff_m=NA,
                           FutureRunoff_m=NA,
                           SeasRunoff_m=NA,
                           AvgStationSWE_m=NA,
                           StationSeasP_m=NA)

##########
# Find basin-average ASO SWE
for (flight in 1:length(WatershedData$Date)){
  SWEraster = raster(paste0("Data/ASO_SWE/Tuolumne/",ASOmapNames[flight],".tif"))
  
  SWEraster[SWEraster[,]<0] = NA
  
  WatershedData$SWE_m[flight] = extract(SWEraster,BasinOutline,fun=mean,na.rm=TRUE)
  
  plot(SWEraster, main=paste(ASOmapNames[flight],round(WatershedData$SWE_m[flight],2)))
  lines(BasinOutline)
}

##########
# Find post-flight accumulated station precip
for (flight in 1:length(WatershedData$Date)){
  StartDate = WatershedData$Date[flight]
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(PrecipTimeseries$Date >= StartDate &
                 PrecipTimeseries$Date <= EndDate)
  
  WatershedData$FutureP_m[flight] = sum(PrecipTimeseries$Precip_m[Ptrs],na.rm=TRUE)
}

SWEplusFutureP_m = WatershedData$SWE_m + WatershedData$FutureP_m

##########
# Calculate full-season precip (10/1-7/31)
for (flight in 1:length(WatershedData$Date)){
  StartDate = as.Date(paste0(WatershedData$Year[flight]-1,"-10-01"))
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(PrecipTimeseries$Date >= StartDate &
                 PrecipTimeseries$Date <= EndDate)
  
  WatershedData$SeasP_m[flight] = sum(PrecipTimeseries$Precip_m[Ptrs],na.rm=TRUE)
  
  # Regularize so that sum(P seasonal) is strictly >= (SWE + future P) at any time
  YrPtrs = which(WatershedData$Year==WatershedData$Year[flight])
  WatershedData$SeasP_m[flight] = max(WatershedData$SeasP_m[flight],
                                      max(SWEplusFutureP_m[YrPtrs]))
}

##########
# Calculate post-flight, seasonal, and last year's runoff volume
RunoffTimeseries = read.csv("Data/DailyRunoffTimeseries_TuolumneHetchHetchy.csv")[,-1]
RunoffTimeseries$Date = as.Date(RunoffTimeseries$Date,format="%m/%d/%Y")
RunoffTimeseries$Runoff_m = (RunoffTimeseries$Runoff_cfs * (60*60*24) / (3.28084^3)) / BasinArea_m

plot(RunoffTimeseries$Date, RunoffTimeseries$Runoff_m, type="l")

for (flight in 1:length(WatershedData$Date)){
  # Post-flight runoff
  StartDate = WatershedData$Date[flight]
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(RunoffTimeseries$Date >= StartDate &
                 RunoffTimeseries$Date <= EndDate)
  
  WatershedData$FutureRunoff_m[flight] = sum(RunoffTimeseries$Runoff_m[Ptrs],na.rm=TRUE)
  
  # Seasonal runoff
  StartDate = as.Date(paste0(WatershedData$Year[flight]-1,"-10-01"))
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(RunoffTimeseries$Date >= StartDate &
                 RunoffTimeseries$Date <= EndDate)
  
  WatershedData$SeasRunoff_m[flight] = sum(RunoffTimeseries$Runoff_m[Ptrs],na.rm=TRUE)
  
  # Last year's runoff
  StartDate = as.Date(paste0(WatershedData$Year[flight]-2,"-10-01"))
  EndDate = as.Date(paste0(WatershedData$Year[flight]-1,"-09-30"))
  
  Ptrs = which(RunoffTimeseries$Date >= StartDate &
                 RunoffTimeseries$Date <= EndDate)
  
  WatershedData$LastYrRunoff_m[flight] = sum(RunoffTimeseries$Runoff_m[Ptrs],na.rm=TRUE)
}

##########
# Eliminate data after June and count number of relevant flights
WatershedData = WatershedData[(WatershedData$Month<7),]
rownames(WatershedData) = 1:length(WatershedData$Date)
for (flight in 1:length(WatershedData$Date)){
  Ptrs = which(WatershedData$Year==WatershedData$Year[flight])
  WatershedData$FlightsBeforeJuly[flight] = length(Ptrs)
}

##########
# Find average snow pillow SWE
# for (flight in 1:length(WatershedData$Date)){
#   
#   Ptrs = which(SnowStationTimeseries$Date==WatershedData$Date[flight])
#   
#   WatershedData$AvgStationSWE_m[flight] = mean(SnowStationTimeseries$SWE_m[Ptrs], na.rm=TRUE)
# }

WatershedData$AvgStationSWE_m = NA
WatershedData$nMissingStations = NA
PreSolvedModels = list()
PreSolvedStationCombos = c()

for (flight in 1:length(WatershedData$Date)){
  
  SnowStationSubset = SnowStationTimeseries[SnowStationTimeseries$Date==WatershedData$Date[flight],]
  
  AvailableStations = as.character(SnowStationSubset$Station[is.finite(SnowStationSubset$SWE_m)])
  MissingStations = setdiff(as.character(unique(SnowStationTimeseries$Station)),AvailableStations)
  WatershedData$nMissingStations[flight] = length(MissingStations)
  print(MissingStations)
  
  # Impute each missing station based on whatever other stations are available
  
  for (Station in MissingStations){
    
    FeatureData = data.frame(Season=sin((2*pi/365)*(as.numeric(format(unique(SnowStationTimeseries$Date),"%j"))-81.75))+1)
    for (AddStation in AvailableStations){
      FeatureData = cbind(FeatureData,SnowStationTimeseries$SWE_m[SnowStationTimeseries$Station==AddStation])
    }
    names(FeatureData) = c("Season",AvailableStations)
    
    StationCombo = paste0(paste(sort(AvailableStations),collapse="l"),"_",Station)
    
    if (StationCombo %in% PreSolvedStationCombos){
      
      InterpModel = PreSolvedModels[[which(PreSolvedStationCombos==StationCombo)]]
      
    } else {
      
      # Fit and save new model
      
      OKptrs = which(is.finite(rowSums(FeatureData)) &
                       is.finite(SnowStationTimeseries$SWE_m[SnowStationTimeseries$Station==Station]))
      
      InterpModel = km(formula=~.,
                       design=FeatureData[OKptrs,],
                       response=SnowStationTimeseries$SWE_m[SnowStationTimeseries$Station==Station][OKptrs],
                       covtype="exp",
                       estim.method="MLE",
                       nugget.estim=TRUE,
                       control=list(trace=FALSE))
      
      PreSolvedStationCombos = c(PreSolvedStationCombos,StationCombo)
      PreSolvedModels = append(PreSolvedModels,InterpModel)
    }
    
    InterpPrediction = predict.km(InterpModel,FeatureData[min(which(SnowStationTimeseries$Date==WatershedData$Date[flight])),],
                                  type="UK",light.return=TRUE)$mean
    
    SnowStationSubset$SWE_m[SnowStationSubset$Station==Station] = max(0,InterpPrediction)
  }
  
  WatershedData$AvgStationSWE_m[flight] = mean(SnowStationSubset$SWE_m)
  
  print(flight/length(WatershedData$Date))
}

##########
# Calculate full-season precip (10/1-7/31) and regularize with station SWE

SWEplusFutureP_m = WatershedData$AvgStationSWE_m + WatershedData$FutureP_m

for (flight in 1:length(WatershedData$Date)){
  StartDate = as.Date(paste0(WatershedData$Year[flight]-1,"-10-01"))
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(PrecipTimeseries$Date >= StartDate &
                 PrecipTimeseries$Date <= EndDate)
  
  WatershedData$StationSeasP_m[flight] = sum(PrecipTimeseries$Precip_m[Ptrs],na.rm=TRUE)
  
  # Regularize so that sum(P seasonal) is strictly >= (SWE + future P) at any time
  # Using the station data instead of ASO SWE this time
  YrPtrs = which(WatershedData$Year==WatershedData$Year[flight])
  WatershedData$StationSeasP_m[flight] = max(WatershedData$StationSeasP_m[flight],
                                      max(SWEplusFutureP_m[YrPtrs]))
}

print(WatershedData)

write.csv(WatershedData, "Data/CompiledData_Tuolumne.csv")

rm(list=(ls())[ls()!="PrecipTimeseries" & ls()!="SnowStationTimeseries"])

################################################################################
# Merced River
################################################################################

# Basin outline and area from processing in QGIS
BasinOutline = readOGR("Data/BasinDelineation_Merced/Upslope_HappyIsles")
BasinArea_m = area(BasinOutline)

ASOmapNames = sub(".tif","",list.files("Data/ASO_SWE/Merced","*.tif"))
ASOflightDates = as.Date(ASOmapNames,format="%Y%m%d")

WatershedData = data.frame(Date=ASOflightDates,
                           Year=as.numeric(format(ASOflightDates,"%Y")),
                           Month=as.numeric(format(ASOflightDates,"%m")),
                           Day=as.numeric(format(ASOflightDates,"%d")),
                           DayOfYear=as.numeric(format(ASOflightDates,"%j")),
                           FlightsBeforeJuly=NA,
                           SWE_m=NA,
                           FutureP_m=NA,
                           SeasP_m=NA,
                           LastYrRunoff_m=NA,
                           FutureRunoff_m=NA,
                           SeasRunoff_m=NA,
                           AvgStationSWE_m=NA,
                           StationSeasP_m=NA)

##########
# Find basin-average ASO SWE
for (flight in 1:length(WatershedData$Date)){
  SWEraster = raster(paste0("Data/ASO_SWE/Merced/",ASOmapNames[flight],".tif"))
  
  SWEraster[SWEraster[,]<0] = NA
  
  WatershedData$SWE_m[flight] = extract(SWEraster,BasinOutline,fun=mean,na.rm=TRUE)
  
  plot(SWEraster, main=paste(ASOmapNames[flight],round(WatershedData$SWE_m[flight],2)))
  lines(BasinOutline)
}

##########
# Find post-flight accumulated station precip
for (flight in 1:length(WatershedData$Date)){
  StartDate = WatershedData$Date[flight]
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(PrecipTimeseries$Date >= StartDate &
                 PrecipTimeseries$Date <= EndDate)
  
  WatershedData$FutureP_m[flight] = sum(PrecipTimeseries$Precip_m[Ptrs],na.rm=TRUE)
}

SWEplusFutureP_m = WatershedData$SWE_m + WatershedData$FutureP_m

##########
# Calculate full-season precip (10/1-7/31)
for (flight in 1:length(WatershedData$Date)){
  StartDate = as.Date(paste0(WatershedData$Year[flight]-1,"-10-01"))
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(PrecipTimeseries$Date >= StartDate &
                 PrecipTimeseries$Date <= EndDate)
  
  WatershedData$SeasP_m[flight] = sum(PrecipTimeseries$Precip_m[Ptrs],na.rm=TRUE)
  
  # Regularize so that sum(P seasonal) is strictly >= (SWE + future P) at any time
  YrPtrs = which(WatershedData$Year==WatershedData$Year[flight])
  WatershedData$SeasP_m[flight] = max(WatershedData$SeasP_m[flight],
                                      max(SWEplusFutureP_m[YrPtrs]))
}

##########
# Calculate post-flight, seasonal, and last year's runoff volume
RunoffTimeseries = read.csv("Data/DailyRunoffTimeseries_MercedHappyIsles.csv")[,-1]
RunoffTimeseries$Date = as.Date(RunoffTimeseries$Date,format="%m/%d/%Y")
RunoffTimeseries$Runoff_m = (RunoffTimeseries$Runoff_cfs * (60*60*24) / (3.28084^3)) / BasinArea_m

plot(RunoffTimeseries$Date, RunoffTimeseries$Runoff_m, type="l")

for (flight in 1:length(WatershedData$Date)){
  # Post-flight runoff
  StartDate = WatershedData$Date[flight]
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(RunoffTimeseries$Date >= StartDate &
                 RunoffTimeseries$Date <= EndDate)
  
  WatershedData$FutureRunoff_m[flight] = sum(RunoffTimeseries$Runoff_m[Ptrs],na.rm=TRUE)
  
  # Seasonal runoff
  StartDate = as.Date(paste0(WatershedData$Year[flight]-1,"-10-01"))
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(RunoffTimeseries$Date >= StartDate &
                 RunoffTimeseries$Date <= EndDate)
  
  WatershedData$SeasRunoff_m[flight] = sum(RunoffTimeseries$Runoff_m[Ptrs],na.rm=TRUE)
  
  # Last year's runoff
  StartDate = as.Date(paste0(WatershedData$Year[flight]-2,"-10-01"))
  EndDate = as.Date(paste0(WatershedData$Year[flight]-1,"-09-30"))
  
  Ptrs = which(RunoffTimeseries$Date >= StartDate &
                 RunoffTimeseries$Date <= EndDate)
  
  WatershedData$LastYrRunoff_m[flight] = sum(RunoffTimeseries$Runoff_m[Ptrs],na.rm=TRUE)
}

##########
# Eliminate data after June and count number of relevant flights
WatershedData = WatershedData[(WatershedData$Month<7),]
rownames(WatershedData) = 1:length(WatershedData$Date)
for (flight in 1:length(WatershedData$Date)){
  Ptrs = which(WatershedData$Year==WatershedData$Year[flight])
  WatershedData$FlightsBeforeJuly[flight] = length(Ptrs)
}

##########
# Find average snow pillow SWE
# for (flight in 1:length(WatershedData$Date)){
#   
#   Ptrs = which(SnowStationTimeseries$Date==WatershedData$Date[flight])
#   
#   WatershedData$AvgStationSWE_m[flight] = mean(SnowStationTimeseries$SWE_m[Ptrs], na.rm=TRUE)
# }

WatershedData$AvgStationSWE_m = NA
WatershedData$nMissingStations = NA
PreSolvedModels = list()
PreSolvedStationCombos = c()

for (flight in 1:length(WatershedData$Date)){
  
  SnowStationSubset = SnowStationTimeseries[SnowStationTimeseries$Date==WatershedData$Date[flight],]
  
  AvailableStations = as.character(SnowStationSubset$Station[is.finite(SnowStationSubset$SWE_m)])
  MissingStations = setdiff(as.character(unique(SnowStationTimeseries$Station)),AvailableStations)
  WatershedData$nMissingStations[flight] = length(MissingStations)
  print(MissingStations)
  
  # Impute each missing station based on whatever other stations are available
  
  for (Station in MissingStations){
    
    FeatureData = data.frame(Season=sin((2*pi/365)*(as.numeric(format(unique(SnowStationTimeseries$Date),"%j"))-81.75))+1)
    for (AddStation in AvailableStations){
      FeatureData = cbind(FeatureData,SnowStationTimeseries$SWE_m[SnowStationTimeseries$Station==AddStation])
    }
    names(FeatureData) = c("Season",AvailableStations)
    
    StationCombo = paste0(paste(sort(AvailableStations),collapse="l"),"_",Station)
    
    if (StationCombo %in% PreSolvedStationCombos){
      
      InterpModel = PreSolvedModels[[which(PreSolvedStationCombos==StationCombo)]]
      
    } else {
      
      # Fit and save new model
      
      OKptrs = which(is.finite(rowSums(FeatureData)) &
                       is.finite(SnowStationTimeseries$SWE_m[SnowStationTimeseries$Station==Station]))
      
      InterpModel = km(formula=~.,
                       design=FeatureData[OKptrs,],
                       response=SnowStationTimeseries$SWE_m[SnowStationTimeseries$Station==Station][OKptrs],
                       covtype="exp",
                       estim.method="MLE",
                       nugget.estim=TRUE,
                       control=list(trace=FALSE))
      
      PreSolvedStationCombos = c(PreSolvedStationCombos,StationCombo)
      PreSolvedModels = append(PreSolvedModels,InterpModel)
    }
    
    InterpPrediction = predict.km(InterpModel,FeatureData[min(which(SnowStationTimeseries$Date==WatershedData$Date[flight])),],
                                  type="UK",light.return=TRUE)$mean
    
    SnowStationSubset$SWE_m[SnowStationSubset$Station==Station] = max(0,InterpPrediction)
  }
  
  WatershedData$AvgStationSWE_m[flight] = mean(SnowStationSubset$SWE_m)
  
  print(flight/length(WatershedData$Date))
}

##########
# Calculate full-season precip (10/1-7/31) and regularize with station SWE

SWEplusFutureP_m = WatershedData$AvgStationSWE_m + WatershedData$FutureP_m

for (flight in 1:length(WatershedData$Date)){
  StartDate = as.Date(paste0(WatershedData$Year[flight]-1,"-10-01"))
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(PrecipTimeseries$Date >= StartDate &
                 PrecipTimeseries$Date <= EndDate)
  
  WatershedData$StationSeasP_m[flight] = sum(PrecipTimeseries$Precip_m[Ptrs],na.rm=TRUE)
  
  # Regularize so that sum(P seasonal) is strictly >= (SWE + future P) at any time
  # Using the station data instead of ASO SWE this time
  YrPtrs = which(WatershedData$Year==WatershedData$Year[flight])
  WatershedData$StationSeasP_m[flight] = max(WatershedData$StationSeasP_m[flight],
                                             max(SWEplusFutureP_m[YrPtrs]))
}

print(WatershedData)

write.csv(WatershedData, "Data/CompiledData_Merced.csv")
