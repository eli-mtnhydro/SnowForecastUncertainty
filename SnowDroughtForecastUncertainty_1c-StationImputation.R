# Prepare data for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

library(raster)
library(rgdal)
library(ggplot2)

library(DiceKriging)

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

################################################################################
# Prepare snow pillow timeseries from 8 stations

SnowStationTimeseries = read.csv("Data/DailySnowPillowTimeseries_8stations.csv")[,-1]
SnowStationTimeseries$Date = as.Date(paste0(SnowStationTimeseries$Date),format="%Y%m%d")
SnowStationTimeseries$SWE_m = SnowStationTimeseries$SWE_in / (12 * 3.28084)
SnowStationTimeseries$Station = factor(SnowStationTimeseries$Station)

plot(SnowStationTimeseries$Date, SnowStationTimeseries$SWE_m, type="l")
points(WatershedData$Date, WatershedData$AvgStationSWE_m, col="red", pch=19) # Must be generated first

ggplot(data=SnowStationTimeseries, aes(x=Date, y=SWE_m)) +
  geom_line(linewidth=1, aes(color=Station)) +
  facet_wrap("Station", ncol=2)

##########
# Find average snow pillow SWE
for (flight in 1:length(WatershedData$Date)){
  
  Ptrs = which(SnowStationTimeseries$Date==WatershedData$Date[flight])
  
  WatershedData$AvgStationSWE_m[flight] = mean(SnowStationTimeseries$SWE_m[Ptrs], na.rm=TRUE)
}

WatershedData$AvgStationSWEimputed_m = NA
WatershedData$nMissingStations = NA

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

    OKptrs = which(is.finite(rowSums(FeatureData)) &
                     is.finite(SnowStationTimeseries$SWE_m[SnowStationTimeseries$Station==Station]))

    InterpModel = km(formula=~.,
                     design=FeatureData[OKptrs,],
                     response=SnowStationTimeseries$SWE_m[SnowStationTimeseries$Station==Station][OKptrs],
                     covtype="exp",
                     estim.method="MLE",
                     nugget.estim=TRUE,
                     control=list(trace=FALSE))

    InterpPrediction = predict.km(InterpModel,FeatureData[min(which(SnowStationTimeseries$Date==WatershedData$Date[flight])),],
                                  type="UK",light.return=TRUE)$mean

    SnowStationSubset$SWE_m[SnowStationSubset$Station==Station] = max(0,InterpPrediction)
  }

  WatershedData$AvgStationSWEimputed_m[flight] = mean(SnowStationSubset$SWE_m)

  print(flight/length(WatershedData$Date))
}
















