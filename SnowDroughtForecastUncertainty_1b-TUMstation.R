# Evaluate TUM precip against ASO SWE for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

library(raster)
library(rgdal)

################################################################################
# Get precip timeseries from TUM station

PrecipTimeseries = read.csv("Data/DailyPrecipTimeseries_TUM.csv")[,-1]
PrecipTimeseries$Date = as.Date(paste0(PrecipTimeseries$Date),format="%Y%m%d")
PrecipTimeseries$Precip_m = PrecipTimeseries$Precip_in / (12 * 3.28084)

plot(PrecipTimeseries$Date, PrecipTimeseries$Precip_m, type="l")
# Potential outlier on 8/1/2018 does not matter for this analysis (outisde 10/1-7/31 window)

################################################################################
# Read pre-processed ASO data

WatershedDataMerced = read.csv("Data/CompiledData_Merced.csv")[,-1]
WatershedDataMerced$Watershed = "Merced"

WatershedDataTuolumne = read.csv("Data/CompiledData_Tuolumne.csv")[,-1]
WatershedDataTuolumne$Watershed = "Tuolumne"

WatershedData = rbind(WatershedDataTuolumne, WatershedDataMerced)

################################################################################
# Compare Oct. 1 --> flight TUM precip to ASO SWE

# Keep only flights before April 1
Ptrs = which(WatershedData$Month < 4)
WatershedData = WatershedData[Ptrs,]

##########
# Calculate prior precip (10/1-flight)

WatershedData$PriorP_m = NA

for (flight in 1:length(WatershedData$Date)){
  StartDate = as.Date(paste0(WatershedData$Year[flight]-1,"-10-01"))
  EndDate = WatershedData$Date[flight]
  
  Ptrs = which(PrecipTimeseries$Date >= StartDate &
                 PrecipTimeseries$Date <= EndDate)
  
  WatershedData$PriorP_m[flight] = sum(PrecipTimeseries$Precip_m[Ptrs],na.rm=TRUE)
}

print(WatershedData)

plot(WatershedData$PriorP_m, WatershedData$SWE_m, xlim=c(0,1.5), ylim=c(0,1.5))
lines(c(0,2),c(0,2))

################################################################################
# Metrics

# Correlation
cor(WatershedData$PriorP_m, WatershedData$SWE_m)

# R^2
cor(WatershedData$PriorP_m, WatershedData$SWE_m)^2

# RMSE
sqrt(mean((WatershedData$PriorP_m - WatershedData$SWE_m)^2))

# Mean prior P
mean(WatershedData$PriorP_m)

# Mean percent error
mean(abs(WatershedData$PriorP_m - WatershedData$SWE_m)/WatershedData$PriorP_m)

################################################################################
# Mean and median elevation in Tuolumne and Merced watersheds

BasinOutlineTuolumne = readOGR("Data/BasinDelineation_Tuolumne/Upslope_HetchHetchy")
BasinOutlineMerced = readOGR("Data/BasinDelineation_Merced/Upslope_HappyIsles")

DEM = raster("QGISmap/TBMB_DEM.tif")
plot(DEM)
lines(BasinOutlineTuolumne)
lines(BasinOutlineMerced)

MeanElevationTuolumne = extract(DEM,BasinOutlineTuolumne,fun=mean,na.rm=TRUE)
MeanElevationMerced = extract(DEM,BasinOutlineMerced,fun=mean,na.rm=TRUE)

MedianElevationTuolumne = extract(DEM,BasinOutlineTuolumne,fun=median,na.rm=TRUE)
MedianElevationMerced = extract(DEM,BasinOutlineMerced,fun=median,na.rm=TRUE)

# TUM station is at 2635 m

# Tuolumne mean, median: 2720 m, 2810 m
# Merced mean, median: 2752 m, 2790 m















