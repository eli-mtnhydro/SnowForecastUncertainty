# Evaluate TUM precip against ASO SWE for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

library(terra)
library(sf)
library(ncdf4)

################################################################################
# Get precip timeseries from TUM station

PrecipTimeseries = read.csv("Data/DailyPrecipTimeseries_TUM.csv")[,-1]
PrecipTimeseries$Date = as.Date(paste0(PrecipTimeseries$Date),format="%Y%m%d")
PrecipTimeseries$Precip_m = PrecipTimeseries$Precip_in / (12 * 3.28084)
PrecipTimeseries$Precip_m[which(is.na(PrecipTimeseries$Precip_m))] = 0

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
# Get daily watershed-average gridMET precipitation for Tuolumne and Merced

FirstYr = 2012
LastYr = 2022

PrecipData = data.frame(Watershed="Merced",
                        Date=seq(as.Date(paste0(FirstYr,"/1/1"),format="%Y/%m/%d"),
                                 as.Date(paste0(LastYr,"/9/30"),format="%Y/%m/%d"),
                                 by="days"),
                        GRIDMETprecip_m=NA)

EndDate = "2022-10-01" # yyyy-mm-dd

WatershedMask = rast("MercedMask.tif")
WatershedMask = project(WatershedMask,"EPSG:4326",
                        res=0.1/24,"near")
WatershedPtrs = which(WatershedMask[,]==1)

DHSVMresolution = unique(res(WatershedMask))

# Set up area to create meteorology data
StationAreaBuffer = 1 # Number of cells to buffer GridMet stations
DomainTemplate = buffer(as.polygons(ext(WatershedMask),
                                    crs=crs(WatershedMask)),
                        width=4000*StationAreaBuffer)
DomainTemplate = as.polygons(ext(DomainTemplate),
                             crs=crs(DomainTemplate))

plot(DomainTemplate)
plot(as.polygons(ext(WatershedMask),crs=crs(WatershedMask)), add=TRUE)

DomainTemplate = project(DomainTemplate, "EPSG:4326")

# Read data from gridMet downloads
GridMetDir = "E:/DHSVM_MountainHydrology/1_Setup/MeteorologyData/gridMET/"

GridMetResolution = 1/24 # degree

for (yr in FirstYr:LastYr){
  
  print(paste0("Opening NetCDF data for ",yr))
  
  nc_data_pr = nc_open(paste0(GridMetDir,"pr_",yr,".nc"))
  
  # Convert weather variables into array data
  pr.array = ncvar_get(nc_data_pr,"precipitation_amount")
  
  # Set up dimensions and create the new NetCDF if necessary
  if (yr == FirstYr){
    # Generate vectors of all lat/lon and days in full gridMet domain
    lons = ncvar_get(nc_data_pr,"lon")
    lats = ncvar_get(nc_data_pr,"lat")
    days = ncvar_get(nc_data_pr,"day")
    
    # Change from cell centers to edges of grid
    FullExtent = ext(c(min(lons) - GridMetResolution / 2,
                       max(lons) + GridMetResolution / 2,
                       min(lats) - GridMetResolution / 2,
                       max(lats) + GridMetResolution / 2))
    
    # Generate sample single-day array to determine new lat/lon dimensions
    pr.slice = pr.array[,,1]
    pr.rast = rast(t(pr.slice), crs="EPSG:4326", extent=FullExtent)
    #plot(pr.rast)
    RastTemplate = crop(pr.rast,DomainTemplate)
    nRowTemp = nrow(RastTemplate)
    LocalExtent = ext(RastTemplate)
    #plot(RastTemplate)
    
    # Keep track of how many days of data have been written to the file
    tStart = 1
  }
  
  # Load corresponding weather data into the file
  if (yr==LastYr){
    tLength = as.numeric(difftime(as.Date(EndDate),
                                  as.Date(paste0(LastYr,"-1-1")))) + 1
  } else {
    tLength = length(ncvar_get(nc_data_pr,"day"))
  }
  TimeIndices = tStart:(tLength+tStart-1)
  for (i in 1:tLength){
    
    # Rasterize and clip weather data for each day
    
    pr.rast = project(rast(t(pr.array[,,i]), crs="EPSG:4326", extent=FullExtent),WatershedMask)
    
    pr.avg = mean(unlist(pr.rast[WatershedPtrs]))
    PrecipData[TimeIndices[i],"GRIDMETprecip_m"] = pr.avg
    
    print(paste0(round(100*i/tLength,1),"% done loading data for year ",yr))
  }
  tStart = tLength + tStart
  
  nc_close(nc_data_pr)
}

write.csv(PrecipData,"Data/Merced_gridMET_DailyPrecip.csv")

################################################################################
# Compare daily precip

MercedDailyGRIDMET = read.csv("Data/Merced_gridMET_DailyPrecip.csv")[,-1]
TuolumneDailyGRIDMET = read.csv("Data/Tuolumne_gridMET_DailyPrecip.csv")[,-1]

MercedDailyGRIDMET$GRIDMETprecip_m = MercedDailyGRIDMET$GRIDMETprecip_m / 1000
TuolumneDailyGRIDMET$GRIDMETprecip_m = TuolumneDailyGRIDMET$GRIDMETprecip_m / 1000

plot(MercedDailyGRIDMET$GRIDMETprecip_m,TuolumneDailyGRIDMET$GRIDMETprecip_m)

MergedDailyPrecipData = data.frame(Date=seq(as.Date("2012/10/1",format="%Y/%m/%d"),
                                            as.Date("2022/9/30",format="%Y/%m/%d"),
                                            by="days"),
                                   TuolumneGRIDMET_m=NA,
                                   MercedGRIDMET_m=NA,
                                   TUM_m=NA)

MercedDailyGRIDMET$Date = as.Date(MercedDailyGRIDMET$Date,format="%Y-%m-%d")
TuolumneDailyGRIDMET$Date = as.Date(TuolumneDailyGRIDMET$Date,format="%Y-%m-%d")

MergedDailyPrecipData$TuolumneGRIDMET_m = TuolumneDailyGRIDMET$GRIDMETprecip_m[match(MergedDailyPrecipData$Date,
                                                                                     TuolumneDailyGRIDMET$Date)]
MergedDailyPrecipData$MercedGRIDMET_m = MercedDailyGRIDMET$GRIDMETprecip_m[match(MergedDailyPrecipData$Date,
                                                                                   MercedDailyGRIDMET$Date)]
MergedDailyPrecipData$TUM_m = PrecipTimeseries$Precip_m[match(MergedDailyPrecipData$Date,
                                                              PrecipTimeseries$Date)]

# Only compare where precip is calculated, October - July
MergedDailyPrecipData$Month = as.numeric(format(MergedDailyPrecipData$Date,"%m"))
MergedDailyPrecipData = MergedDailyPrecipData[which(MergedDailyPrecipData$Month %in% c(1:7,10:12)),]

plot(MergedDailyPrecipData$Date,MergedDailyPrecipData$TUM_m,type="l",
     main="TUM Station",ylim=c(0,0.35))
plot(MergedDailyPrecipData$Date,MergedDailyPrecipData$TuolumneGRIDMET_m,type="l",
     main="Tuolumne gridMET",ylim=c(0,0.35))
plot(MergedDailyPrecipData$Date,MergedDailyPrecipData$MercedGRIDMET_m,type="l",
     main="Merced gridMET",ylim=c(0,0.35))

cor(MergedDailyPrecipData$TuolumneGRIDMET_m,MergedDailyPrecipData$TUM_m)
cor(MergedDailyPrecipData$MercedGRIDMET_m,MergedDailyPrecipData$TUM_m)

################################################################################
# Calculate future precip from each ASO flight date through July 31

WatershedData$FutureP_TuolumneGRIDMET_m = NA
WatershedData$FutureP_MercedGRIDMET_m = NA

for (flight in 1:length(WatershedData$Date)){
  StartDate = WatershedData$Date[flight]
  EndDate = as.Date(paste0(WatershedData$Year[flight],"-07-31"))
  
  Ptrs = which(MergedDailyPrecipData$Date >= StartDate &
                 MergedDailyPrecipData$Date <= EndDate)
  
  WatershedData$FutureP_TuolumneGRIDMET_m[flight] = sum(MergedDailyPrecipData$TuolumneGRIDMET_m[Ptrs])
  WatershedData$FutureP_MercedGRIDMET_m[flight] = sum(MergedDailyPrecipData$MercedGRIDMET_m[Ptrs])
}

plot(WatershedData$FutureP_m, WatershedData$FutureP_TuolumneGRIDMET_m, xlim=c(0,1.5), ylim=c(0,1.5))
lines(c(0,2),c(0,2))

plot(WatershedData$FutureP_m, WatershedData$FutureP_MercedGRIDMET_m, xlim=c(0,1.5), ylim=c(0,1.5))
lines(c(0,2),c(0,2))

cor(WatershedData$FutureP_m, WatershedData$FutureP_TuolumneGRIDMET_m)
cor(WatershedData$FutureP_m, WatershedData$FutureP_MercedGRIDMET_m)

sqrt(mean((WatershedData$FutureP_m - WatershedData$FutureP_TuolumneGRIDMET_m)^2))
sqrt(mean((WatershedData$FutureP_m - WatershedData$FutureP_MercedGRIDMET_m)^2))

mean(WatershedData$FutureP_m)









