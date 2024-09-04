# Produce numbers for paper for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

SkillSet = read.csv("SnowDroughtForecastUncertainty_ModelSkillResults.csv")[,-1]
head(SkillSet)

DroughtQuantiles = unique(SkillSet$Drought)
xSnow = unique(SkillSet$SnowFrac)

AvgSkill = read.csv("SnowDroughtForecastUncertainty_ModelSkillResults_AvgSkill.csv")[,-1]
DeltaSkill = read.csv("SnowDroughtForecastUncertainty_ModelSkillResults_DeltaSkill.csv")[,-1]

# Read watershed data
TuolumneData = read.csv("Data/CompiledData_Tuolumne.csv")
MercedData = read.csv("Data/CompiledData_Merced.csv")

################################################################################

##########
# Fractional contribution to total uncertainty at different snow frac levels
for (xfrac in c(0.1,0.3,0.5,0.7,0.9)){
  
  HydroUncertainty = 1 - AvgSkill[(AvgSkill[,"Drought"]=="AllConditions" &
                                     AvgSkill[,"SnowFrac"]==xfrac &
                                     AvgSkill[,"Mode"]=="ASOBackcast"),]$Skill
  
  PrecipUncertainty = 1 - HydroUncertainty - AvgSkill[(AvgSkill[,"Drought"]=="AllConditions" &
                                                         AvgSkill[,"SnowFrac"]==xfrac &
                                                         AvgSkill[,"Mode"]=="ASOForecast"),]$Skill
  
  SnowUncertainty = 1 - HydroUncertainty - PrecipUncertainty - AvgSkill[(AvgSkill[,"Drought"]=="AllConditions" &
                                                                           AvgSkill[,"SnowFrac"]==xfrac &
                                                                           AvgSkill[,"Mode"]=="StationForecast"),]$Skill
  
  HydroRel = mean(HydroUncertainty / (HydroUncertainty + PrecipUncertainty + SnowUncertainty))
  PrecipRel = mean(PrecipUncertainty / (HydroUncertainty + PrecipUncertainty + SnowUncertainty))
  SnowRel = mean(SnowUncertainty / (HydroUncertainty + PrecipUncertainty + SnowUncertainty))
  
  print(paste0("Snow fraction: ",xfrac))
  print(paste0("Hydro uncertainty fraction of total uncertainty: ",100*round(HydroRel,2),"%"))
  print(paste0("Precip uncertainty fraction of total uncertainty: ",100*round(PrecipRel,2),"%"))
  print(paste0("Snow uncertainty fraction of total uncertainty: ",100*round(SnowRel,2),"%"))
  print("*")
}

# Historical conditions for SWE frac. in March-April from script #5 (precip. stats)
# Mean: 0.66 Tuolumne, 0.49 Merced
# Median: 0.67 Tuolumne, 0.41 Merced
# Roughly 50% historical SWE frac.
# --> hydro 45% of total uncertainty
# --> future precip 14% of total uncertainty
# --> snow pillows 41% of total uncertainty

##########
# Fractional decrease in TOTAL forecast skill during warm snow drought (all precip. conditions, 0.5 --> 0.1 SWEfrac)
Skill50 = mean(AvgSkill[(AvgSkill$Mode=="StationForecast" &
                           AvgSkill$SnowFrac==0.5 &
                           AvgSkill$Drought=="AllConditions"),]$Skill)

Skill10 = mean(AvgSkill[(AvgSkill$Mode=="StationForecast" &
                           AvgSkill$SnowFrac==0.1 &
                           AvgSkill$Drought=="AllConditions"),]$Skill)

PctChangeStation = (Skill10 - Skill50) / Skill50
print(paste0("Total station-forecast skill percent change: ",100*round(PctChangeStation,2),"%"))

Skill50 = mean(AvgSkill[(AvgSkill$Mode=="ASOForecast" &
                           AvgSkill$SnowFrac==0.5 &
                           AvgSkill$Drought=="AllConditions"),]$Skill)

Skill10 = mean(AvgSkill[(AvgSkill$Mode=="ASOForecast" &
                           AvgSkill$SnowFrac==0.1 &
                           AvgSkill$Drought=="AllConditions"),]$Skill)

PctChangeASO = (Skill10 - Skill50) / Skill50
print(paste0("Total ASO-forecast skill percent change: ",100*round(PctChangeASO,2),"%"))

PctChangeStation / PctChangeASO
PctChangeASO / PctChangeStation

# "X% less sensitive"
1 - PctChangeASO / PctChangeStation

##########
# Fractional increase in hydrological model uncertainty during warm snow drought (all precip. conditions, 0.9 --> 0.1 SWEfrac)
HydroUnc50 = mean(1 - AvgSkill[(AvgSkill$Mode=="ASOBackcast" &
                                  AvgSkill$SnowFrac==0.9 &
                                  AvgSkill$Drought=="AllConditions" &
                                  AvgSkill$Watershed=="Merced"),]$Skill)

HydroUnc10 = mean(1 - AvgSkill[(AvgSkill$Mode=="ASOBackcast" &
                                  AvgSkill$SnowFrac==0.1 &
                                  AvgSkill$Drought=="AllConditions" &
                                  AvgSkill$Watershed=="Merced"),]$Skill)

HydroPctChange = (HydroUnc10 - HydroUnc50) / HydroUnc50
print(paste0("Hydrological model uncertainty percent increase Merced: ",100*round(HydroPctChange,2),"%"))

HydroUnc50 = mean(1 - AvgSkill[(AvgSkill$Mode=="ASOBackcast" &
                                  AvgSkill$SnowFrac==0.9 &
                                  AvgSkill$Drought=="AllConditions" &
                                  AvgSkill$Watershed=="Tuolumne"),]$Skill)

HydroUnc10 = mean(1 - AvgSkill[(AvgSkill$Mode=="ASOBackcast" &
                                  AvgSkill$SnowFrac==0.1 &
                                  AvgSkill$Drought=="AllConditions" &
                                  AvgSkill$Watershed=="Tuolumne"),]$Skill)

HydroPctChange = (HydroUnc10 - HydroUnc50) / HydroUnc50
print(paste0("Hydrological model uncertainty percent increase Tuolumne: ",100*round(HydroPctChange,2),"%"))

##########
# Differences between the models

for (xfrac in c(0.1,0.3,0.5,0.7,0.9)){
  
  SkillMaxTuolumne = max(SkillSet[SkillSet$Mode=="ASOBackcast" &
                                    SkillSet$Watershed=="Tuolumne" &
                                    SkillSet$Drought==1 &
                                    SkillSet$SnowFrac==xfrac,]$Skill)
  SkillMinTuolumne = min(SkillSet[SkillSet$Mode=="ASOBackcast" &
                                    SkillSet$Watershed=="Tuolumne" &
                                    SkillSet$Drought==1 &
                                    SkillSet$SnowFrac==xfrac,]$Skill)
  
  SkillMaxMerced = max(SkillSet[SkillSet$Mode=="ASOBackcast" &
                                  SkillSet$Watershed=="Merced" &
                                  SkillSet$Drought==1 &
                                  SkillSet$SnowFrac==xfrac,]$Skill)
  SkillMinMerced = min(SkillSet[SkillSet$Mode=="ASOBackcast" &
                                  SkillSet$Watershed=="Merced" &
                                  SkillSet$Drought==1 &
                                  SkillSet$SnowFrac==xfrac,]$Skill)
  
  print(paste0("Snow fraction: ",xfrac))
  print(paste0("Tuolumne model range: ",100*round(SkillMaxTuolumne-SkillMinTuolumne,2),"%"))
  print(paste0("Merced model range: ",100*round(SkillMaxMerced-SkillMinMerced,2),"%"))
  print("*")
}

##########
# Tuolumne vs. Merced comparison

for (xfrac in c(0.1,0.3,0.5,0.7,0.9)){
  
  SkillTuolumne = mean(SkillSet[SkillSet$Mode=="ASOBackcast" &
                                  SkillSet$Watershed=="Tuolumne" &
                                  SkillSet$Drought==1 &
                                  SkillSet$SnowFrac==xfrac,]$Skill)
  
  SkillMerced = mean(SkillSet[SkillSet$Mode=="ASOBackcast" &
                                SkillSet$Watershed=="Merced" &
                                SkillSet$Drought==1 &
                                SkillSet$SnowFrac==xfrac,]$Skill)
  
  print(paste0("Snow fraction: ",xfrac))
  print(paste0("Tuolumne - Merced mean skill: ",100*round(SkillTuolumne-SkillMerced,2),"%"))
  print("*")
}

##########
# Fractional increase in UNCERTAINTY sources during dry snow drought

for (xfrac in c(0.1,0.3,0.5,0.7,0.9)){
  
  SnowpackUncPctIncreas = (DeltaSkill[(DeltaSkill$Mode=="StationForecast" &
                                         DeltaSkill$SnowFrac==xfrac),]$UncertaintyContribIncreasePct)
  
  PrecipUncPctIncreas = (DeltaSkill[(DeltaSkill$Mode=="ASOForecast" &
                                       DeltaSkill$SnowFrac==xfrac),]$UncertaintyContribIncreasePct)
  
  ModelUncPctIncreas = (DeltaSkill[(DeltaSkill$Mode=="ASOBackcast" &
                                      DeltaSkill$SnowFrac==xfrac),]$UncertaintyContribIncreasePct)
  
  
  print(paste0("Snow fraction: ",xfrac))
  print(paste0("Snow uncertainty relative increase: ",100*round(SnowpackUncPctIncreas,2),"%"))
  print(paste0("Precip uncertainty relative increase: ",100*round(PrecipUncPctIncreas,2),"%"))
  print(paste0("Hydro uncertainty relative increase: ",100*round(ModelUncPctIncreas,2),"%"))
  print("*")
}

# Prints Tuolumne, then Merced

##########
# Minimum benefit from ASO during warm AND dry snow drought

HydroUncertainty = 1 - (AvgSkill[(AvgSkill[,"Drought"]=="Drought" &
                                    AvgSkill[,"SnowFrac"]==0.1 &
                                    AvgSkill[,"Mode"]=="ASOBackcastDrought"),]$Skill)

PrecipUncertainty = 1 - HydroUncertainty - (AvgSkill[(AvgSkill[,"Drought"]=="Drought" &
                                                        AvgSkill[,"SnowFrac"]==0.1 &
                                                        AvgSkill[,"Mode"]=="ASOForecastDrought"),]$Skill)

SnowUncertainty = 1 - HydroUncertainty - PrecipUncertainty - (AvgSkill[(AvgSkill[,"Drought"]=="Drought" &
                                                                          AvgSkill[,"SnowFrac"]==0.1 &
                                                                          AvgSkill[,"Mode"]=="StationForecastDrought"),]$Skill)

HydroRel = HydroUncertainty / (HydroUncertainty + PrecipUncertainty + SnowUncertainty)
PrecipRel = PrecipUncertainty / (HydroUncertainty + PrecipUncertainty + SnowUncertainty)
SnowRel = SnowUncertainty / (HydroUncertainty + PrecipUncertainty + SnowUncertainty)

print(paste0("Snow uncertainty fraction of total uncertainty in extreme warm + dry snow drought: ",100*round(SnowRel,2),"%"))

# Prints Tuolumne, then Merced

##########
# All possible ASO benefits under the full range of precip scenarios

SnowUnc = AvgSkill[AvgSkill$Mode=="StationForecast","UncertaintyContrib"]

TotalUnc = AvgSkill[AvgSkill$Mode=="StationForecast","UncertaintyContrib"] +
  AvgSkill[AvgSkill$Mode=="ASOForecast","UncertaintyContrib"] +
  AvgSkill[AvgSkill$Mode=="ASOBackcast","UncertaintyContrib"]

print(paste0("Snow uncertainty fraction of total uncertainty for all watersheds x SWEfrac: ",100*round(SnowUnc/TotalUnc,2),"%"))

MeanImprovement = mean(SnowUnc/TotalUnc)

print(paste0("AVERAGE snow uncertainty fraction of total uncertainty for all watersheds x SWEfrac: ",100*round(MeanImprovement,2),"%"))

################################################################################

# Watershed physical characteristics

library(terra)
library(sf)

DEM = rast("QGISmap/TBMB_DEM.tif")
DEM = project(DEM,"EPSG:32611",res=30,origin=c(0,0))

WatershedBoundary = vect("QGISmap/MercedWatershed.kml")
WatershedBoundary = project(WatershedBoundary,DEM)

BasinDEM = crop(DEM,ext(WatershedBoundary))

plot(BasinDEM,reset=FALSE)
plot(WatershedBoundary,add=TRUE)

BasinDEM = mask(BasinDEM,WatershedBoundary)

plot(BasinDEM)

BasinMask = project(BasinDEM,"EPSG:4326")
BasinMask[which(is.finite(BasinMask[,]))] = 1
BasinMask[which(!is.finite(BasinMask[,]))] = 0
writeRaster(BasinMask,"MercedMask.tif",overwrite=TRUE)
st_write(st_as_sf(as.polygons(BasinMask)[2]), "MercedMask/MercedMask.shp", append=FALSE)

Area = length(which(is.finite(BasinDEM[,]))) * 30^2 / (1000^2)
Elevation = mean(BasinDEM[,],na.rm=TRUE)

print(paste0("Area = ",round(Area)," km^2"))
print(paste0("Mean elevation = ",round(Elevation)," m"))

#















































################################################################################
# Legacy / extra

##########
# Fractional increase in future precipitation uncertainty during dry snow drought (xSnow = 0.5-0.9, all conditions vs. drought)
for (xfrac in c(0.5, 0.7, 0.9)){
  
  PrecipUncAll = mean(AvgSkill[(AvgSkill$Mode=="ASOForecast" &
                                  AvgSkill$SnowFrac==xfrac &
                                  AvgSkill$Drought=="AllConditions" &
                                  AvgSkill$Watershed=="Tuolumne"),]$UncertaintyContrib)
  
  PrecipUncDrought = mean(AvgSkill[(AvgSkill$Mode=="ASOForecastDrought" &
                                      AvgSkill$SnowFrac==xfrac &
                                      AvgSkill$Drought=="Drought" &
                                      AvgSkill$Watershed=="Tuolumne"),]$UncertaintyContrib)
  
  PrecipPctChange = (PrecipUncDrought - PrecipUncAll) / PrecipUncAll
  
  print(paste0("Snow fraction: ",xfrac))
  print(paste0("Precip. uncertainty percent increase Tuolumne: ",100*round(PrecipPctChange,2),"%"))
  
  PrecipUncAll = mean(AvgSkill[(AvgSkill$Mode=="ASOForecast" &
                                  AvgSkill$SnowFrac==xfrac &
                                  AvgSkill$Drought=="AllConditions" &
                                  AvgSkill$Watershed=="Merced"),]$UncertaintyContrib)
  
  PrecipUncDrought = mean(AvgSkill[(AvgSkill$Mode=="ASOForecastDrought" &
                                      AvgSkill$SnowFrac==xfrac &
                                      AvgSkill$Drought=="Drought" &
                                      AvgSkill$Watershed=="Merced"),]$UncertaintyContrib)
  
  PrecipPctChange = (PrecipUncDrought - PrecipUncAll) / PrecipUncAll
  
  print(paste0("Precip. uncertainty percent increase Merced: ",100*round(PrecipPctChange,2),"%"))
  print("*")
}


##########
# Fractional reduction in total uncertainty using ASO SWE during combined warm + dry snow drought

for (xfrac in c(0.1)){
  
  HydroUncertainty = 1 - AvgSkill[(AvgSkill[,"Drought"]=="Drought" &
                                     AvgSkill[,"SnowFrac"]==xfrac &
                                     AvgSkill[,"Mode"]=="ASOBackcastDrought"),]$Skill
  
  PrecipUncertainty = 1 - HydroUncertainty - AvgSkill[(AvgSkill[,"Drought"]=="Drought" &
                                                         AvgSkill[,"SnowFrac"]==xfrac &
                                                         AvgSkill[,"Mode"]=="ASOForecastDrought"),]$Skill
  
  SnowUncertainty = 1 - HydroUncertainty - PrecipUncertainty - AvgSkill[(AvgSkill[,"Drought"]=="Drought" &
                                                                           AvgSkill[,"SnowFrac"]==xfrac &
                                                                           AvgSkill[,"Mode"]=="StationForecastDrought"),]$Skill
  
  HydroRel = (HydroUncertainty / (HydroUncertainty + PrecipUncertainty + SnowUncertainty))
  PrecipRel = (PrecipUncertainty / (HydroUncertainty + PrecipUncertainty + SnowUncertainty))
  SnowRel = (SnowUncertainty / (HydroUncertainty + PrecipUncertainty + SnowUncertainty))
  
  print(paste0("Snow fraction: ",xfrac," AND bottom-quartile precipitation drought"))
  print(paste0("Hydro uncertainty fraction of total uncertainty: ",100*round(HydroRel,2),"%"))
  print(paste0("Precip uncertainty fraction of total uncertainty: ",100*round(PrecipRel,2),"%"))
  print(paste0("Snow uncertainty fraction of total uncertainty: ",100*round(SnowRel,2),"%"))
  print("*")
}

##########
# Increase in hydrological model uncertainty during warm snow drought (all precip. conditions, 0.5 --> 0.1 SWEfrac)

Unc50 = AvgSkill[(AvgSkill$Mode=="ASOBackcast" &
                    AvgSkill$SnowFrac==0.5 &
                    AvgSkill$Drought=="AllConditions"),]$UncertaintyContrib

Unc10 = AvgSkill[(AvgSkill$Mode=="ASOBackcast" &
                    AvgSkill$SnowFrac==0.1 &
                    AvgSkill$Drought=="AllConditions"),]$UncertaintyContrib

PctChangeHydroUnc = mean((Unc10 - Unc50) / Unc50)
print(paste0("Hydro uncertainty relative increase, SWEfrac 0.5-->0.1: ",100*round(PctChangeHydroUnc,2),"%"))













