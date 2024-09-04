# Create winter-spring precip pairs for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

################################################################################
# Prepare precip timeseries from TUM station

Pdata = read.csv("Data/DailyPrecipTimeseries_TUM.csv")[,-1]
Pdata$Date = as.Date(paste0(Pdata$Date),format="%Y%m%d")
Pdata$Year = as.numeric(format(Pdata$Date,"%Y"))
Pdata$Month = as.numeric(format(Pdata$Date,"%m"))
Pdata$Day = as.numeric(format(Pdata$Date,"%d"))
Pdata$Precip_m = Pdata$Precip_in / (12 * 3.28084)

# Replace NAs (missing) with 0
Pdata$Precip_m[is.na(Pdata$Precip_m)] = 0

plot(Pdata$Date, Pdata$Precip_m, type="l")
# Potential outlier on 8/1/2018 does not matter for this analysis (outisde 10/1-7/31 window)

head(Pdata)

Pyears = 1986:2022

################################################################################
# Function to extract precip October 1-now

Pbefore = function(yr,mon,day,datasource){
  
  CumulP = 0
  
  for (i in 1:length(datasource$Year)){
    
    if ((datasource$Year[i] == (yr-1)) && (datasource$Month[i] >= 10)){
      
      # Last year 10/1 through 12/31
      CumulP = CumulP + datasource$Precip_m[i]
    }
    else if ((datasource$Year[i] == yr) && (datasource$Month[i] < mon)){
      
      # Current year 1/1 through month before flight
      CumulP = CumulP + datasource$Precip_m[i]
    }
    else if ((datasource$Year[i] == yr) && (datasource$Month[i] == mon) && (datasource$Day[i] <= day)){
      
      # Current year 1/1 through month before flight
      CumulP = CumulP + datasource$Precip_m[i]
    }
  }
  return(CumulP)
}

################################################################################
# Function to extract precip now-August 1; only valid beginning January 1

Pafter = function(yr,mon,day,datasource){
  
  CumulP = 0
  
  for (i in 1:length(datasource$Year)){
    
    if ((datasource$Year[i] == yr) && (datasource$Month[i] == mon) && (datasource$Day[i] >= day)){
      
      # Starting month
      CumulP = CumulP + datasource$Precip_m[i]
    }
    else if ((datasource$Year[i] == yr) && (datasource$Month[i] > mon) && (datasource$Month[i] <= 7)){
      
      # Other months until 7/31
      CumulP = CumulP + datasource$Precip_m[i]
    }
  }
  return(CumulP)
}

################################################################################
# Get weather statistics beginning the 1st day of month(s) of interest

Pmonth = 4 # April

PrecipStats = data.frame(WaterYear=Pyears,
                         Oct1ToApr1_Precip_m=NA,
                         Apr1ToJul31_Precip_m=NA)

for (y in 1:length(Pyears)){
  
  ########## Pre-forecast precip
  PrecipStats$Oct1ToApr1_Precip_m[y] = Pbefore(Pyears[y],Pmonth,1,Pdata)
  
  ########## Post-forecast precip
  PrecipStats$Apr1ToJul31_Precip_m[y] = Pafter(Pyears[y],Pmonth,1,Pdata)
}

write.csv(PrecipStats, "Data/April1st_PrecipStats_TUM.csv")

########## Plot for conceptual figure

par(lwd=3)
hist(PrecipStats$Apr1ToJul31_Precip_m,
     xlim=c(0,0.4),breaks=seq(from=0,to=0.4,by=0.05),col='gray50',
     main="April - July P Observed",
     xlab="Precipitation through 7/31, m",
     ylab="Frequency (Years)",
     cex.axis=1.5, cex.main=2, cex.lab=1.5, lwd=3)


# Test correlation of pre- and post-forecast precip
cor(PrecipStats$Oct1ToApr1_Precip_m, PrecipStats$Apr1ToJul31_Precip_m)

plot(PrecipStats$Oct1ToApr1_Precip_m, PrecipStats$Apr1ToJul31_Precip_m)

################################################################################
# Optional - not needed in further processing
################################################################################

################################################################################
# Get weather statistics for each flight date
# Have to do it by flight because we don't know how much SWE is left on an arbitrary day

# Read watershed data
TuolumneData = read.csv("Data/CompiledData_Tuolumne.csv")
MercedData = read.csv("Data/CompiledData_Merced.csv")

########## Tuolumne

nTuolumne = length(TuolumneData$Year)

PuntilAugByFlightTuolumne = matrix(0,nrow=nTuolumne,ncol=length(Pyears))

for (d in 1:nTuolumne){
  for (y in 1:length(Pyears)){
    PuntilAugByFlightTuolumne[d,y] = Pafter(Pyears[y],TuolumneData$Month[d],TuolumneData$Day[d],Pdata)
  }
}
PuntilAugByFlightTuolumne[PuntilAugByFlightTuolumne == 0] = 0.001 # 0.0 values give -Inf logs

PdayLogMeanTuolumne = 1:nTuolumne*NA
PdayLogSigmaTuolumne = 1:nTuolumne*NA
for (d in 1:nTuolumne){
  PdayLogMeanTuolumne[d] = mean(log(PuntilAugByFlightTuolumne[d,]))
  PdayLogSigmaTuolumne[d] = sd(log(PuntilAugByFlightTuolumne[d,]))
}

# Estimate MsoFar by getting P Oct1-flight and comparing it to current SWE
MsoFarTuolumne = 1:nTuolumne
for (f in 1:nTuolumne){
  MsoFarTuolumne[f] = max(Pbefore(TuolumneData$Year[f],TuolumneData$Month[f],TuolumneData$Day[f],Pdata),
                          TuolumneData$SWE_m[f]) # Don't accept obvious undercatch
}

########## Merced

nMerced = length(MercedData$Year)

PuntilAugByFlightMerced = matrix(0,nrow=nMerced,ncol=length(Pyears))

for (d in 1:nMerced){
  for (y in 1:length(Pyears)){
    PuntilAugByFlightMerced[d,y] = Pafter(Pyears[y],MercedData$Month[d],MercedData$Day[d],Pdata)
  }
}
PuntilAugByFlightMerced[PuntilAugByFlightMerced == 0] = 0.001 # 0.0 values give -Inf logs

PdayLogMeanMerced = 1:nMerced
PdayLogSigmaMerced = 1:nMerced
for (d in 1:nMerced){
  PdayLogMeanMerced[d] = mean(log(PuntilAugByFlightMerced[d,]))
  PdayLogSigmaMerced[d] = sd(log(PuntilAugByFlightMerced[d,]))
}

# Estimate MsoFar by getting P Oct1-flight and comparing it to current SWE
MsoFarMerced = 1:nMerced
for (f in 1:nMerced){
  MsoFarMerced[f] = max(Pbefore(MercedData$Year[f],MercedData$Month[f],MercedData$Day[f],Pdata),MercedData$SWE_m[f]) # Don't accept obvious undercatch
}

################################################################################
# Check correlation of drought conditions and snow fraction
TuolumnePtrs = (TuolumneData$Month > 2 & TuolumneData$Month < 5)
MercedPtrs = (MercedData$Month > 2 & MercedData$Month < 5)
SnowFracObsTuolumne = TuolumneData[TuolumnePtrs,]$SWE_m/MsoFarTuolumne[TuolumnePtrs]
SnowFracObsMerced = MercedData[MercedPtrs,]$SWE_m/MsoFarMerced[MercedPtrs]

mean(SnowFracObsTuolumne)
mean(SnowFracObsMerced)

median(SnowFracObsTuolumne)
median(SnowFracObsMerced)

hist(SnowFracObsTuolumne)
hist(SnowFracObsMerced)

par(mfrow=c(1,1))
plot(MsoFarTuolumne[TuolumnePtrs],SnowFracObsTuolumne,col="blue",pch=19,main="Snow Fraction vs. Drought Level\nMarch-April ASO Flights",xlab="Prior P, m",ylab="Fraction of P remaining as SWE")
points(MsoFarMerced[MercedPtrs],SnowFracObsMerced,col="red",pch=17)
legend(1,0.4,legend=c("Tuolumne","Merced"),col=c("blue","red"),pch=c(19,17))

plot(TuolumneData$SWE_m[TuolumnePtrs],SnowFracObsTuolumne,col="blue",pch=19,main="SWE vs. Drought Level\nMarch-April ASO Flights",xlab="Measured SWE, m",ylab="Fraction of P remaining as SWE")
points(MercedData$SWE_m[MercedPtrs],SnowFracObsMerced,col="red",pch=17)
legend(0.8,0.6,legend=c("Tuolumne","Merced"),col=c("blue","red"),pch=c(19,17))
