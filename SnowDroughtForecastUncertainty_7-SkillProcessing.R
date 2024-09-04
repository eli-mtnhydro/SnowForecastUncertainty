# Post-process data for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

SkillSet = read.csv("SnowDroughtForecastUncertainty_ModelSkillResults.csv")[,-1]
head(SkillSet)

DroughtQuantiles = unique(SkillSet$Drought)
xSnow = unique(SkillSet$SnowFrac)

################################################################################
# Aggregate and post-process the results

DroughtLevel = 0.25 # Must be included in DroughtQuantiles

# Compute averages
TuolumneAvgSkillValues = 0*1:(length(xSnow)*3)
MercedAvgSkillValues = 0*1:(length(xSnow)*3)
TuolumneAvgSkillValuesDrought = 0*1:(length(xSnow)*3)
MercedAvgSkillValuesDrought = 0*1:(length(xSnow)*3)

TuolumneAvgIncrementalSkillValues = 0*1:(length(xSnow)*3)
MercedAvgIncrementalSkillValues = 0*1:(length(xSnow)*3)
TuolumneAvgIncrementalSkillValuesDrought = 0*1:(length(xSnow)*3)
MercedAvgIncrementalSkillValuesDrought = 0*1:(length(xSnow)*3)

TuolumneAvgUncertaintyValues = 0*1:(length(xSnow)*3)
MercedAvgUncertaintyValues = 0*1:(length(xSnow)*3)
TuolumneAvgUncertaintyValuesDrought = 0*1:(length(xSnow)*3)
MercedAvgUncertaintyValuesDrought = 0*1:(length(xSnow)*3)

for (x in 1:length(xSnow)){
  
  ########## Absolute skill: station, forecast, backcast
  
  TuolumneAvgSkillValues[x] = mean(SkillSet[(SkillSet[,"Watershed"]=="Tuolumne" &
                                               SkillSet[,"Drought"]==1 &
                                               SkillSet[,"SnowFrac"]==xSnow[x] &
                                               SkillSet[,"Mode"]=="StationForecast"),]$Skill)
  
  TuolumneAvgSkillValues[x+length(xSnow)] = mean(SkillSet[(SkillSet[,"Watershed"]=="Tuolumne" &
                                                             SkillSet[,"Drought"]==1 &
                                                             SkillSet[,"SnowFrac"]==xSnow[x] &
                                                             SkillSet[,"Mode"]=="ASOForecast"),]$Skill)
  
  TuolumneAvgSkillValues[x+2*length(xSnow)] = mean(SkillSet[(SkillSet[,"Watershed"]=="Tuolumne" &
                                                               SkillSet[,"Drought"]==1 &
                                                               SkillSet[,"SnowFrac"]==xSnow[x] &
                                                               SkillSet[,"Mode"]=="ASOBackcast"),]$Skill)
  
  MercedAvgSkillValues[x] = mean(SkillSet[(SkillSet[,"Watershed"]=="Merced" &
                                             SkillSet[,"Drought"]==1 &
                                             SkillSet[,"SnowFrac"]==xSnow[x] &
                                             SkillSet[,"Mode"]=="StationForecast"),]$Skill)
  
  MercedAvgSkillValues[x+length(xSnow)] = mean(SkillSet[(SkillSet[,"Watershed"]=="Merced" &
                                                           SkillSet[,"Drought"]==1 &
                                                           SkillSet[,"SnowFrac"]==xSnow[x] &
                                                           SkillSet[,"Mode"]=="ASOForecast"),]$Skill)
  
  MercedAvgSkillValues[x+2*length(xSnow)] = mean(SkillSet[(SkillSet[,"Watershed"]=="Merced" &
                                                             SkillSet[,"Drought"]==1 &
                                                             SkillSet[,"SnowFrac"]==xSnow[x] & SkillSet[,"Mode"]=="ASOBackcast"),]$Skill)
  
  TuolumneAvgSkillValuesDrought[x] = mean(SkillSet[(SkillSet[,"Watershed"]=="Tuolumne" &
                                                      SkillSet[,"Drought"]==DroughtLevel &
                                                      SkillSet[,"SnowFrac"]==xSnow[x] &
                                                      SkillSet[,"Mode"]=="StationForecast"),]$Skill)
  
  TuolumneAvgSkillValuesDrought[x+length(xSnow)] = mean(SkillSet[(SkillSet[,"Watershed"]=="Tuolumne" &
                                                                    SkillSet[,"Drought"]==DroughtLevel &
                                                                    SkillSet[,"SnowFrac"]==xSnow[x] &
                                                                    SkillSet[,"Mode"]=="ASOForecast"),]$Skill)
  
  TuolumneAvgSkillValuesDrought[x+2*length(xSnow)] = mean(SkillSet[(SkillSet[,"Watershed"]=="Tuolumne" &
                                                                      SkillSet[,"Drought"]==DroughtLevel &
                                                                      SkillSet[,"SnowFrac"]==xSnow[x] &
                                                                      SkillSet[,"Mode"]=="ASOBackcast"),]$Skill)
  
  MercedAvgSkillValuesDrought[x] = mean(SkillSet[(SkillSet[,"Watershed"]=="Merced" &
                                                    SkillSet[,"Drought"]==DroughtLevel &
                                                    SkillSet[,"SnowFrac"]==xSnow[x] &
                                                    SkillSet[,"Mode"]=="StationForecast"),]$Skill)
  
  MercedAvgSkillValuesDrought[x+length(xSnow)] = mean(SkillSet[(SkillSet[,"Watershed"]=="Merced" &
                                                                  SkillSet[,"Drought"]==DroughtLevel &
                                                                  SkillSet[,"SnowFrac"]==xSnow[x] &
                                                                  SkillSet[,"Mode"]=="ASOForecast"),]$Skill)
  
  MercedAvgSkillValuesDrought[x+2*length(xSnow)] = mean(SkillSet[(SkillSet[,"Watershed"]=="Merced" &
                                                                    SkillSet[,"Drought"]==DroughtLevel &
                                                                    SkillSet[,"SnowFrac"]==xSnow[x] &
                                                                    SkillSet[,"Mode"]=="ASOBackcast"),]$Skill)
  
  ########## Incremental skill
  
  TuolumneAvgIncrementalSkillValues[x] = TuolumneAvgSkillValues[x]
  TuolumneAvgIncrementalSkillValues[x+length(xSnow)] = TuolumneAvgSkillValues[x+length(xSnow)] - TuolumneAvgSkillValues[x]
  TuolumneAvgIncrementalSkillValues[x+2*length(xSnow)] = TuolumneAvgSkillValues[x+2*length(xSnow)] - TuolumneAvgSkillValues[x+length(xSnow)]
  
  MercedAvgIncrementalSkillValues[x] = MercedAvgSkillValues[x]
  MercedAvgIncrementalSkillValues[x+length(xSnow)] = MercedAvgSkillValues[x+length(xSnow)] - MercedAvgSkillValues[x]
  MercedAvgIncrementalSkillValues[x+2*length(xSnow)] = MercedAvgSkillValues[x+2*length(xSnow)] - MercedAvgSkillValues[x+length(xSnow)]
  
  TuolumneAvgIncrementalSkillValuesDrought[x] = TuolumneAvgSkillValuesDrought[x]
  TuolumneAvgIncrementalSkillValuesDrought[x+length(xSnow)] = TuolumneAvgSkillValuesDrought[x+length(xSnow)] - TuolumneAvgSkillValuesDrought[x]
  TuolumneAvgIncrementalSkillValuesDrought[x+2*length(xSnow)] = TuolumneAvgSkillValuesDrought[x+2*length(xSnow)] - TuolumneAvgSkillValuesDrought[x+length(xSnow)]
  
  MercedAvgIncrementalSkillValuesDrought[x] = MercedAvgSkillValuesDrought[x]
  MercedAvgIncrementalSkillValuesDrought[x+length(xSnow)] = MercedAvgSkillValuesDrought[x+length(xSnow)] - MercedAvgSkillValuesDrought[x]
  MercedAvgIncrementalSkillValuesDrought[x+2*length(xSnow)] = MercedAvgSkillValuesDrought[x+2*length(xSnow)] - MercedAvgSkillValuesDrought[x+length(xSnow)]
  
  ########## Uncertainty contribution
  
  # Station forecast: snow uncertainty = 1 - hydro uncertainty - precip uncertainty - station skill
  # ASO forecast: precip uncertainty = 1 - hydro uncertainty - ASO forecast skill
  # ASO backcast: hydro uncertainty = 1 - ASO backcast skill
  # !!! easiest to calculate in reverse order, as done here:
  
  TuolumneAvgUncertaintyValues[x+2*length(xSnow)] = 1 - TuolumneAvgSkillValues[x+2*length(xSnow)]
  TuolumneAvgUncertaintyValues[x+length(xSnow)] = 1 - TuolumneAvgUncertaintyValues[x+2*length(xSnow)] - TuolumneAvgSkillValues[x+length(xSnow)]
  TuolumneAvgUncertaintyValues[x] = 1 - TuolumneAvgUncertaintyValues[x+2*length(xSnow)] - TuolumneAvgUncertaintyValues[x+length(xSnow)] - TuolumneAvgSkillValues[x]
  
  MercedAvgUncertaintyValues[x+2*length(xSnow)] = 1 - MercedAvgSkillValues[x+2*length(xSnow)]
  MercedAvgUncertaintyValues[x+length(xSnow)] = 1 - MercedAvgUncertaintyValues[x+2*length(xSnow)] - MercedAvgSkillValues[x+length(xSnow)]
  MercedAvgUncertaintyValues[x] = 1 - MercedAvgUncertaintyValues[x+2*length(xSnow)] - MercedAvgUncertaintyValues[x+length(xSnow)] - MercedAvgSkillValues[x]
  
  TuolumneAvgUncertaintyValuesDrought[x+2*length(xSnow)] = 1 - TuolumneAvgSkillValuesDrought[x+2*length(xSnow)]
  TuolumneAvgUncertaintyValuesDrought[x+length(xSnow)] = 1 - TuolumneAvgUncertaintyValuesDrought[x+2*length(xSnow)] - TuolumneAvgSkillValuesDrought[x+length(xSnow)]
  TuolumneAvgUncertaintyValuesDrought[x] = 1 - TuolumneAvgUncertaintyValuesDrought[x+2*length(xSnow)] - TuolumneAvgUncertaintyValuesDrought[x+length(xSnow)] - TuolumneAvgSkillValuesDrought[x]
  
  MercedAvgUncertaintyValuesDrought[x+2*length(xSnow)] = 1 - MercedAvgSkillValuesDrought[x+2*length(xSnow)]
  MercedAvgUncertaintyValuesDrought[x+length(xSnow)] = 1 - MercedAvgUncertaintyValuesDrought[x+2*length(xSnow)] - MercedAvgSkillValuesDrought[x+length(xSnow)]
  MercedAvgUncertaintyValuesDrought[x] = 1 - MercedAvgUncertaintyValuesDrought[x+2*length(xSnow)] - MercedAvgUncertaintyValuesDrought[x+length(xSnow)] - MercedAvgSkillValuesDrought[x]
}

JoinedSkillValues = c(TuolumneAvgSkillValues,
                      TuolumneAvgSkillValuesDrought,
                      MercedAvgSkillValues,
                      MercedAvgSkillValuesDrought)

JoinedIncrementalSkillValues = c(TuolumneAvgIncrementalSkillValues,
                                 TuolumneAvgIncrementalSkillValuesDrought,
                                 MercedAvgIncrementalSkillValues,
                                 MercedAvgIncrementalSkillValuesDrought)

JoinedUncertaintyValues = c(TuolumneAvgUncertaintyValues,
                            TuolumneAvgUncertaintyValuesDrought,
                            MercedAvgUncertaintyValues,
                            MercedAvgUncertaintyValuesDrought)

# 5x snow fraction, 3x modes, 2x non-drought/drought, 2x basins

SnowFrac = rep(xSnow,3*2*2)

Watershed = c(rep("Tuolumne",
                  length(xSnow)*3*2),
              rep("Merced",
                  length(xSnow)*3*2))

Mode = rep(c(rep("StationForecast",length(xSnow)),
             rep("ASOForecast",length(xSnow)),
             rep("ASOBackcast",length(xSnow)),
             rep("StationForecastDrought",length(xSnow)),
             rep("ASOForecastDrought",length(xSnow)),
             rep("ASOBackcastDrought",length(xSnow))),
           2)

Drought = rep(c(rep("AllConditions",
                    length(xSnow)*3),
                rep("Drought",
                    length(xSnow)*3)),
              2)

# Combinations of watershed/drought
BarNum = c(rep(1,length(xSnow)*3),
           rep(2,length(xSnow)*3),
           rep(3,length(xSnow)*3),
           rep(4,length(xSnow)*3))

AvgSkill = data.frame(SnowFrac,
                      Mode,
                      Drought,
                      Watershed,
                      Skill=JoinedSkillValues,
                      IncrementalSkill=JoinedIncrementalSkillValues,
                      UncertaintyContrib=JoinedUncertaintyValues,
                      BarNum)

write.csv(AvgSkill,"SnowDroughtForecastUncertainty_ModelSkillResults_AvgSkill.csv")

########## Drought vs. all conditions comparison only

TuolumneDeltaSkill = AvgSkill[(AvgSkill[,"Watershed"]=="Tuolumne" &
                                 AvgSkill[,"Drought"]=="Drought"),]$Skill - AvgSkill[(AvgSkill[,"Watershed"]=="Tuolumne" &
                                                                                        AvgSkill[,"Drought"]=="AllConditions"),]$Skill

MercedDeltaSkill = AvgSkill[(AvgSkill[,"Watershed"]=="Merced" &
                               AvgSkill[,"Drought"]=="Drought"),]$Skill - AvgSkill[(AvgSkill[,"Watershed"]=="Merced" &
                                                                                      AvgSkill[,"Drought"]=="AllConditions"),]$Skill

TuolumneDeltaUncertaintyContrib = AvgSkill[(AvgSkill[,"Watershed"]=="Tuolumne" &
                                              AvgSkill[,"Drought"]=="Drought"),]$UncertaintyContrib - AvgSkill[(AvgSkill[,"Watershed"]=="Tuolumne" &
                                                                                                     AvgSkill[,"Drought"]=="AllConditions"),]$UncertaintyContrib

MercedDeltaUncertaintyContrib = AvgSkill[(AvgSkill[,"Watershed"]=="Merced" &
                                              AvgSkill[,"Drought"]=="Drought"),]$UncertaintyContrib - AvgSkill[(AvgSkill[,"Watershed"]=="Merced" &
                                                                                                                  AvgSkill[,"Drought"]=="AllConditions"),]$UncertaintyContrib

# Percent increase in uncertainty
TuolumneUncertaintyContribPctIncrease = TuolumneDeltaUncertaintyContrib / AvgSkill[(AvgSkill[,"Watershed"]=="Tuolumne" &
                                                                                      AvgSkill[,"Drought"]=="AllConditions"),]$UncertaintyContrib

MercedUncertaintyContribPctIncrease = MercedDeltaUncertaintyContrib / AvgSkill[(AvgSkill[,"Watershed"]=="Merced" &
                                                                                      AvgSkill[,"Drought"]=="AllConditions"),]$UncertaintyContrib

# 5x snow fraction, 3x modes, 2x basins
SnowFrac = c(AvgSkill[(AvgSkill[,"Watershed"]=="Tuolumne" &
                         AvgSkill[,"Drought"]=="AllConditions"),]$SnowFrac,
             AvgSkill[(AvgSkill[,"Watershed"]=="Merced" &
                         AvgSkill[,"Drought"]=="AllConditions"),]$SnowFrac)

Mode = c(AvgSkill[(AvgSkill[,"Watershed"]=="Tuolumne" &
                     AvgSkill[,"Drought"]=="AllConditions"),]$Mode,
         AvgSkill[(AvgSkill[,"Watershed"]=="Merced" &
                     AvgSkill[,"Drought"]=="AllConditions"),]$Mode)

Watershed = c(AvgSkill[(AvgSkill[,"Watershed"]=="Tuolumne" &
                          AvgSkill[,"Drought"]=="AllConditions"),]$Watershed,
              AvgSkill[(AvgSkill[,"Watershed"]=="Merced" &
                          AvgSkill[,"Drought"]=="AllConditions"),]$Watershed)

# Which mode
BarNum = rep(c(rep(1,length(xSnow)),
               rep(2,length(xSnow)),
               rep(3,length(xSnow))),
             2)

DeltaSkill = data.frame(SnowFrac,
                        Mode,
                        Watershed,
                        DeltaSkill=c(TuolumneDeltaSkill,
                                     MercedDeltaSkill),
                        UncertaintyContribIncrease=c(TuolumneDeltaUncertaintyContrib,
                                                     MercedDeltaUncertaintyContrib),
                        UncertaintyContribIncreasePct=c(TuolumneUncertaintyContribPctIncrease,
                                                        MercedUncertaintyContribPctIncrease),
                        BarNum)

write.csv(DeltaSkill,"SnowDroughtForecastUncertainty_ModelSkillResults_DeltaSkill.csv")
