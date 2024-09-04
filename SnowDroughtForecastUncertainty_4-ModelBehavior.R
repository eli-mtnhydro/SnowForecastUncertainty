# Make plot of model behaviors for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

library(ggplot2)
library(ggh4x) # devtools::install_github("teunbrand/ggh4x")

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

################################################################################
# Forecast functions

RunoffForecast1 = function(W,M,a){
  L = a
  Q = W - (W/M)*L
  return(Q)
  Q = pmax(Q,0)
}

RunoffForecast2 = function(W,M,b){
  L = b*M
  Q = W - (W/M)*L
  return(Q)
  Q = pmax(Q,0)
}

RunoffForecast3 = function(W,M,a,b){
  L = a + b*M
  Q = W - (W/M)*L
  return(Q)
  Q = pmax(Q,0)
}

RunoffForecast4 = function(W,M,N,a,b,c){
  L = a + b*M + c*N
  Q = W - (W/M)*L
  return(Q)
  Q = pmax(Q,0)
}

################################################################################
# Apply models to Tuolumne and Merced using the Bayesian parameter samples

# Read watershed data
TuolumneData = read.csv("Data/CompiledData_Tuolumne.csv")
MercedData = read.csv("Data/CompiledData_Merced.csv")

# Read Bayesian samples
TuolumnePars1 = read.csv("BayesianModels/TuolumneASO_Pars1.csv")
TuolumnePars2 = read.csv("BayesianModels/TuolumneASO_Pars2.csv")
TuolumnePars3 = read.csv("BayesianModels/TuolumneASO_Pars3.csv")
TuolumnePars4 = read.csv("BayesianModels/TuolumneASO_Pars4.csv")
MercedPars1 = read.csv("BayesianModels/MercedASO_Pars1.csv")
MercedPars2 = read.csv("BayesianModels/MercedASO_Pars2.csv")
MercedPars3 = read.csv("BayesianModels/MercedASO_Pars3.csv")
MercedPars4 = read.csv("BayesianModels/MercedASO_Pars4.csv")

# Range over which to analyze the models (water depth in m)
par(mfrow=c(1,2))
hist(unique(TuolumneData$SeasP_m),breaks=seq(from=0,to=2,by=0.1))
hist(unique(MercedData$SeasP_m),breaks=seq(from=0,to=2,by=0.1))
MNrange = seq(0.4,1,0.01)

# Run each model with each set of sampled parameters
# 2 basins, 4 models, 3 runoff-loss-efficiency values

Values = rep(NA,length(MNrange)*2*4*3)

ValueType = rep(c(rep("Runoff",length(MNrange)*4),
                  rep("Loss",length(MNrange)*4),
                  rep("Efficiency",length(MNrange)*4)),2)

Watershed = c(rep("Tuolumne",length(MNrange)*4*3),
              rep("Merced",length(MNrange)*4*3))

Model = rep(c(rep("1ConstantLoss",length(MNrange)),
              rep("2ConstantEfficiency",length(MNrange)),
              rep("3LinearRunoff",length(MNrange)),
              rep("4LinearRunoffStorage",length(MNrange))),2*3)

WaterInput = rep(MNrange,2*4*3)

ModelSpace = data.frame(WaterInput,Model,Watershed,Values,ValueType)

NvalTuolumne = median(unique(TuolumneData$LastYrRunoff_m))
NvalMerced = median(unique(MercedData$LastYrRunoff_m))

for (i in 1:length(MNrange)){
  Mval = MNrange[i]
  Wval = MNrange[i]
  
  ##############################################################################
  # Tuolumne
  ##############################################################################
  
  ########## Constant Loss Model
  Qval = median(RunoffForecast1(Wval,Mval,TuolumnePars1$a))
  
  # Runoff
  ModelSpace[(ModelSpace[,"Model"]=="1ConstantLoss" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Runoff"),]$Values[i] = Qval
  
  # Loss
  ModelSpace[(ModelSpace[,"Model"]=="1ConstantLoss" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Loss"),]$Values[i] = Wval - Qval
  
  # Efficiency
  ModelSpace[(ModelSpace[,"Model"]=="1ConstantLoss" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Efficiency"),]$Values[i] = Qval / Wval
  
  ########## Constant Efficiency Model
  Qval = median(RunoffForecast2(Wval,Mval,TuolumnePars2$b))
  
  # Runoff
  ModelSpace[(ModelSpace[,"Model"]=="2ConstantEfficiency" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Runoff"),]$Values[i] = Qval
  
  # Loss
  ModelSpace[(ModelSpace[,"Model"]=="2ConstantEfficiency" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Loss"),]$Values[i] = Wval - Qval
  
  # Efficiency
  ModelSpace[(ModelSpace[,"Model"]=="2ConstantEfficiency" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Efficiency"),]$Values[i] = Qval / Wval
  
  ########## Linear Runoff Model
  Qval = median(RunoffForecast3(Wval,Mval,TuolumnePars3$a,TuolumnePars3$b))
  
  # Runoff
  ModelSpace[(ModelSpace[,"Model"]=="3LinearRunoff" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Runoff"),]$Values[i] = Qval
  
  # Loss
  ModelSpace[(ModelSpace[,"Model"]=="3LinearRunoff" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Loss"),]$Values[i] = Wval - Qval
  
  # Efficiency
  ModelSpace[(ModelSpace[,"Model"]=="3LinearRunoff" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Efficiency"),]$Values[i] = Qval / Wval
  
  ########## Linear Runoff Model With Storage
  Qval = median(RunoffForecast4(Wval,Mval,NvalTuolumne,TuolumnePars4$a,TuolumnePars4$b,TuolumnePars4$c))
  
  # Runoff
  ModelSpace[(ModelSpace[,"Model"]=="4LinearRunoffStorage" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Runoff"),]$Values[i] = Qval
  
  # Loss
  ModelSpace[(ModelSpace[,"Model"]=="4LinearRunoffStorage" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Loss"),]$Values[i] = Wval - Qval
  
  # Efficiency
  ModelSpace[(ModelSpace[,"Model"]=="4LinearRunoffStorage" &
                ModelSpace[,"Watershed"]=="Tuolumne" &
                ModelSpace[,"ValueType"]=="Efficiency"),]$Values[i] = Qval / Wval
  
  ##############################################################################
  # Merced
  ##############################################################################
  
  ########## Constant Loss Model
  Qval = median(RunoffForecast1(Wval,Mval,MercedPars1$a))
  
  # Runoff
  ModelSpace[(ModelSpace[,"Model"]=="1ConstantLoss" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Runoff"),]$Values[i] = Qval
  
  # Loss
  ModelSpace[(ModelSpace[,"Model"]=="1ConstantLoss" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Loss"),]$Values[i] = Wval - Qval
  
  # Efficiency
  ModelSpace[(ModelSpace[,"Model"]=="1ConstantLoss" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Efficiency"),]$Values[i] = Qval / Wval
  
  ########## Constant Efficiency Model
  Qval = median(RunoffForecast2(Wval,Mval,MercedPars2$b))
  
  # Runoff
  ModelSpace[(ModelSpace[,"Model"]=="2ConstantEfficiency" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Runoff"),]$Values[i] = Qval
  
  # Loss
  ModelSpace[(ModelSpace[,"Model"]=="2ConstantEfficiency" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Loss"),]$Values[i] = Wval - Qval
  
  # Efficiency
  ModelSpace[(ModelSpace[,"Model"]=="2ConstantEfficiency" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Efficiency"),]$Values[i] = Qval / Wval
  
  ########## Linear Runoff Model
  Qval = median(RunoffForecast3(Wval,Mval,MercedPars3$a,MercedPars3$b))
  
  # Runoff
  ModelSpace[(ModelSpace[,"Model"]=="3LinearRunoff" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Runoff"),]$Values[i] = Qval
  
  # Loss
  ModelSpace[(ModelSpace[,"Model"]=="3LinearRunoff" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Loss"),]$Values[i] = Wval - Qval
  
  # Efficiency
  ModelSpace[(ModelSpace[,"Model"]=="3LinearRunoff" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Efficiency"),]$Values[i] = Qval / Wval
  
  ########## Linear Runoff Model With Storage
  Qval = median(RunoffForecast4(Wval,Mval,NvalMerced,MercedPars4$a,MercedPars4$b,MercedPars4$c))
  
  # Runoff
  ModelSpace[(ModelSpace[,"Model"]=="4LinearRunoffStorage" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Runoff"),]$Values[i] = Qval
  
  # Loss
  ModelSpace[(ModelSpace[,"Model"]=="4LinearRunoffStorage" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Loss"),]$Values[i] = Wval - Qval
  
  # Efficiency
  ModelSpace[(ModelSpace[,"Model"]=="4LinearRunoffStorage" &
                ModelSpace[,"Watershed"]=="Merced" &
                ModelSpace[,"ValueType"]=="Efficiency"),]$Values[i] = Qval / Wval
  
}

head(ModelSpace)

# Really complicated ggplot to show loss, runoff, and efficiency on different scales

ModelSpace$ValueType = factor(ModelSpace$ValueType, levels=c("Loss","Efficiency","Runoff"))

design = matrix(c(1,2,3,2), 2, 2)

g1 = ggplot(data=ModelSpace, aes(x=WaterInput, y=Values)) +
  geom_line(aes(color=Watershed, linetype=Model), linewidth=3, lineend="round") +
  labs(x="Water Input (SWE + P), m") +
  scale_color_manual(values=c("aquamarine3","dodgerblue3"),
                     labels=c("Merced","Tuolumne")) +
  scale_linetype_manual(values=c("dashed","dotted","dotdash","solid"),
                        labels=c("1. Constant Loss",
                                 "2. Constant Efficiency",
                                 "3. Linear Closed\nWater Balance",
                                 "4. Linear Open\nWater Balance")) +
  scale_x_continuous(expand=c(0,0)) +
  ggh4x::facet_manual(ValueType ~., design=design, scales="free_y", strip.position="left",
                      labeller=as_labeller(c("Loss"="Loss, m",
                                             "Efficiency"="Runoff Efficiency (Q/P)",
                                             "Runoff"="Runoff, m"))) +
  facetted_pos_scales(y=list(ValueType=="Loss"~scale_y_continuous(limits=c(0.1,0.32),
                                                                  breaks=seq(0.1,0.4,0.1),
                                                                  sec.axis=dup_axis(name=NULL,labels=NULL)),
                             ValueType=="Runoff"~scale_y_continuous(limits=c(0.2,0.85),
                                                                    breaks=seq(0.2,0.9,0.1),
                                                                    sec.axis=dup_axis(name=NULL,labels=NULL)),
                             ValueType=="Efficiency"~scale_y_continuous(limits=c(0.5,0.85),
                                                                        breaks=seq(0.5,0.9,0.1),
                                                                        labels=paste0(seq(50,90,10),"%"),
                                                                        sec.axis=dup_axis(name=NULL,labels=NULL)))) +
  labs(linetype="Runoff Model",
       color="\nWatershed") +
  theme_bw() +
  theme(axis.title.x=element_text(size=32,margin=margin(10,0,0,0)),
        axis.text.x=element_text(size=24,margin=margin(5,0,5,0),color="black"),
        axis.title.y=element_blank(),
        axis.text.y=element_text(size=24,margin=margin(0,5,0,5),color="black"),
        legend.title=element_text(size=32),
        legend.text=element_text(size=24),
        legend.spacing.y=unit(1,"cm"),
        axis.ticks=element_line(color="black"),
        axis.ticks.length=unit(0.75,"cm"),
        plot.margin=margin(20,20,20,20),
        panel.background=element_blank(),
        panel.grid=element_blank(),
        panel.spacing=unit(0.5,"cm"),
        legend.key=element_blank(),
        legend.key.width=unit(3,"cm"),
        legend.key.height=unit(1.5,"cm"),
        strip.text=element_text(color="black",size=32),
        strip.background=element_blank(),
        strip.placement="outside",
        strip.text.y=element_text(angle=180)) +
  guides(linetype=guide_legend(override.aes=list(linewidth=2), byrow=TRUE, order=1),
         color=guide_legend(override.aes=list(linewidth=4), byrow=TRUE, order=2))

# print(g1)

# ggsave("Loss-Runoff-Efficiency_Plots.png", plot=g1, dpi=300)
ggsave("Figures/Figure_4.pdf", plot=g1,
       width=unit(18,"cm"), height=unit(12,"cm"), dpi=300)

# Max and min runoff efficiency
max(ModelSpace[ModelSpace$ValueType=="Efficiency","Values"])
min(ModelSpace[ModelSpace$ValueType=="Efficiency","Values"])
