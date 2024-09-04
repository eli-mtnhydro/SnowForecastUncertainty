# Make plot of measured vs. modeled runoff for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

library(hydroGOF)
library(ggplot2)

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

nTuolumne = length(TuolumneData$Year)
nMerced = length(MercedData$Year)

# Read Bayesian samples
TuolumnePars1 = read.csv("BayesianModels/TuolumneASO_Pars1.csv")
TuolumnePars2 = read.csv("BayesianModels/TuolumneASO_Pars2.csv")
TuolumnePars3 = read.csv("BayesianModels/TuolumneASO_Pars3.csv")
TuolumnePars4 = read.csv("BayesianModels/TuolumneASO_Pars4.csv")
MercedPars1 = read.csv("BayesianModels/MercedASO_Pars1.csv")
MercedPars2 = read.csv("BayesianModels/MercedASO_Pars2.csv")
MercedPars3 = read.csv("BayesianModels/MercedASO_Pars3.csv")
MercedPars4 = read.csv("BayesianModels/MercedASO_Pars4.csv")

ParameterSamples = length(TuolumnePars1$a)

################################################################################
WeatherSamples = 1000

########## Tuolumne ##########
QforecastTuolumne = array(0, dim=c(nModels,nTuolumne,WeatherSamples,ParameterSamples))
QbackcastTuolumne = array(0, dim=c(nModels,nTuolumne,ParameterSamples))
ModelMedianTuolumne = array(0, dim=c(nModels,nTuolumne))

# Calculate model characteristics for each flight
for (f in 1:nTuolumne) {
  # Backcast mode
  Mval = TuolumneData$SeasP_m[f]
  Wval = TuolumneData$SWE_m[f] + TuolumneData$FutureP_m[f]
  Nval = TuolumneData$LastYrRunoff_m[f]
  
  QbackcastTuolumne[1,f,] = RunoffForecast1(Wval,Mval,TuolumnePars1$a) +
    rnorm(ParameterSamples,0,1) * TuolumnePars1$sigma * RunoffForecast1(Mval,Mval,TuolumnePars1$a)
  
  QbackcastTuolumne[2,f,] = RunoffForecast2(Wval,Mval,TuolumnePars2$b) +
    rnorm(ParameterSamples,0,1) * TuolumnePars2$sigma * RunoffForecast2(Mval,Mval,TuolumnePars2$b)
  
  QbackcastTuolumne[3,f,] = RunoffForecast3(Wval,Mval,TuolumnePars3$a,TuolumnePars3$b) +
    rnorm(ParameterSamples,0,1) * TuolumnePars3$sigma * RunoffForecast3(Mval,Mval,TuolumnePars3$a,TuolumnePars3$b)
  
  QbackcastTuolumne[4,f,] = RunoffForecast4(Wval,Mval,Nval,TuolumnePars4$a,TuolumnePars4$b,TuolumnePars4$c) +
    rnorm(ParameterSamples,0,1) * TuolumnePars4$sigma * RunoffForecast4(Mval,Mval,Nval,TuolumnePars4$a,TuolumnePars4$b,TuolumnePars4$c)
  
  ModelMedianTuolumne[1,f] = median(QbackcastTuolumne[1,f,])
  ModelMedianTuolumne[2,f] = median(QbackcastTuolumne[2,f,])
  ModelMedianTuolumne[3,f] = median(QbackcastTuolumne[3,f,])
  ModelMedianTuolumne[4,f] = median(QbackcastTuolumne[4,f,])
}

########## Merced ##########
QforecastMerced = array(0, dim=c(nModels,nMerced,WeatherSamples,ParameterSamples))
QbackcastMerced = array(0, dim=c(nModels,nMerced,ParameterSamples))
ModelMedianMerced = array(0, dim=c(nModels,nMerced))

# Calculate model characteristics for each flight
for (f in 1:nMerced) {
  # Backcast mode
  Mval = MercedData$SeasP_m[f]
  Wval = MercedData$SWE_m[f] + MercedData$FutureP_m[f]
  Nval = MercedData$LastYrRunoff_m[f]
  
  QbackcastMerced[1,f,] = RunoffForecast1(Wval,Mval,MercedPars1$a) +
    rnorm(ParameterSamples,0,1) * MercedPars1$sigma * RunoffForecast1(Mval,Mval,MercedPars1$a)
  
  QbackcastMerced[2,f,] = RunoffForecast2(Wval,Mval,MercedPars2$b) +
    rnorm(ParameterSamples,0,1) * MercedPars2$sigma * RunoffForecast2(Mval,Mval,MercedPars2$b)
  
  QbackcastMerced[3,f,] = RunoffForecast3(Wval,Mval,MercedPars3$a,MercedPars3$b) +
    rnorm(ParameterSamples,0,1) * MercedPars3$sigma * RunoffForecast3(Mval,Mval,MercedPars3$a,MercedPars3$b)
  
  QbackcastMerced[4,f,] = RunoffForecast4(Wval,Mval,Nval,MercedPars4$a,MercedPars4$b,MercedPars4$c) +
    rnorm(ParameterSamples,0,1) * MercedPars4$sigma * RunoffForecast4(Mval,Mval,Nval,MercedPars4$a,MercedPars4$b,MercedPars4$c)
  
  ModelMedianMerced[1,f] = median(QbackcastMerced[1,f,])
  ModelMedianMerced[2,f] = median(QbackcastMerced[2,f,])
  ModelMedianMerced[3,f] = median(QbackcastMerced[3,f,])
  ModelMedianMerced[4,f] = median(QbackcastMerced[4,f,])
}

################################################################################
# Measured vs. modeled plots

par(mfrow=c(2,2))
plot(TuolumneData$FutureRunoff_m,ModelMedianTuolumne[1,],col="deepskyblue2",pch=16,xlim=c(0,1.7),ylim=c(0,1.7),main="Constant-Loss Model",xlab="Observed Runoff, m",ylab="Modeled runoff, m",cex=1)
points(MercedData$FutureRunoff_m,ModelMedianMerced[1,],col="firebrick2",pch=17,cex=1.1)
lines(0:2,0:2,col='black',lty="dashed")
legend(0.9,0.6,cex=0.8,legend=c("Tuolumne","Merced","1:1 Line"),col=c("deepskyblue2","firebrick2","black"),lty=c(NA,NA,2),pch=c(16,17,NA))

plot(TuolumneData$FutureRunoff_m,ModelMedianTuolumne[2,],col="deepskyblue2",pch=16,xlim=c(0,1.7),ylim=c(0,1.7),main="Constant-Efficiency Model",xlab="Observed Runoff, m",ylab="Modeled runoff, m",cex=1)
points(MercedData$FutureRunoff_m,ModelMedianMerced[2,],col="firebrick2",pch=17,cex=1.1)
lines(0:2,0:2,col='black',lty="dashed")
legend(0.9,0.6,cex=0.8,legend=c("Tuolumne","Merced","1:1 Line"),col=c("deepskyblue2","firebrick2","black"),lty=c(NA,NA,2),pch=c(16,17,NA))

plot(TuolumneData$FutureRunoff_m,ModelMedianTuolumne[3,],col="deepskyblue2",pch=16,xlim=c(0,1.7),ylim=c(0,1.7),main="Linear Closed Water Balance",xlab="Observed Runoff, m",ylab="Modeled runoff, m",cex=1)
points(MercedData$FutureRunoff_m,ModelMedianMerced[3,],col="firebrick2",pch=17,cex=1.1)
lines(0:2,0:2,col='black',lty="dashed")
legend(0.9,0.6,cex=0.8,legend=c("Tuolumne","Merced","1:1 Line"),col=c("deepskyblue2","firebrick2","black"),lty=c(NA,NA,2),pch=c(16,17,NA))

plot(TuolumneData$FutureRunoff_m,ModelMedianTuolumne[4,],col="deepskyblue2",pch=16,xlim=c(0,1.7),ylim=c(0,1.7),main="Linear Open Water Balance",xlab="Observed Runoff, m",ylab="Modeled runoff, m",cex=1)
points(MercedData$FutureRunoff_m,ModelMedianMerced[4,],col="firebrick2",pch=17,cex=1.1)
lines(0:2,0:2,col='black',lty="dashed")
legend(0.9,0.6,cex=0.8,legend=c("Tuolumne","Merced","1:1 Line"),col=c("deepskyblue2","firebrick2","black"),lty=c(NA,NA,2),pch=c(16,17,NA))

########## ggplot version

RunoffData = data.frame(Measured=c(rep(TuolumneData$FutureRunoff_m,4),
                                 rep(MercedData$FutureRunoff_m,4)),
                        Modeled=c(ModelMedianTuolumne[1,],
                                  ModelMedianTuolumne[2,],
                                  ModelMedianTuolumne[3,],
                                  ModelMedianTuolumne[4,],
                                   ModelMedianMerced[1,],
                                  ModelMedianMerced[2,],
                                  ModelMedianMerced[3,],
                                  ModelMedianMerced[4,]),
                        Watershed=c(rep("Tuolumne",length(TuolumneData$FutureRunoff_m)*4),
                                    rep("Merced",length(MercedData$FutureRunoff_m)*4)),
                        Model=c(rep("1. Constant Loss",length(TuolumneData$FutureRunoff_m)),
                                rep("2. Constant Efficiency",length(TuolumneData$FutureRunoff_m)),
                                rep("3. Linear Closed\nWater Balance",length(TuolumneData$FutureRunoff_m)),
                                rep("4. Linear Open\nWater Balance",length(TuolumneData$FutureRunoff_m)),
                                rep("1. Constant Loss",length(MercedData$FutureRunoff_m)),
                                rep("2. Constant Efficiency",length(MercedData$FutureRunoff_m)),
                                rep("3. Linear Closed\nWater Balance",length(MercedData$FutureRunoff_m)),
                                rep("4. Linear Open\nWater Balance",length(MercedData$FutureRunoff_m))))

RunoffData$Model = factor(RunoffData$Model, levels=c("1. Constant Loss",
                                                     "2. Constant Efficiency",
                                                     "3. Linear Closed\nWater Balance",
                                                     "4. Linear Open\nWater Balance"))

RunoffData$Watershed = factor(RunoffData$Watershed,levels=c("Tuolumne","Merced"))

max(RunoffData$Measured)
max(RunoffData$Modeled)

g1 = ggplot(data=RunoffData, aes(x=Modeled, y=Measured)) +
  geom_point(size=2.5, stroke=NA, aes(fill=Watershed, shape=Watershed)) +
  geom_abline(slope=1, intercept=0, linewidth=1) +
  labs(x="Modeled Runoff, m", y="Measured Runoff, m") +
  scale_fill_manual(values=c("dodgerblue3","aquamarine3")) +
  scale_shape_manual(values=c(24,25)) +
  scale_x_continuous(limits=c(0,1.75),
                     breaks=seq(0,1.75,0.5),
                     expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1.75),
                     breaks=seq(0,1.75,0.5),
                     expand=c(0,0)) +
  coord_fixed() +
  theme_bw() +
  theme(axis.title.x=element_text(size=18,margin=margin(5,0,0,0)),
        axis.text.x=element_text(size=14,margin=margin(2.5,0,2.5,0),color="black"),
        axis.title.y=element_text(size=18,margin=margin(0,5,0,0)),
        axis.text.y=element_text(size=14,margin=margin(0,2.5,0,2.5),color="black"),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.spacing.y=unit(1,"cm"),
        axis.ticks.length=unit(0.25,"cm"),
        plot.margin=margin(10,10,10,10),
        panel.background=element_blank(),
        panel.grid=element_blank(),
        panel.spacing=unit(0.5,"cm"),
        strip.text=element_text(color="black",size=16,face=2),
        strip.background=element_blank(),
        strip.placement="outside",
        strip.text.y=element_text(angle=180),
        aspect.ratio=1) +
  guides(fill=guide_legend(override.aes=list(size=6), byrow=TRUE)) +
  facet_wrap(~Model)

print(g1)

# ggsave("RunoffPlots_Measured-vs-Modeled.png", plot=g1, dpi=300)
ggsave("Figures/Figure_3.pdf", plot=g1,
       width=unit(8,"cm"), height=unit(7,"cm"), dpi=300)

################################################################################
# Model metrics

# R^2
for (mod in 1:nModels){
  # Average across basins for a particular model
  print((cor(ModelMedianTuolumne[mod,],TuolumneData$FutureRunoff_m)^2 +
           cor(ModelMedianMerced[mod,],MercedData$FutureRunoff_m)^2) / 2)
}

# NSE (kind of odd on seasonal timescales, but?)
AvgNSE = 0
for (mod in 1:nModels){
  # Average across all models and basins
  AvgNSE = AvgNSE + NSE(ModelMedianTuolumne[mod,],TuolumneData$FutureRunoff_m) / (nModels*2)
  AvgNSE = AvgNSE + NSE(ModelMedianMerced[mod,],MercedData$FutureRunoff_m) / (nModels*2)
  
  # Average across basins for a particular model
  print((NSE(ModelMedianTuolumne[mod,],TuolumneData$FutureRunoff_m) +
           NSE(ModelMedianMerced[mod,],MercedData$FutureRunoff_m)) / 2)
}
print(AvgNSE)

# Bias
lm(data.frame(Measured=c(TuolumneData$FutureRunoff_m,MercedData$FutureRunoff_m),
              Modeled=c(ModelMedianTuolumne[1,],ModelMedianMerced[1,])))

lm(data.frame(Measured=c(TuolumneData$FutureRunoff_m,MercedData$FutureRunoff_m),
              Modeled=c(ModelMedianTuolumne[2,],ModelMedianMerced[2,])))

# Mean absolute percent error
SelectedModel = 1

TuolumneResidualData = data.frame(Date=TuolumneData$Date,
                                  Month=TuolumneData$Month,
                                  Measured=TuolumneData$FutureRunoff_m,
                                  Modeled=ModelMedianTuolumne[SelectedModel,])
TuolumneResidualData$Residual = TuolumneResidualData$Modeled - TuolumneResidualData$Measured
TuolumneResidualData$PercentErr = TuolumneResidualData$Residual / TuolumneResidualData$Measured
TuolumneResidualData = TuolumneResidualData[c(1,7,21,29,41,44,46,50,53,57),]
mean(abs(TuolumneResidualData$PercentErr))

MercedResidualData = data.frame(Date=MercedData$Date,
                                Month=MercedData$Month,
                                Measured=MercedData$FutureRunoff_m,
                                Modeled=ModelMedianMerced[SelectedModel,])
MercedResidualData$Residual = MercedResidualData$Modeled - MercedResidualData$Measured
MercedResidualData$PercentErr = MercedResidualData$Residual / MercedResidualData$Measured
MercedResidualData = MercedResidualData[c(1,8,9,10,12,16,19),]
mean(abs(MercedResidualData$PercentErr))





