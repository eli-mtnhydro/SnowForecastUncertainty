# Make key result figures for Snow Drought Forecast Uncertainty project
# Eli Boardman copyright 2023

library(ggplot2)
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

dir = "C:/Users/board/Desktop/SnowDroughtForecastUncertainty/"
setwd(dir)

SkillSet = read.csv("SnowDroughtForecastUncertainty_ModelSkillResults.csv")[,-1]
head(SkillSet)

DroughtQuantiles = unique(SkillSet$Drought)
xSnow = unique(SkillSet$SnowFrac)

AvgSkill = read.csv("SnowDroughtForecastUncertainty_ModelSkillResults_AvgSkill.csv")[,-1]
DeltaSkill = read.csv("SnowDroughtForecastUncertainty_ModelSkillResults_DeltaSkill.csv")[,-1]

################################################################################
# Plot 1: Uncertainty partitioning averaged across model, basin, all conditions

JoinedSkillValues = (AvgSkill[(AvgSkill$Watershed=="Tuolumne" &
                                 AvgSkill$Drought == "AllConditions"),]$Skill +
                       AvgSkill[(AvgSkill$Watershed=="Merced" &
                                   AvgSkill$Drought == "AllConditions"),]$Skill) / 2

# 5x snow fraction, 3x modes
SnowFrac = rep(xSnow,3)
Mode = c(rep("StationForecast",length(xSnow)),
         rep("ASOForecast",length(xSnow)),
         rep("ASOBackcast",length(xSnow)))

SuperSkill = data.frame(SnowFrac,
                        Mode,
                        Skill=JoinedSkillValues)
SuperSkill

polylabels = c("Hydrological\nModel Error",
               "Post-Forecast\nPrecipitation",
               "Snowpack\nQuantification")

g1 = ggplot() + geom_polygon_pattern(mapping=aes(x=c(rev(xSnow),xSnow),
                                    y=c(rev(SuperSkill[SuperSkill$Mode=="ASOBackcast",]$Skill),
                                        rep(1,length(xSnow))),
                                    fill=polylabels[1]),color="black",
                                    pattern="crosshatch",pattern_angle=45,pattern_density=0.01,pattern_spacing=0.1,pattern_fill="black") +
  geom_polygon_pattern(mapping=aes(x=c(rev(xSnow),xSnow),
                           y=c(c(rev(SuperSkill[SuperSkill$Mode=="ASOForecast",]$Skill),
                                 SuperSkill[SuperSkill$Mode=="ASOBackcast",]$Skill)),
                           fill=polylabels[2]),color="black",
                       pattern="stripe",pattern_angle=0,pattern_density=0.01,pattern_spacing=0.05,pattern_fill="black") +
  geom_polygon_pattern(mapping=aes(x=c(rev(xSnow),xSnow),
                           y=c(c(rev(SuperSkill[SuperSkill$Mode=="StationForecast",]$Skill),
                                 SuperSkill[SuperSkill$Mode=="ASOForecast",]$Skill)),
                           fill=polylabels[3]),color="black",
                       pattern="none",pattern_angle=0,pattern_density=0.01,pattern_spacing=0.1,pattern_fill="black") +
  geom_line(mapping=aes(x=SuperSkill$SnowFrac,
                        y=SuperSkill$Skill,
                        group=SuperSkill$Mode,
                        color=SuperSkill$Mode),
            linewidth=4, lineend="round") +
  scale_fill_manual(values=c("lightskyblue1","lightgoldenrod1","gray90"),
                    breaks=polylabels) +
  scale_color_manual(values=c("deepskyblue1","goldenrod1","gray50"),
                     labels=c("ASO-Based Backcast","ASO-Based Forecast","Station Forecast")) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(min(xSnow),max(xSnow)),
                     breaks=seq(min(xSnow),max(xSnow),0.2),
                     labels=c("10%","30%","50%","70%","90%")) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1),
                     breaks=seq(0,1,0.25),
                     labels=c("0%","25%","50%","75%","100%"),
                     sec.axis=dup_axis(name=NULL,labels=NULL)) +
  labs(color="\nModel Mode",
       fill="Uncertainty Source",
       x="Fraction of Prior Precipitation Remaining as SWE",
       y="Model Skill") +
  theme_bw() +
  theme(axis.title.x=element_text(size=40,margin=margin(10,0,0,0)),
        axis.text.x=element_text(size=32,margin=margin(10,0,0,0),color="black"),
        axis.title.y=element_text(size=40,margin=margin(0,10,0,0)),
        axis.text.y=element_text(size=32,margin=margin(0,10,0,0),color="black"),
        legend.title=element_text(size=40),
        legend.text=element_text(size=32),
        legend.spacing.y=unit(1,"cm"),
        axis.ticks=element_line(color="black"),
        axis.ticks.length=unit(0.75,"cm"),
        plot.margin=margin(20,20,20,20),
        panel.background=element_blank(),
        panel.grid=element_blank(),
        legend.key=element_blank(),
        legend.key.width=unit(3,"cm"),
        legend.key.height=unit(1.5,"cm"),
        legend.box.margin=margin(0,0,0,10),
        panel.border=element_rect(color="black",fill=NA,linewidth=1)) +
  guides(fill=guide_legend(byrow=TRUE,order=1,
                           override.aes=list(pattern=c("crosshatch","stripe","none"),
                                             pattern_density=0.01,
                                             pattern_spacing=0.1,
                                             pattern_angle=c(45,0,0))),
         color=guide_legend(override.aes=list(linewidth=6), byrow=TRUE,order=2))

print(g1)

# ggsave("UncertaintySources_AvgModelModes.png", plot=g1, dpi=300)
ggsave("Figures/Figure_5.pdf", plot=g1,
       width=unit(18,"cm"), height=unit(12,"cm"), dpi=300)

################################################################################
# Plot 1: Uncertainty partitioning averaged across model, basin, all conditions

### Adding other panel with percent uncertainty partitioning ###

JoinedSkillValues = (AvgSkill[(AvgSkill$Watershed=="Tuolumne" &
                                 AvgSkill$Drought == "AllConditions"),]$Skill +
                       AvgSkill[(AvgSkill$Watershed=="Merced" &
                                   AvgSkill$Drought == "AllConditions"),]$Skill) / 2

# 5x snow fraction, 3x modes
SnowFrac = rep(xSnow,3)
Mode = c(rep("StationForecast",length(xSnow)),
         rep("ASOForecast",length(xSnow)),
         rep("ASOBackcast",length(xSnow)))

SuperSkill = data.frame(SnowFrac,
                        Mode,
                        Skill=JoinedSkillValues,
                        UncertaintyFrac=NA,
                        UncertaintyPos=NA)

for (x in unique(SnowFrac)){
  TotalUncertainty = 1 - SuperSkill$Skill[SuperSkill$SnowFrac==x &
                                            SuperSkill$Mode=="StationForecast"]
  
  # Snowpack attribution
  UncertaintyPart = SuperSkill$Skill[SuperSkill$SnowFrac==x &
                                                 SuperSkill$Mode=="ASOForecast"] -
    SuperSkill$Skill[SuperSkill$SnowFrac==x &
                       SuperSkill$Mode=="StationForecast"]
  SuperSkill$UncertaintyFrac[SuperSkill$SnowFrac==x & SuperSkill$Mode=="StationForecast"] = UncertaintyPart / TotalUncertainty
  
  # Precipitation attribution
  UncertaintyPart = SuperSkill$Skill[SuperSkill$SnowFrac==x &
                                       SuperSkill$Mode=="ASOBackcast"] -
    SuperSkill$Skill[SuperSkill$SnowFrac==x &
                       SuperSkill$Mode=="ASOForecast"]
  SuperSkill$UncertaintyFrac[SuperSkill$SnowFrac==x & SuperSkill$Mode=="ASOForecast"] = UncertaintyPart / TotalUncertainty
  
  # Model attribution
  UncertaintyPart = 1 - SuperSkill$Skill[SuperSkill$SnowFrac==x &
                                           SuperSkill$Mode=="ASOBackcast"]
  SuperSkill$UncertaintyFrac[SuperSkill$SnowFrac==x & SuperSkill$Mode=="ASOBackcast"] = UncertaintyPart / TotalUncertainty
}

# Convert from stackable fractions to absolute y-axis positions
SuperSkill$UncertaintyPos[SuperSkill$Mode=="ASOBackcast"] = 1 - SuperSkill$UncertaintyFrac[SuperSkill$Mode=="ASOBackcast"]
SuperSkill$UncertaintyPos[SuperSkill$Mode=="ASOForecast"] = SuperSkill$UncertaintyPos[SuperSkill$Mode=="ASOBackcast"] -
  SuperSkill$UncertaintyFrac[SuperSkill$Mode=="ASOForecast"]
SuperSkill$UncertaintyPos[SuperSkill$Mode=="StationForecast"] = 0

polylabels = c("Hydrological\nModel Error",
               "Post-Forecast\nPrecipitation",
               "Snowpack\nQuantification")

g1 = ggplot() + geom_polygon_pattern(mapping=aes(x=c(rev(xSnow),xSnow),
                                         y=c(rev(SuperSkill[SuperSkill$Mode=="ASOBackcast",]$UncertaintyPos),
                                             rep(1,length(xSnow))),
                                         fill=polylabels[1]),color="black",
                                     pattern="crosshatch",pattern_angle=45,pattern_density=0.01,pattern_spacing=0.1,pattern_fill="black") +
  geom_polygon_pattern(mapping=aes(x=c(rev(xSnow),xSnow),
                           y=c(c(rev(SuperSkill[SuperSkill$Mode=="ASOForecast",]$UncertaintyPos),
                                 SuperSkill[SuperSkill$Mode=="ASOBackcast",]$UncertaintyPos)),
                           fill=polylabels[2]),color="black",
                       pattern="stripe",pattern_angle=0,pattern_density=0.01,pattern_spacing=0.05,pattern_fill="black") +
  geom_polygon_pattern(mapping=aes(x=c(rev(xSnow),xSnow),
                           y=c(c(rev(SuperSkill[SuperSkill$Mode=="StationForecast",]$UncertaintyPos),
                                 SuperSkill[SuperSkill$Mode=="ASOForecast",]$UncertaintyPos)),
                           fill=polylabels[3]),color="black",
                       pattern="none",pattern_angle=0,pattern_density=0.01,pattern_spacing=0.1,pattern_fill="black") +
  scale_fill_manual(values=c("lightskyblue1","lightgoldenrod1","gray90"),
                    breaks=polylabels) +
  scale_color_manual(values=c("deepskyblue1","goldenrod1","gray50"),
                     labels=c("ASO-Based Backcast","ASO-Based Forecast","Station Forecast")) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(min(xSnow),max(xSnow)),
                     breaks=seq(min(xSnow),max(xSnow),0.2),
                     labels=c("10%","30%","50%","70%","90%")) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1),
                     breaks=seq(0,1,0.25),
                     labels=c("0%","25%","50%","75%","100%"),
                     sec.axis=dup_axis(name=NULL,labels=NULL)) +
  labs(fill="Uncertainty Source",
       x="Fraction of Prior Precipitation Remaining as SWE",
       y="Fraction of Total Uncertainty") +
  theme_bw() +
  theme(axis.title.x=element_text(size=40,margin=margin(10,0,0,0)),
        axis.text.x=element_text(size=32,margin=margin(10,0,0,0),color="black"),
        axis.title.y=element_text(size=40,margin=margin(0,10,0,0)),
        axis.text.y=element_text(size=32,margin=margin(0,10,0,0),color="black"),
        legend.title=element_text(size=40),
        legend.text=element_text(size=32),
        legend.spacing.y=unit(1,"cm"),
        axis.ticks=element_line(color="black"),
        axis.ticks.length=unit(0.75,"cm"),
        plot.margin=margin(20,20,20,20),
        panel.background=element_blank(),
        panel.grid=element_blank(),
        legend.key=element_blank(),
        legend.key.width=unit(3,"cm"),
        legend.key.height=unit(1.5,"cm"),
        legend.box.margin=margin(0,0,0,10),
        panel.border=element_rect(color="black",fill=NA,linewidth=1)) +
  guides(fill=guide_legend(byrow=TRUE,order=1,
                           override.aes=list(pattern=c("crosshatch","stripe","none"),
                                             pattern_density=0.01,
                                             pattern_spacing=0.1,
                                             pattern_angle=c(45,0,0))))

print(g1)

# ggsave("UncertaintySourcesRelative_AvgModelModes.png", plot=g1, dpi=300)
ggsave("Figures/SupplementaryFigure_1.pdf", plot=g1,
       width=unit(18,"cm"), height=unit(12,"cm"), dpi=300)

print(SuperSkill)

max(SuperSkill$UncertaintyFrac[SuperSkill$Mode=="StationForecast"]) -
  min(SuperSkill$UncertaintyFrac[SuperSkill$Mode=="StationForecast"])

max(SuperSkill$UncertaintyFrac[SuperSkill$Mode=="ASOForecast"]) -
  min(SuperSkill$UncertaintyFrac[SuperSkill$Mode=="ASOForecast"])

max(SuperSkill$UncertaintyFrac[SuperSkill$Mode=="ASOBackcast"]) -
  min(SuperSkill$UncertaintyFrac[SuperSkill$Mode=="ASOBackcast"])

max(1 - SuperSkill$Skill[SuperSkill$Mode=="StationForecast"])
min(1 - SuperSkill$Skill[SuperSkill$Mode=="StationForecast"])

0.7725471 / 0.3297827

################################################################################
# Plot 2: Uncertainty sources by model, basin, and snow drought condition

subset = SkillSet[SkillSet$Drought==1,] # All precip conditions, not drought
head(subset)

# Create data for residual uncertainty bars
SnowFrac = c(subset$SnowFrac,
             rep(xSnow,4*2))
Mode = c(subset$Mode,
         rep("AAAPerfect",length(xSnow)*4*2))
Watershed = c(subset$Watershed,
              c(rep("Tuolumne",length(xSnow)*4),
                rep("Merced",length(xSnow)*4)))
ModelNum = c(subset$ModelNum,
             rep(c(rep(1,length(xSnow)),
                   rep(2,length(xSnow)),
                   rep(3,length(xSnow)),
                   rep(4,length(xSnow))),2))
IncrementalSkill = c(subset$IncrementalSkill,
                     0.999-subset[subset$Mode=="ASOBackcast",]$Skill)

subset = data.frame(SnowFrac, Mode, ModelNum, Watershed, IncrementalSkill)
subset2 = subset[subset$Mode=="StationForecast",] # For model name labels

barwidth = 0.03
spacewidth = 0.01
xLabelOffset = (4 * barwidth + 3 * spacewidth) / 2 - barwidth / 2 # Center labels under all 4 bars

g = ggplot() + geom_bar_pattern(data=subset[subset$ModelNum==1,],
                        mapping=aes(x=SnowFrac, y=IncrementalSkill, fill=as.factor(Mode)),
                        stat="identity", position="stack", width=barwidth,
                        pattern=rep(c(rep("none",10),rep("stripe",5),rep("crosshatch",5)),2),
                        pattern_angle=rep(c(rep(0,15),rep(45,5)),2),pattern_density=0.01,pattern_spacing=0.03,pattern_fill="black") +
  geom_bar_pattern(data=subset[subset$ModelNum==2,],
           mapping=aes(x=SnowFrac+barwidth+spacewidth, y=IncrementalSkill, fill=as.factor(Mode)),
           stat="identity", position="stack", width=barwidth,
           pattern=rep(c(rep("none",10),rep("stripe",5),rep("crosshatch",5)),2),
           pattern_angle=rep(c(rep(0,15),rep(45,5)),2),pattern_density=0.01,pattern_spacing=0.03,pattern_fill="black") +
  geom_bar_pattern(data=subset[subset$ModelNum==3,],
           mapping=aes(x=SnowFrac+2*(barwidth+spacewidth), y=IncrementalSkill, fill=as.factor(Mode)),
           stat="identity", position="stack", width=barwidth,
           pattern=rep(c(rep("none",10),rep("stripe",5),rep("crosshatch",5)),2),
           pattern_angle=rep(c(rep(0,15),rep(45,5)),2),pattern_density=0.01,pattern_spacing=0.03,pattern_fill="black") +
  geom_bar_pattern(data=subset[subset$ModelNum==4,],
           mapping=aes(x=SnowFrac+3*(barwidth+spacewidth), y=IncrementalSkill, fill=as.factor(Mode)),
           stat="identity", position="stack", width=barwidth,
           pattern=rep(c(rep("none",10),rep("stripe",5),rep("crosshatch",5)),2),
           pattern_angle=rep(c(rep(0,15),rep(45,5)),2),pattern_density=0.01,pattern_spacing=0.03,pattern_fill="black") +
  geom_text(data=data.frame(xpos=subset2$SnowFrac+(barwidth+spacewidth)*(subset2$ModelNum-1),
                            ypos=subset2$IncrementalSkill-0.013,
                            lab=c(c(rep("1.",5)),
                                  c(rep("2.",5)),
                                  c(rep("3.",5)),
                                  c(rep("4.",5))),
                            Watershed=c(rep("Tuolumne",20),rep("Merced",20))),
            aes(x=xpos,y=ypos,label=lab,hjust=0.5,vjust=0.5,angle=0),
            color="black",size=5,show.legend=FALSE) +
  geom_point(data=data.frame(xpos=rep(0,4),ypos=rep(0,4),
                             lab=c("1. Constant Loss",
                                   "2. Constant Efficiency",
                                   "3. Linear Closed Water Balance",
                                   "4. Linear Open Water Balance")),
             aes(x=xpos,y=ypos,shape=as.factor(lab)),alpha=0,
             show.legend=TRUE) +
  labs(fill="Uncertainty Source",
       shape="\nRunoff Model",
       x="Fraction of Prior Precipitation Remaining as SWE",
       y="Model Skill") +
  scale_fill_manual(breaks=c("AAAPerfect",
                             "ASOBackcast",
                             "ASOForecast"),
                    values=c("AAAPerfect"="deepskyblue2",
                             "ASOBackcast"="goldenrod1",
                             "ASOForecast"="gray50",
                             "StationForecast"="transparent"),
                    labels=c("Hydrological\nModel Error",
                             "Post-Forecast\nPrecipitation",
                             "Snowpack\nQuantification")) +
  scale_x_continuous(limits=c(min(xSnow)-barwidth/2, max(xSnow)+barwidth*3.5+spacewidth*3),
                     breaks=(xSnow + xLabelOffset),
                     labels=c("10%","30%","50%","70%","90%")) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1),
                     breaks=seq(0,1,0.25),
                     labels=c("0%","25%","50%","75%","100%"),
                     sec.axis=dup_axis(name=NULL,labels=NULL)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=32,margin=margin(10,0,0,0)),
        axis.text.x=element_text(size=24,margin=margin(10,0,0,0),color="black"),
        axis.title.y=element_text(size=32,margin=margin(0,10,0,0)),
        axis.text.y=element_text(size=24,margin=margin(0,10,0,0),color="black"),
        legend.title=element_text(size=32),
        legend.text=element_text(size=24),
        legend.spacing.y=unit(0.5,"cm"),
        axis.ticks=element_line(color="black"),
        axis.ticks.length=unit(0.5,"cm"),
        plot.margin=margin(20,20,20,20),
        panel.background=element_blank(),
        panel.grid=element_blank(),
        legend.key=element_blank(),
        legend.key.width=unit(2,"cm"),
        legend.key.height=unit(1,"cm"),
        panel.spacing=unit(0.5,"cm"),
        strip.text=element_text(color="white",size=32),
        strip.background=element_rect(color="black",fill=NA,linewidth=1),
        panel.border=element_rect(color="black",fill=NA,linewidth=1)) +
  guides(fill=guide_legend(keywidth=unit(2,"cm"),byrow=TRUE,order=1,
                           override.aes=list(pattern=c("crosshatch","stripe","none"),
                                             pattern_density=0.01,
                                             pattern_spacing=0.02,
                                             pattern_angle=c(45,0,0))),
         shape=guide_legend(keywidth=unit(0,"cm"),byrow=TRUE,order=2)) +
  facet_grid(cols=vars(Watershed))

# Change basin name colors to match later convention
if (TRUE){
  fill_colors = c("aquamarine4","dodgerblue4")
  g2 = ggplot_gtable(ggplot_build(g))
  strips = which(startsWith(g2$layout$name,"strip"))
  k = 1
  for (i in strips){
    j = which(grepl("rect",g2$grobs[[i]]$grobs[[1]]$childrenOrder))
    g2$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill = fill_colors[k]
    k = k+1
  }
  
  # plot(g2)
}

# ggsave("UncertaintySources_ByWatershedAndModel.png", plot=g2, dpi=300)
ggsave("Figures/Figure_6.pdf", plot=g2,
       width=unit(18,"cm"), height=unit(9,"cm"), dpi=300)

# geom_text(data=data.frame(xpos=rep(0.4,4),
#                           ypos=rep(0.25,4)-seq(0,0.15,0.15/3),
#                           lab=c("1. Constant Loss",
#                                 "2. Constant Efficiency",
#                                 "3. Linear Closed Water Balance",
#                                 "4. Linear Open Water Balance"),
#                           Watershed=c(rep("Tuolumne",20),rep("Merced",20))),
#           aes(x=xpos,y=ypos,label=lab,hjust=0,vjust=0.5,angle=0),
#           color="black",size=6,show.legend=FALSE) +

################################################################################
# Plot 3: Drought effect on skill by watershed

barwidth = 0.075
spacewidth = 0.01
xLabelOffset = (2 * barwidth+spacewidth) / 2 - barwidth / 2 # Center labels under all bars

g3 = ggplot() + geom_bar_pattern(data=DeltaSkill[(DeltaSkill$Watershed=="Merced"),],
                    mapping=aes(x=SnowFrac, y=UncertaintyContribIncreasePct , fill=Mode),
                    stat="identity", position="dodge", width=barwidth,
                    pattern=c(rep("none",5),rep("stripe",5),rep("crosshatch",5)),
                    pattern_angle=c(rep(0,10),rep(45,5)),pattern_density=0.04,pattern_spacing=0.04,pattern_fill="black") +
  geom_bar_pattern(data=DeltaSkill[(DeltaSkill$Watershed=="Tuolumne"),],
           mapping=aes(x=SnowFrac+barwidth+spacewidth, y=UncertaintyContribIncreasePct , fill=Mode),
           stat="identity", position="dodge", width=barwidth,
           pattern=c(rep("none",5),rep("stripe",5),rep("crosshatch",5)),
           pattern_angle=c(rep(0,10),rep(45,5)),pattern_density=0.04,pattern_spacing=0.04,pattern_fill="black") +
  geom_text(aes(x=DeltaSkill[DeltaSkill$Mode=="StationForecast",]$SnowFrac +
                  (barwidth+spacewidth)*(DeltaSkill[DeltaSkill$Mode=="StationForecast",]$BarNum-1),
                y=c(rep(0,5),rep(-0.01,5)),
                label=c(rep("",6),rep("Merced",4))),
            hjust=1, vjust=0.5, size=10, color="aquamarine4", fontface="bold", angle=90) +
  geom_text(aes(x=DeltaSkill[DeltaSkill$Mode=="ASOForecast",]$SnowFrac +
                  (barwidth+spacewidth)*(DeltaSkill[DeltaSkill$Mode=="ASOForecast",]$BarNum-1),
                y=c(rep(0,5),rep(-0.01,5)),
                label=c(rep("",6),rep("Tuolumne",4))),
            hjust=1, vjust=0.5, size=10, color="dodgerblue4", fontface="bold", angle=90) +
  labs(fill="Change in\nUncertainty Source\nDuring Dry\nSnow Drought",
       x="Fraction of Prior Precipitation Remaining as SWE",
       y="Relative Increase") +
  scale_fill_manual(values=c("deepskyblue2","goldenrod1","gray50"),
                    labels=c("Hydrological\nModel Error",
                             "Post-Forecast\nPrecipitation",
                             "Snowpack\nQuantification")) +
  scale_x_continuous(limits=c(0.5-barwidth/2,0.9+1.5*barwidth+spacewidth),
                     breaks=xSnow+xLabelOffset,
                     labels=c("10%","30%","50%","70%","90%")) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(-0.15,0.5),
                     breaks=seq(0,0.5,0.1),
                     labels=c("Baseline",paste0("+",seq(10,50,10),"%")),
                     sec.axis=dup_axis(name=NULL,labels=NULL)) +
  theme_bw() +
  geom_hline(aes(yintercept=0)) +
  theme(axis.title.x=element_text(size=40,margin=margin(10,0,0,0)),
        axis.text.x=element_text(size=32,margin=margin(10,0,0,0),color="black"),
        axis.title.y=element_text(size=40,margin=margin(0,10,0,0),hjust=0.75),
        axis.text.y=element_text(size=32,margin=margin(0,10,0,0),color="black"),
        legend.title=element_text(size=40),
        legend.text=element_text(size=32),
        legend.spacing.y=unit(1,"cm"),
        axis.ticks=element_line(color="black"),
        axis.ticks.length=unit(0.75,"cm"),
        plot.margin=margin(20,20,20,20),
        panel.background=element_blank(),
        panel.grid=element_blank(),
        legend.key=element_blank(),
        legend.key.width=unit(3,"cm"),
        legend.key.height=unit(1.5,"cm"),
        panel.border=element_rect(color="black",fill=NA,linewidth=1)) +
  guides(fill=guide_legend(byrow=TRUE,
                           override.aes=list(pattern=c("crosshatch","stripe","none"),
                                             pattern_density=0.04,
                                             pattern_spacing=0.04,
                                             pattern_angle=c(45,0,0))))

print(g3)

# ggsave("Uncertainty_IncreaseDuringDrought.png", plot=g3, dpi=300)
ggsave("Figures/Figure_7.png", plot=g3,
       width=unit(18,"cm"), height=unit(12,"cm"), dpi=300)


