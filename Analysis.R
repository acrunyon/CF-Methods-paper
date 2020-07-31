# Read in BIBE data
#Create df that is 
# Changed units to metric 200414

#setwd(WD_plots)
library(ggplot2)
library(plyr)
library(lubridate)
library(dplyr)
library(forcats)
library(reshape2)
library(zoo)
library(gridExtra)
library(grid)
library(ggrepel)
library(ggpubr)

rm(list=ls())
setwd("C:/Users/achildress/DOI/NPS-CCRP-FC Science Adaptation - Documents/General/Climate Futures ms/Figs/")
load("BIBE-data.RData")
setwd("C:/Users/achildress/DOI/NPS-CCRP-FC Science Adaptation - Documents/General/Climate Futures ms/Figs/v3_200414/")

# Threshold percentages for defining Climate futures. Default low/high:  0.25, 0.75
CFLow = 0.25     
CFHigh = 0.75
CFs = c("Warm Wet", "Hot Wet", "Central", "Warm Dry", "Hot Dry") #Use spaces and characters only
Range = 30  #Number of years to summarize (should be at least 30)

CF_sub<-c("Warm Wet","Hot Dry")
GCM_sub<-c("inmcm4.rcp45", "IPSL-CM5A-MR.rcp85")

col.RCP2 = c("#3030FF","#FFBF00") #medium blue & light orange
colors2 <- c("#000080","#FA4646") #Navy & light red

### Create DFs for summarizing ###
ALL_HIST$Year<-format(ALL_HIST$Date,"%Y")
ALL_FUTURE$Year<-format(ALL_FUTURE$Date,"%Y")
ALL_HIST$TmeanCustom<-(ALL_HIST$TminCustom+ALL_HIST$TmaxCustom)/2
ALL_FUTURE$TmeanCustom<-(ALL_FUTURE$TminCustom+ALL_FUTURE$TmaxCustom)/2

## Imperial to metric for precip and tmean in ALL_HIST, ALL_FUTURE, and gridmet
ALL_HIST$Precip_mm<-ALL_HIST$PrecipCustom * 25.4
ALL_FUTURE$Precip_mm<-ALL_FUTURE$PrecipCustom * 25.4
gridmet$Precip_mm <-gridmet$PrecipCustom * 25.4

ALL_HIST$Tmean_C<-(ALL_HIST$TmeanCustom - 32)*(5/9)
ALL_FUTURE$Tmean_C<-(ALL_FUTURE$TmeanCustom - 32)*(5/9)
gridmet$Tmean_C<-(gridmet$TavgCustom - 32)*(5/9)


Baseline_all<-subset(ALL_HIST,Year<2000)
Baseline_all$per<-"Historical"
Future_all<-ALL_FUTURE
Future_all$per<-NA
Future_all$per[which(Future_all$Year>2024 & Future_all$Year <2056)]<-"2040"
Future_all$per[which(Future_all$Year>2044 & Future_all$Year <2076)]<-"2060"
Future_all$per[which(Future_all$Year>2064 & Future_all$Year <2096)]<-"2080"


####Set Average values for all four weather variables, using all baseline years and all climate models
BaseMeanPr = mean(Baseline_all$Precip_mm)
BaseMeanTmean = mean(Baseline_all$Tmean_C)

####Create Future/Baseline means data tables, with averages for all four weather variables, organized by GCM
Future_Means = data.frame(aggregate(cbind(Precip_mm, Tmean_C)
                                    ~ GCM+per, Future_all, mean,na.rm=F))   # , Future_all$Wind

Baseline_Means = data.frame(aggregate(cbind(Precip_mm, Tmean_C)~GCM+per, 
                                      Baseline_all, mean))    

#### add delta columns in order to classify CFs
Future_Means$DeltaPr = Future_Means$Precip_mm - Baseline_Means$Precip_mm
Future_Means$DeltaTmean = Future_Means$Tmean_C - Baseline_Means$Tmean_C

FM40<-subset(Future_Means,per=="2040")
FM60<-subset(Future_Means,per=="2060")
FM80<-subset(Future_Means,per=="2080")

#### 2040
#### Set limits for CF classification
Pr0 = as.numeric(quantile(FM40$DeltaPr, 0))
Pr25 = as.numeric(quantile(FM40$DeltaPr, CFLow))
PrAvg = as.numeric(mean(FM40$DeltaPr))
Pr75 = as.numeric(quantile(FM40$DeltaPr, CFHigh))
Pr100 = as.numeric(quantile(FM40$DeltaPr, 1))
Tavg0 = as.numeric(quantile(FM40$DeltaTmean, 0))
Tavg25 = as.numeric(quantile(FM40$DeltaTmean, CFLow)) 
Tavg = as.numeric(mean(FM40$DeltaTmean))

Tavg75 = as.numeric(quantile(FM40$DeltaTmean, CFHigh))
Tavg100 = as.numeric(quantile(FM40$DeltaTmean, 1))

#### Designate Climate Future
FM40$CF1 = as.numeric((FM40$DeltaTmean<Tavg & FM40$DeltaPr>Pr75) | FM40$DeltaTmean<Tavg25 & FM40$DeltaPr>PrAvg)
FM40$CF2 = as.numeric((FM40$DeltaTmean>Tavg & FM40$DeltaPr>Pr75) | FM40$DeltaTmean>Tavg75 & FM40$DeltaPr>PrAvg)
FM40$CF3 = as.numeric((FM40$DeltaTmean>Tavg25 & FM40$DeltaTmean<Tavg75) & (FM40$DeltaPr>Pr25 & FM40$DeltaPr<Pr75))
FM40$CF4 = as.numeric((FM40$DeltaTmean<Tavg & FM40$DeltaPr<Pr25) | FM40$DeltaTmean<Tavg25 & FM40$DeltaPr<PrAvg)
FM40$CF5 = as.numeric((FM40$DeltaTmean>Tavg & FM40$DeltaPr<Pr25) | FM40$DeltaTmean>Tavg75 & FM40$DeltaPr<PrAvg)

#Assign full name of climate future to new variable CF
FM40$CF[FM40$CF1==1]=CFs[1]
FM40$CF[FM40$CF2==1]=CFs[2]
FM40$CF[FM40$CF3==1]=CFs[3]
FM40$CF[FM40$CF4==1]=CFs[4]
FM40$CF[FM40$CF5==1]=CFs[5]
FM40$CF=as.factor(FM40$CF)
FM40$CF = factor(FM40$CF,ordered=TRUE,levels=CFs)

#     Remove extraneous Climate Future columns
FM40$CF1 = NULL
FM40$CF2 = NULL
FM40$CF3 = NULL
FM40$CF4 = NULL
FM40$CF5 = NULL

#     Add column with emissions scenario for each GCM run
FM40$emissions[grep("rcp85",FM40$GCM)] = "RCP 8.5"
FM40$emissions[grep("rcp45",FM40$GCM)] = "RCP 4.5"

#### 2060
#### Set limits for CF classification
Pr0 = as.numeric(quantile(FM60$DeltaPr, 0))
Pr25 = as.numeric(quantile(FM60$DeltaPr, CFLow))
PrAvg = as.numeric(mean(FM60$DeltaPr))
Pr75 = as.numeric(quantile(FM60$DeltaPr, CFHigh))
Pr100 = as.numeric(quantile(FM60$DeltaPr, 1))
Tavg0 = as.numeric(quantile(FM60$DeltaTmean, 0))
Tavg25 = as.numeric(quantile(FM60$DeltaTmean, CFLow)) 
Tavg = as.numeric(mean(FM60$DeltaTmean))

Tavg75 = as.numeric(quantile(FM60$DeltaTmean, CFHigh))
Tavg100 = as.numeric(quantile(FM60$DeltaTmean, 1))

#### Designate Climate Future
FM60$CF1 = as.numeric((FM60$DeltaTmean<Tavg & FM60$DeltaPr>Pr75) | FM60$DeltaTmean<Tavg25 & FM60$DeltaPr>PrAvg)
FM60$CF2 = as.numeric((FM60$DeltaTmean>Tavg & FM60$DeltaPr>Pr75) | FM60$DeltaTmean>Tavg75 & FM60$DeltaPr>PrAvg)
FM60$CF3 = as.numeric((FM60$DeltaTmean>Tavg25 & FM60$DeltaTmean<Tavg75) & (FM60$DeltaPr>Pr25 & FM60$DeltaPr<Pr75))
FM60$CF4 = as.numeric((FM60$DeltaTmean<Tavg & FM60$DeltaPr<Pr25) | FM60$DeltaTmean<Tavg25 & FM60$DeltaPr<PrAvg)
FM60$CF5 = as.numeric((FM60$DeltaTmean>Tavg & FM60$DeltaPr<Pr25) | FM60$DeltaTmean>Tavg75 & FM60$DeltaPr<PrAvg)

#Assign full name of climate future to new variable CF
FM60$CF[FM60$CF1==1]=CFs[1]
FM60$CF[FM60$CF2==1]=CFs[2]
FM60$CF[FM60$CF3==1]=CFs[3]
FM60$CF[FM60$CF4==1]=CFs[4]
FM60$CF[FM60$CF5==1]=CFs[5]
FM60$CF=as.factor(FM60$CF)
FM60$CF = factor(FM60$CF,ordered=TRUE,levels=CFs)

#     Remove extraneous Climate Future columns
FM60$CF1 = NULL
FM60$CF2 = NULL
FM60$CF3 = NULL
FM60$CF4 = NULL
FM60$CF5 = NULL

#     Add column with emissions scenario for each GCM run
FM60$emissions[grep("rcp85",FM60$GCM)] = "RCP 8.5"
FM60$emissions[grep("rcp45",FM60$GCM)] = "RCP 4.5"

#### 2080
#### Set limits for CF classification
Pr0 = as.numeric(quantile(FM80$DeltaPr, 0))
Pr25 = as.numeric(quantile(FM80$DeltaPr, CFLow))
PrAvg = as.numeric(mean(FM80$DeltaPr))
Pr75 = as.numeric(quantile(FM80$DeltaPr, CFHigh))
Pr100 = as.numeric(quantile(FM80$DeltaPr, 1))
Tavg0 = as.numeric(quantile(FM80$DeltaTmean, 0))
Tavg25 = as.numeric(quantile(FM80$DeltaTmean, CFLow)) 
Tavg = as.numeric(mean(FM80$DeltaTmean))

Tavg75 = as.numeric(quantile(FM80$DeltaTmean, CFHigh))
Tavg100 = as.numeric(quantile(FM80$DeltaTmean, 1))

#### Designate Climate Future
FM80$CF1 = as.numeric((FM80$DeltaTmean<Tavg & FM80$DeltaPr>Pr75) | FM80$DeltaTmean<Tavg25 & FM80$DeltaPr>PrAvg)
FM80$CF2 = as.numeric((FM80$DeltaTmean>Tavg & FM80$DeltaPr>Pr75) | FM80$DeltaTmean>Tavg75 & FM80$DeltaPr>PrAvg)
FM80$CF3 = as.numeric((FM80$DeltaTmean>Tavg25 & FM80$DeltaTmean<Tavg75) & (FM80$DeltaPr>Pr25 & FM80$DeltaPr<Pr75))
FM80$CF4 = as.numeric((FM80$DeltaTmean<Tavg & FM80$DeltaPr<Pr25) | FM80$DeltaTmean<Tavg25 & FM80$DeltaPr<PrAvg)
FM80$CF5 = as.numeric((FM80$DeltaTmean>Tavg & FM80$DeltaPr<Pr25) | FM80$DeltaTmean>Tavg75 & FM80$DeltaPr<PrAvg)

#Assign full name of climate future to new variable CF
FM80$CF[FM80$CF1==1]=CFs[1]
FM80$CF[FM80$CF2==1]=CFs[2]
FM80$CF[FM80$CF3==1]=CFs[3]
FM80$CF[FM80$CF4==1]=CFs[4]
FM80$CF[FM80$CF5==1]=CFs[5]
FM80$CF=as.factor(FM80$CF)
FM80$CF = factor(FM80$CF,ordered=TRUE,levels=CFs)

#     Remove extraneous Climate Future columns
FM80$CF1 = NULL
FM80$CF2 = NULL
FM80$CF3 = NULL
FM80$CF4 = NULL
FM80$CF5 = NULL

#     Add column with emissions scenario for each GCM run
FM80$emissions[grep("rcp85",FM80$GCM)] = "RCP 8.5"
FM80$emissions[grep("rcp45",FM80$GCM)] = "RCP 4.5"

####Add column with CF classification to Future_all/Baseline_all
Future_Means<-rbind(FM40,FM60,FM80)
CF_GCM = data.frame(GCM = Future_Means$GCM, CF = Future_Means$CF,per = Future_Means$per)

############################################################################## Analysis #################################################################################################

############### SCATTERPLOTS
# Figure 1 - 2040 individual scatterplot
# 2040
dualscatter = ggplot(FM40, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM40$DeltaTmean,.25), 
                               xmax=quantile(FM40$DeltaTmean,.75), 
                               ymin=quantile(FM40$DeltaPr,.25)*365, 
                               ymax=quantile(FM40$DeltaPr,.75)*365))
dualscatter + geom_text_repel(aes(label=GCM),size=5) + 
  geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=18), legend.title=element_text(size=16),
        legend.position = c(.9,1),legend.direction = "vertical",legend.text.align = 1,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ###
  labs(title =" ", 
       x = "Changes in annual average temperature (C)", # Change
       y = "Changes in annual average precipitation (mm)") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="EmissionsScenarios")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) #change

ggsave("2040-Scatter-.png", width = 15, height = 9)

# Figure 2 time-slice panel -- WITH MODEL NAMES
# 2040
dualscatter = ggplot(FM40, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM40$DeltaTmean,.25), 
                               xmax=quantile(FM40$DeltaTmean,.75), 
                               ymin=quantile(FM40$DeltaPr,.25)*365, 
                               ymax=quantile(FM40$DeltaPr,.75)*365))
A<- dualscatter  + geom_text_repel(aes(label=GCM)) + 
  geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) +
  theme(axis.text=element_text(size=18),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=18), legend.title=element_blank(),
        legend.position = c(.9,1),legend.direction = "vertical",legend.text.align = 1,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.2,1,0.2,1), "cm")) + 
  ###
  labs(title ="(a) 2040", 
       x = " ", # Change
       y = " ") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) + #change
  scale_x_continuous(limits = c(min(FM40$DeltaTmean), max(FM80$DeltaTmean))) +
  scale_y_continuous(limits = c((min(FM80$DeltaPr)*365)-.5, (max(FM80$DeltaPr)*365)+.5))
A

# 2060
dualscatter = ggplot(FM60, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM60$DeltaTmean,.25), 
                               xmax=quantile(FM60$DeltaTmean,.75), 
                               ymin=quantile(FM60$DeltaPr,.25)*365, 
                               ymax=quantile(FM60$DeltaPr,.75)*365))
B<-dualscatter  + geom_text_repel(aes(label=GCM)) + 
  geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) +
  theme(axis.text=element_text(size=18),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=18), legend.title=element_text(size=16),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.2,1,0.2,1), "cm")) + 
  ###
  labs(title ="(b) 2060", 
       x = " ", # Change
       y = "Annual precipitation change (mm)") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) +#change
  scale_x_continuous(limits = c(min(FM40$DeltaTmean), max(FM80$DeltaTmean))) +
  scale_y_continuous(limits = c((min(FM80$DeltaPr)*365)-.5, (max(FM80$DeltaPr)*365)+.5))


# 2080
dualscatter = ggplot(FM80, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM80$DeltaTmean,.25), 
                               xmax=quantile(FM80$DeltaTmean,.75), 
                               ymin=quantile(FM80$DeltaPr,.25)*365, 
                               ymax=quantile(FM80$DeltaPr,.75)*365))
C<-dualscatter  + geom_text_repel(aes(label=GCM)) + 
  geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=18), legend.title=element_text(size=16),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.2,1,0.2,1), "cm")) + 
  ###
  labs(title ="(c) 2080", 
       x = "Annual temperature change (C)", # Change
       y = " ") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions Scenarios ")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) + #change
  scale_x_continuous(limits = c(min(FM40$DeltaTmean), max(FM80$DeltaTmean))) +
  scale_y_continuous(limits = c((min(FM80$DeltaPr)*365)-.5, (max(FM80$DeltaPr)*365)+.5))


grid.arrange(A,B,C, nrow=3)

g <- arrangeGrob(A,B,C, nrow=3)
ggsave("Scatter_pannel.png", g,width = 10, height = 12)

# Figure 2 time-slice panel -- NO NAMES
# 2040
dualscatter = ggplot(FM40, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM40$DeltaTmean,.25), 
                               xmax=quantile(FM40$DeltaTmean,.75), 
                               ymin=quantile(FM40$DeltaPr,.25)*365, 
                               ymax=quantile(FM40$DeltaPr,.75)*365))
A<- dualscatter  + geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) +
  theme(axis.text=element_text(size=18),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=18), legend.title=element_blank(),
        legend.position = c(.9,1),legend.direction = "vertical",legend.text.align = 1,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.2,1,0.2,1), "cm")) + 
  ###
  labs(title ="(a) 2040", 
       x = " ", # Change
       y = " ") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) + #change
  scale_x_continuous(limits = c(min(FM40$DeltaTmean), max(FM80$DeltaTmean))) +
  scale_y_continuous(limits = c((min(FM80$DeltaPr)*365)-.5, (max(FM80$DeltaPr)*365)+.5))
A

# 2060
dualscatter = ggplot(FM60, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM60$DeltaTmean,.25), 
                               xmax=quantile(FM60$DeltaTmean,.75), 
                               ymin=quantile(FM60$DeltaPr,.25)*365, 
                               ymax=quantile(FM60$DeltaPr,.75)*365))
B<-dualscatter  + geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) +
  theme(axis.text=element_text(size=18),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=18), legend.title=element_text(size=16),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.2,1,0.2,1), "cm")) + 
  ###
  labs(title ="(b) 2060", 
       x = " ", # Change
       y = "Annual precipitation change (mm)") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) +#change
  scale_x_continuous(limits = c(min(FM40$DeltaTmean), max(FM80$DeltaTmean))) +
  scale_y_continuous(limits = c((min(FM80$DeltaPr)*365)-.5, (max(FM80$DeltaPr)*365)+.5))


# 2080
dualscatter = ggplot(FM80, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM80$DeltaTmean,.25), 
                               xmax=quantile(FM80$DeltaTmean,.75), 
                               ymin=quantile(FM80$DeltaPr,.25)*365, 
                               ymax=quantile(FM80$DeltaPr,.75)*365))
C<-dualscatter  + geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=18), legend.title=element_text(size=16),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0.2,1,0.2,1), "cm")) + 
  ###
  labs(title ="(c) 2080", 
       x = "Annual temperature change (C)", # Change
       y = " ") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions Scenarios ")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) + #change
  scale_x_continuous(limits = c(min(FM40$DeltaTmean), max(FM80$DeltaTmean))) +
  scale_y_continuous(limits = c((min(FM80$DeltaPr)*365)-.5, (max(FM80$DeltaPr)*365)+.5))


grid.arrange(A,B,C, nrow=3)

g <- arrangeGrob(A,B,C, nrow=3)
ggsave("Scatter_pannel-NO_NAMES.png", g,width = 10, height = 12)

# Figure X - 2080 individual scatterplots for 3 CF methods
# Projection names
dualscatter = ggplot(FM40, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(DeltaTmean,.25), 
                               xmax=quantile(DeltaTmean,.75), 
                               ymin=quantile(DeltaPr,.25)*365, 
                               ymax=quantile(DeltaPr,.75)*365))
A<- dualscatter + geom_text_repel(aes(label=GCM),size=3.5) +
  geom_point(colour="black",size=4) +
  theme(axis.text=element_text(size=18), axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ###
  labs(title ="(a) Projections")  #change
A

# RCPs
dualscatter = ggplot(FM40, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(DeltaTmean,.25), 
                               xmax=quantile(DeltaTmean,.75), 
                               ymin=quantile(DeltaPr,.25)*365, 
                               ymax=quantile(DeltaPr,.75)*365))
B<- dualscatter + 
  geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) + geom_point(aes(x=mean(DeltaTmean[which(emissions=="RCP 4.5")]), y=mean(365*DeltaPr[which(emissions=="RCP 4.5")])), shape=8, size=10, stroke=4, colour=col.RCP2[1]) +
  geom_point(aes(x=mean(DeltaTmean[which(emissions=="RCP 8.5")]), y=mean(365*DeltaPr[which(emissions=="RCP 8.5")])), shape=8, size=10, stroke=4, colour=col.RCP2[2]) +
  theme(axis.text=element_text(size=18), axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ###
  labs(title ="(b) RCP") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title=element_blank())) 

B

fm40<-FM40
'%notin%' <- Negate(`%in%`)
fm40$CF[which(fm40$CF %notin% CF_sub)]<-NA
dualscatter = ggplot(fm40, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(DeltaTmean,.25), 
                               xmax=quantile(DeltaTmean,.75), 
                               ymin=quantile(DeltaPr,.25)*365, 
                               ymax=quantile(DeltaPr,.75)*365))
C<- dualscatter + 
  geom_point(colour="black",size=4) +
  geom_point(colour="gray",size=3.5) + 
  geom_point(aes(color=CF),size=3.5) + 
  geom_point(aes(x=mean(DeltaTmean[which(CF=="Warm Wet")]), y=mean(365*DeltaPr[which(CF=="Warm Wet")])), shape=22, size=10, stroke=4, colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTmean[which(CF=="Hot Dry")]), y=mean(365*DeltaPr[which(CF=="Hot Dry")])), shape=22, size=10, stroke=4, colour=colors2[2]) +
  theme(axis.text=element_text(size=18),axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ###
  labs(title ="(c) Quadrant ") + #change
  scale_colour_manual(values=colors2)+
  guides(color=guide_legend(title=element_blank())) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) + #change
  # Annotate quadrants
  annotate("text",x=min(fm40$DeltaTmean),y=min(fm40$DeltaPr)*365,hjust=0,label="Warm Dry",size=6) +
  annotate("text",x=min(fm40$DeltaTmean),y=max(fm40$DeltaPr)*365,hjust=0,label="Warm Wet",size=6) +
  annotate("text",x=max(fm40$DeltaTmean),y=min(fm40$DeltaPr)*365,hjust=1,label="Hot Dry",size=6) +
  annotate("text",x=max(fm40$DeltaTmean),y=max(fm40$DeltaPr)*365,hjust=1,label="Warm Wet",size=6) 
C

fm40.2<-FM40
fm40.2$label<-""
fm40.2$label[which(fm40.2$GCM %in% GCM_sub[1])]<-GCM_sub[1]
fm40.2$label[which(fm40.2$GCM %in% GCM_sub[2])]<-GCM_sub[2]
dualscatter = ggplot(fm40.2, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(DeltaTmean,.25), 
                               xmax=quantile(DeltaTmean,.75), 
                               ymin=quantile(DeltaPr,.25)*365, 
                               ymax=quantile(DeltaPr,.75)*365))
D<- dualscatter + 
  geom_point(colour="black",size=4) +
  geom_point(color="grey",size=3.5) + 
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub[1])]), y=mean(365*DeltaPr[which(GCM==GCM_sub[1])])), shape=21, size=10, stroke=4, colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub[2])]), y=mean(365*DeltaPr[which(GCM==GCM_sub[2])])), shape=21, size=10, stroke=4, colour=colors2[2]) +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub[1])]), y=mean(365*DeltaPr[which(GCM==GCM_sub[1])])), shape=20, size=4,  colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub[2])]), y=mean(365*DeltaPr[which(GCM==GCM_sub[2])])), shape=20, size=4,  colour=colors2[2]) +
  geom_text_repel(aes(label=label),size=5,point.padding = .5) +
   theme(axis.text=element_text(size=18),
         axis.title = element_blank(),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ###
  labs(title ="(d) Individual projection ") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title=element_blank())) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) + #change
  # Annotate quadrants
  annotate("text",x=min(fm40$DeltaTmean),y=min(fm40$DeltaPr)*365,hjust=0,label="Warm Dry",size=6) +
  annotate("text",x=min(fm40$DeltaTmean),y=max(fm40$DeltaPr)*365,hjust=0,label="Warm Wet",size=6) +
  annotate("text",x=max(fm40$DeltaTmean),y=(min(fm40$DeltaPr)*365)+20,hjust=1,label="Hot Dry",size=6) +
  annotate("text",x=max(fm40$DeltaTmean),y=max(fm40$DeltaPr)*365,hjust=1,label="Warm Wet",size=6) 
D


g <- ggarrange(A,B,C,D, nrow=4,labels="AUTO")
grid.arrange(g,bottom=textGrob("Annual temperature change (C)",
                               gp=gpar(fontface="bold", col="black", fontsize=15)),
                               left=textGrob("Annual precipitation change (mm)", gp=gpar(fontface="bold", col="black", fontsize=15),rot=90))
ggsave("CF_methods-scatter_pannel.png", g,width = 8, height = 14)



########################## TIME SERIES


################### Create climate futures ###################################
Diffs_table<-as.data.frame(matrix(nrow=3,ncol=3))
row.names(Diffs_table)<-c("RCP","Quad","Indiv")
names(Diffs_table)<-c("2040","2060","2080")

#subset by Quadrant
FM_quad<-subset(Future_Means,CF %in% CF_sub)
FM_quad$CF<-factor(FM_quad$CF,levels=CF_sub)

#subset by GCM
FM_indiv<-subset(Future_Means,GCM %in% GCM_sub)
FM_indiv$GCM<-factor(FM_indiv$GCM,levels=GCM_sub)

## Aggregate
RCP.P<-aggregate(DeltaPr~emissions+per,Future_Means,mean)
RCP.T<-aggregate(DeltaTmean~emissions+per,Future_Means,mean)

Quad.P<-aggregate(DeltaPr~CF+per,FM_quad,mean)
Quad.T<-aggregate(DeltaTmean~CF+per,FM_quad,mean)

Indiv.P<-aggregate(DeltaPr~GCM+per,FM_indiv,mean)
Indiv.T<-aggregate(DeltaTmean~GCM+per,FM_indiv,mean)

 # Differences
R.P<-aggregate(Diff~per,with(RCP.P,data.frame(per=per,Diff=
                                                ifelse(emissions=="RCP 4.5",-1,1)*DeltaPr*365)),sum)
R.T<-aggregate(Diff~per,with(RCP.T,data.frame(per=per,Diff=
                                                 ifelse(emissions=="RCP 4.5",-1,1)*DeltaTmean)),sum)
Q.P<-aggregate(Diff~per,with(Quad.P,data.frame(per=per,Diff=
                                                ifelse(CF==CF_sub[1],-1,1)*DeltaPr*365)),sum)
Q.T<-aggregate(Diff~per,with(Quad.T,data.frame(per=per,Diff=
                                                ifelse(CF==CF_sub[1],-1,1)*DeltaTmean)),sum)
I.P<-aggregate(Diff~per,with(Indiv.P,data.frame(per=per,Diff=
                                                 ifelse(GCM==GCM_sub[1],-1,1)*DeltaPr*365)),sum)
I.T<-aggregate(Diff~per,with(Indiv.T,data.frame(per=per,Diff=
                                                 ifelse(GCM==GCM_sub[1],-1,1)*DeltaTmean)),sum)
 # Assign to tables
TDiff<-Diffs_table
PDiff<-Diffs_table

TDiff[1,]<-R.T$Diff
TDiff[2,]<-Q.T$Diff
TDiff[3,]<-I.T$Diff

PDiff[1,]<-R.P$Diff
PDiff[2,]<-Q.P$Diff
PDiff[3,]<-I.P$Diff

################################# TIME-SERIES PLOTS ############################################
 # Can do RCP and Individual model plots now, need to discuss quadrant plots

  ### By RCP
Future_all$emissions[grep("rcp85",Future_all$GCM)] = "RCP 8.5"
Future_all$emissions[grep("rcp45",Future_all$GCM)] = "RCP 4.5"
FA_R<-aggregate(cbind(Precip_mm,Tmean_C)~Year+emissions,Future_all,mean)
FA_R$Precip_mm<-FA_R$Precip_mm*365
FA_R<-subset(FA_R,Year>2019 & Year<2098)
FA_R$Year<-as.Date(FA_R$Year, format = "%Y")
FA_R$emissions<-factor(FA_R$emissions,levels = c("RCP 4.5", "RCP 8.5"))

  # RCP shading
RTemp<-aggregate(Tmean_C~Year,FA_R,min);colnames(RTemp)[2]<-"Tymin"
RTemp2<-aggregate(Tmean_C~Year,FA_R,max);colnames(RTemp2)[2]<-"Tymax"
RTemp<-merge(RTemp,RTemp2,by="Year");rm(RTemp2)
FA_R<-merge(FA_R,RTemp,by="Year",all.x = T)

RPr<-aggregate(Precip_mm~Year,FA_R,min);colnames(RPr)[2]<-"Pymin"
RPr2<-aggregate(Precip_mm~Year,FA_R,max);colnames(RPr2)[2]<-"Pymax"
RPr<-merge(RPr,RPr2,by="Year");rm(RPr2)
FA_R<-merge(FA_R,RPr,by="Year",all.x = T)

### By GCM
FA_GCM<-subset(Future_all,GCM %in% GCM_sub)
FA_I<-aggregate(cbind(Precip_mm,Tmean_C)~Year+GCM,FA_GCM,mean)
FA_I$Precip_mm<-FA_I$Precip_mm*365
FA_I<-subset(FA_I,Year>2019 & Year<2098)
FA_I$Year<-as.Date(FA_I$Year, format = "%Y")
FA_I$GCM<-factor(FA_I$GCM,levels = GCM_sub)

# GCM shading
ITemp<-aggregate(Tmean_C~Year,FA_I,min);colnames(ITemp)[2]<-"Tymin"
ITemp2<-aggregate(Tmean_C~Year,FA_I,max);colnames(ITemp2)[2]<-"Tymax"
ITemp<-merge(ITemp,ITemp2,by="Year");rm(ITemp2)
FA_I<-merge(FA_I,ITemp,by="Year",all.x = T)

IPr<-aggregate(Precip_mm~Year,FA_I,min);colnames(IPr)[2]<-"Pymin"
IPr2<-aggregate(Precip_mm~Year,FA_I,max);colnames(IPr2)[2]<-"Pymax"
IPr<-merge(IPr,IPr2,by="Year");rm(IPr2)
FA_I<-merge(FA_I,IPr,by="Year",all.x = T)
FA_I$GCM<-revalue(FA_I$GCM, c("IPSL-CM5A-LR.rcp45"="IPSL-CM5A-LR RCP 4.5", "IPSL-CM5A-MR.rcp85"="IPSL-CM5A-MR RCP 8.5"))

a<-ggplot(FA_R, aes(x=Year, y=Tmean_C, group=emissions, colour = emissions)) +
  geom_ribbon(aes(x=Year,ymin=Tymin,ymax=Tymax), fill="grey",colour="white") +
  geom_line(colour = "black",size=2.5, stat = "identity") +
  geom_line(size = 2, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(emissions), shape = factor(emissions))) +
  theme(axis.text=element_text(size=20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=24,vjust=1.0),
        plot.title=element_text(size=26,hjust=0),
        legend.text=element_text(size=18), legend.title=element_text(size=18),
        legend.position = c(.1,1), legend.direction = "vertical",legend.text.align = 0,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  labs(title = "(a)", 
       x = "Year", y = "Temperature (C)") +
  scale_color_manual(name="",values = col.RCP2) +
  scale_fill_manual(name="",values = col.RCP2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(min(FA_I$Tmean_C), max(FA_I$Tmean_C))) +
  guides(color=guide_legend(override.aes = list(size=7)))  
a

b<-ggplot(FA_R, aes(x=Year, y=Precip_mm, group=emissions, colour = emissions)) +
  geom_ribbon(aes(x=Year,ymin=Pymin,ymax=Pymax), fill="grey",colour="white") +
  geom_line(colour = "black",size=2.5, stat = "identity") +
  geom_line(size = 2, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(emissions), shape = factor(emissions))) +
  theme(axis.text=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=26,hjust=0),
        legend.text=element_text(size=24), legend.title=element_text(size=24),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  labs(title = "(b)", 
       x = "Year", y = "Precipitation (mm)") +
  scale_color_manual(name="",values = col.RCP2) +
  scale_fill_manual(name="",values = col.RCP2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(min(FA_I$Precip_mm), max(FA_I$Precip_mm))) +
  guides(color=guide_legend(override.aes = list(size=7)))
b

c<-ggplot(FA_I, aes(x=Year, y=Tmean_C, group=GCM, colour = GCM)) +
  geom_ribbon(aes(x=Year,ymin=Tymin,ymax=Tymax), fill="grey",colour="white") +
  geom_line(colour = "black",size=2.5, stat = "identity") +
  geom_line(size = 2, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(GCM), shape = factor(GCM))) +
  theme(axis.text=element_text(size=20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=25,hjust=0),
        legend.text=element_text(size=18), legend.title=element_text(size=18),
        legend.position = c(.22,1), legend.direction = "vertical",legend.text.align = 0,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  labs(title = "(c)", 
       x = "Year", y = " ") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(min(FA_I$Tmean_C), max(FA_I$Tmean_C))) +
  guides(color=guide_legend(override.aes = list(size=7))) 
c

d<-ggplot(FA_I, aes(x=Year, y=Precip_mm, group=GCM, colour = GCM)) +
  geom_ribbon(aes(x=Year,ymin=Pymin,ymax=Pymax), fill="grey",colour="white") +
  geom_line(colour = "black",size=2.5, stat = "identity") +
  geom_line(size = 2, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(GCM), shape = factor(GCM))) +
  theme(axis.text=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=26,hjust=0),
        legend.text=element_text(size=24), legend.title=element_text(size=24),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  labs(title = "(d)", 
       x = "Year", y = " ") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(min(FA_I$Precip_mm), max(FA_I$Precip_mm))) +
  guides(color=guide_legend(override.aes = list(size=7)))
d

grid.arrange(a,c,b,d, nrow=2,ncol=2)

g <- arrangeGrob(a,c,b,d, nrow=2,ncol=2)
ggsave("Long-term_panel.png", g,width = 18, height = 12)

############################################# DETO PLOTS ####################################
load("DETO.RData")

t2.annual$me.col<-"b"
t2.annual$me.col[which(t2.annual$GCM=="Climate Future 1")]<-"w"

a<-ggplot(t2.annual, aes(x=GCM, y=BegGrow, colour=GCM)) + 
  geom_boxplot(colour="black",aes(fill = factor(GCM)))+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(GCM),colour=factor(me.col)), position=position_jitter(0.2)) +
  theme(axis.text=element_text(size=20),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=24,vjust=1.0), 
        plot.title=element_text(size=26,hjust=0),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "(a)",
       x = " ", y = "Julian date", colour = "Climate Future") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors5)
dat<-ggplot_build(a)$data[[1]]
dat1<-dat[1,]
A<-a + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)

b<-ggplot(t2.annual, aes(x=GCM, y=HI_mod, colour=GCM)) + 
  geom_boxplot(colour="black",aes(fill = factor(GCM)))+ 
  geom_jitter(shape = 21, size = 5, aes(fill = factor(GCM),colour=factor(me.col)), position=position_jitter(0.2)) +
  theme(axis.text=element_text(size=20),
        axis.title.x=element_text(size=24,vjust=-0.2),
        axis.title.y=element_text(size=24,vjust=1.0), 
        plot.title=element_text(size=26,hjust=0),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(title = "(b)",
       x = " ", y = "Days", colour = "Climate Future") +
  scale_color_manual(name="",values = c("black","white"),guide=FALSE) +
  scale_fill_manual(name="",values = colors5)
dat<-ggplot_build(b)$data[[1]]
dat1<-dat[1,]
B<-b + geom_segment(data=dat1, aes(x=xmin, xend=xmax, 
                                y=middle, yend=middle), colour="grey", size=1)

grid.arrange(A,B,nrow=2,ncol=1)

g <- arrangeGrob(A,B, nrow=2,ncol=1)
ggsave("DETO-plots.png", g,width = 18, height = 12)
