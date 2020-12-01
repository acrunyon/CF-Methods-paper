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
library(stringr)

rm(list=ls())
setwd("C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/Climate Futures ms/Figs/")
load("BIBE-data.RData")
missing_GCMs<-read.csv("BIBE_missing_models.csv")
missing_GCMs$GCM <-str_sub(missing_GCMs$GCM, end=-7)
setwd("C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/Climate Futures ms/Figs/submission/")

# Threshold percentages for defining Climate futures. Default low/high:  0.25, 0.75
CFLow = 0.25     
CFHigh = 0.75
CFs = c("Warm Wet", "Hot Wet", "Central", "Warm Dry", "Hot Dry") #Use spaces and characters only
Range = 30  #Number of years to summarize (should be at least 30)

CF_sub<-c("Warm Wet","Hot Dry")
GCM_sub2040<-c("CNRM-CM5.rcp45", "IPSL-CM5A-MR.rcp85")
GCM_sub2060<-c("GFDL-ESM2M.rcp45", "IPSL-CM5A-MR.rcp85")
GCM_sub2080<-c("inmcm4.rcp45", "IPSL-CM5A-MR.rcp85")

col.RCP2 = c("#3030FF","#FFBF00") #medium blue & light orange
colors2 <- c("#000080","#FA4646") #Navy & light red

### Create DFs for summarizing ###
ALL_HIST$Year<-format(ALL_HIST$Date,"%Y")
ALL_FUTURE$Year<-format(ALL_FUTURE$Date,"%Y")

# Add missing GCMs to hist and future dfs
missing_GCMs$Date<-as.POSIXlt(missing_GCMs$Date, format="%Y-%m-%d")
missing_GCMs$Year<-format(missing_GCMs$Date,"%Y")
ALL_HIST<-ALL_HIST[names(ALL_FUTURE)] #reoorder to match ALL_FUTURE and missing_GCMs dfs

ALL_HIST<-rbind(ALL_HIST,missing_GCMs[which(missing_GCMs$Year<=2005),])
MG1<-missing_GCMs[which(missing_GCMs$Year>2005),]
MG2<-MG1
MG1$GCM<-paste(MG1$GCM,"rcp45",sep=".");MG2$GCM<-paste(MG1$GCM,"rcp85",sep=".")
ALL_FUTURE<-rbind(ALL_FUTURE,MG1,MG2);rm(MG1,MG2)

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
Future_Means$DeltaPr = Future_Means$Precip_mm - BaseMeanPr
Future_Means$DeltaTmean = Future_Means$Tmean_C - BaseMeanTmean

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
# Figure 1 - 2080 individual scatterplots for 3 CF methods
# Projection names
dualscatter = ggplot(FM40, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(DeltaTmean,.25), 
                               xmax=quantile(DeltaTmean,.75), 
                               ymin=quantile(DeltaPr,.25)*365, 
                               ymax=quantile(DeltaPr,.75)*365))
A<- dualscatter + geom_text_repel(aes(label=GCM),size=1.75) +
  geom_point(colour="black",size=2) +
  theme(axis.text=element_text(size=9), axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.title=element_text(size=9,face="bold",vjust=2,hjust=0),
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
  geom_point(colour="black",size=2) +
  geom_point(aes(color=emissions),size=1.75) + geom_point(aes(x=mean(DeltaTmean[which(emissions=="RCP 4.5")]), y=mean(365*DeltaPr[which(emissions=="RCP 4.5")])), shape=8, size=5, stroke=2, colour=col.RCP2[1]) +
  geom_point(aes(x=mean(DeltaTmean[which(emissions=="RCP 8.5")]), y=mean(365*DeltaPr[which(emissions=="RCP 8.5")])), shape=8, size=5, stroke=2, colour=col.RCP2[2]) +
  theme(axis.text=element_text(size=9), axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.title=element_text(size=9,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=9), legend.title=element_text(size=8),
        legend.position = c(.9,1),legend.direction = "vertical",legend.text.align = 1,
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
  geom_point(colour="black",size=2) +
  geom_point(colour="gray",size=1.75) + 
  geom_point(aes(color=CF),size=1.75) + 
  geom_point(aes(x=mean(DeltaTmean[which(CF=="Warm Wet")]), y=mean(365*DeltaPr[which(CF=="Warm Wet")])), shape=22, size=5, stroke=2, colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTmean[which(CF=="Hot Dry")]), y=mean(365*DeltaPr[which(CF=="Hot Dry")])), shape=22, size=5, stroke=2, colour=colors2[2]) +
  theme(axis.text=element_text(size=9),axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.title=element_text(size=9,face="bold",vjust=2,hjust=0),
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
  annotate("text",x=min(fm40$DeltaTmean),y=min(fm40$DeltaPr)*365,hjust=0,label="Warm Dry",size=3) +
  annotate("text",x=min(fm40$DeltaTmean),y=max(fm40$DeltaPr)*365,hjust=0,label="Warm Wet",size=3) +
  annotate("text",x=max(fm40$DeltaTmean),y=min(fm40$DeltaPr)*365,hjust=1,label="Hot Dry",size=3) +
  annotate("text",x=max(fm40$DeltaTmean),y=max(fm40$DeltaPr)*365,hjust=1,label="Hot Wet",size=3) 
C

fm40.2<-FM40
fm40.2$label<-""
fm40.2$label[which(fm40.2$GCM %in% GCM_sub2040[1])]<-GCM_sub2040[1]
fm40.2$label[which(fm40.2$GCM %in% GCM_sub2040[2])]<-GCM_sub2040[2]
dualscatter = ggplot(fm40.2, aes(DeltaTmean, DeltaPr*365, 
                                 xmin=quantile(DeltaTmean,.25), 
                                 xmax=quantile(DeltaTmean,.75), 
                                 ymin=quantile(DeltaPr,.25)*365, 
                                 ymax=quantile(DeltaPr,.75)*365))
D<- dualscatter + 
  geom_point(colour="black",size=2) +
  geom_point(color="grey",size=1.75) + 
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub2040[1])]), y=mean(365*DeltaPr[which(GCM==GCM_sub2040[1])])), shape=21, size=5, stroke=2, colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub2040[2])]), y=mean(365*DeltaPr[which(GCM==GCM_sub2040[2])])), shape=21, size=5, stroke=2, colour=colors2[2]) +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub2040[1])]), y=mean(365*DeltaPr[which(GCM==GCM_sub2040[1])])), shape=20, size=2,  colour=colors2[1]) +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub2040[2])]), y=mean(365*DeltaPr[which(GCM==GCM_sub2040[2])])), shape=20, size=2,  colour=colors2[2]) +
  geom_text_repel(aes(label=label),size=2.5,point.padding = .25) +
  theme(axis.text=element_text(size=9),
        axis.title = element_blank(),
        plot.title=element_text(size=9,face="bold",vjust=2,hjust=0),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ###
  labs(title ="(d) Individual projection ") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title=element_blank())) 
  # geom_rect(color = "black", alpha=0) + 
  # geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  # geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) + #change
  # # Annotate quadrants
  # annotate("text",x=min(fm40$DeltaTmean),y=min(fm40$DeltaPr)*365,hjust=0,label="Warm Dry",size=6) +
  # annotate("text",x=min(fm40$DeltaTmean),y=max(fm40$DeltaPr)*365,hjust=0,label="Warm Wet",size=6) +
  # annotate("text",x=max(fm40$DeltaTmean),y=(min(fm40$DeltaPr)*365)+20,hjust=1,label="Hot Dry",size=6) +
  # annotate("text",x=max(fm40$DeltaTmean),y=max(fm40$DeltaPr)*365,hjust=1,label="Hot Wet",size=6) 
D


g <- ggarrange(A,B,C,D, nrow=4)
G<-grid.arrange(g,bottom=textGrob("Annual temperature change (˚C)",
                               gp=gpar(fontface="bold", col="black", fontsize=9)),
             left=textGrob("Annual precipitation change (mm)", gp=gpar(fontface="bold", col="black", fontsize=9),rot=90))
ggsave("Fig2.eps", G,width = 4, height = 7)
ggsave("Fig2.jpg", G,width = 4, height = 7)


# Figure 2 time-slice panel -- NO NAMES
# 2040
dualscatter = ggplot(FM40, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM40$DeltaTmean,.25), 
                               xmax=quantile(FM40$DeltaTmean,.75), 
                               ymin=quantile(FM40$DeltaPr,.25)*365, 
                               ymax=quantile(FM40$DeltaPr,.75)*365))
A<- dualscatter  + geom_point(colour="black",size=2) +
  geom_point(aes(color=emissions),size=1.75) +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub2040[1])]), y=mean(365*DeltaPr[which(GCM==GCM_sub2040[1])])), shape=21, size=5, stroke=2, colour="black") +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub2040[2])]), y=mean(365*DeltaPr[which(GCM==GCM_sub2040[2])])), shape=21, size=5, stroke=2, colour="black") +
  theme(axis.text=element_text(size=9),
        axis.title = element_blank(),
        plot.title=element_text(size=9,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=9), legend.title=element_blank(),
        legend.position = c(.9,1),legend.direction = "vertical",legend.text.align = 1,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
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
  scale_y_continuous(limits = c((min(FM80$DeltaPr)*365)-.5, (max(FM80$DeltaPr)*365)+.5)) +
  annotate("text",x=min(FM40$DeltaTmean),y=max(FM80$DeltaPr)*365-10,hjust=0,label="Warm \nWet",size=3) +
  annotate("text",x=max(FM80$DeltaTmean),y=(min(FM80$DeltaPr)*365),hjust=1,label="Hot Dry",size=3)  
A

# 2060
dualscatter = ggplot(FM60, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM60$DeltaTmean,.25), 
                               xmax=quantile(FM60$DeltaTmean,.75), 
                               ymin=quantile(FM60$DeltaPr,.25)*365, 
                               ymax=quantile(FM60$DeltaPr,.75)*365))
B<-dualscatter  + geom_point(colour="black",size=2) +
  geom_point(aes(color=emissions),size=1.75) +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub2060[1])]), y=mean(365*DeltaPr[which(GCM==GCM_sub2060[1])])), shape=21, size=5, stroke=2, colour="black") +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub2060[2])]), y=mean(365*DeltaPr[which(GCM==GCM_sub2060[2])])), shape=21, size=5, stroke=2, colour="black") +
  theme(axis.text=element_text(size=9),
        axis.title = element_blank(),
        plot.title=element_text(size=9,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=9), legend.title=element_text(size=8),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        # plot.margin = unit(c(0.2,1,0.2,1), "cm")) + 
  ###
  labs(title ="(b) 2060", 
       x = " ", # Change
       y = " ") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) +#change
  scale_x_continuous(limits = c(min(FM40$DeltaTmean), max(FM80$DeltaTmean))) +
  scale_y_continuous(limits = c((min(FM80$DeltaPr)*365)-.5, (max(FM80$DeltaPr)*365)+.5)) +
  annotate("text",x=min(FM40$DeltaTmean),y=max(FM80$DeltaPr)*365,hjust=0,label="Warm Wet",size=3) +
  annotate("text",x=max(FM80$DeltaTmean),y=(min(FM80$DeltaPr)*365),hjust=1,label="Hot Dry",size=3) 


# 2080
dualscatter = ggplot(FM80, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM80$DeltaTmean,.25), 
                               xmax=quantile(FM80$DeltaTmean,.75), 
                               ymin=quantile(FM80$DeltaPr,.25)*365, 
                               ymax=quantile(FM80$DeltaPr,.75)*365))
C<-dualscatter  + geom_point(colour="black",size=2) +
  geom_point(aes(color=emissions),size=1.75) +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub2080[1])]), y=mean(365*DeltaPr[which(GCM==GCM_sub2080[1])])), shape=21, size=5, stroke=2, colour="black") +
  geom_point(aes(x=mean(DeltaTmean[which(GCM==GCM_sub2080[2])]), y=mean(365*DeltaPr[which(GCM==GCM_sub2080[2])])), shape=21, size=5, stroke=2, colour="black") +
  theme(axis.text=element_text(size=9),
        axis.title = element_blank(),
        plot.title=element_text(size=9,face="bold",vjust=2,hjust=0),
        legend.text=element_text(size=9), legend.title=element_text(size=8),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ###
  labs(title ="(c) 2080", 
       x = " ", # Change
       y = " ") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions Scenarios ")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) + #change
  scale_x_continuous(limits = c(min(FM40$DeltaTmean), max(FM80$DeltaTmean))) +
  scale_y_continuous(limits = c((min(FM80$DeltaPr)*365)-.5, (max(FM80$DeltaPr)*365)+5)) +
  annotate("text",x=min(FM40$DeltaTmean),y=(max(FM80$DeltaPr)*365)-25,hjust=0,label="Warm Wet",size=3) +
  annotate("text",x=max(FM80$DeltaTmean),y=(min(FM80$DeltaPr)*365)+25,hjust=1,label="Hot Dry",size=3) 
C

g <- arrangeGrob(A,B,C, nrow=3)
G<-grid.arrange(g,bottom=textGrob("Annual temperature change (˚C)",
                                  gp=gpar(fontface="bold", col="black", fontsize=9)),
                left=textGrob("Annual precipitation change (mm)", gp=gpar(fontface="bold", col="black", fontsize=9),rot=90))
ggsave("Fig3.eps", G,width = 4, height = 7)
ggsave("Fig3.jpg", G,width = 4, height = 7)

########################## TIME SERIES


################### Create climate futures ###################################
Diffs_table<-as.data.frame(matrix(nrow=9,ncol=3))

row.names(Diffs_table)<-c("RCP8.5","RCP4.5","RCP_range",CF_sub[2],CF_sub[1],"Quad_range",GCM_sub2040[2],"WW","Indiv_range")
names(Diffs_table)<-c("2040","2060","2080")

#subset by Quadrant
FM_quad<-subset(Future_Means,CF %in% CF_sub)
FM_quad$CF<-factor(FM_quad$CF,levels=CF_sub)

#subset by GCM
FM_indiv2040<-subset(FM40,GCM %in% GCM_sub2040)
FM_indiv2060<-subset(FM60,GCM %in% GCM_sub2060)
FM_indiv2080<-subset(FM80,GCM %in% GCM_sub2080)

FM_indiv<-rbind(FM_indiv2040,FM_indiv2060,FM_indiv2080)
FM_indiv$GCM[which(FM_indiv$GCM != GCM_sub2040[2])]<-"WW"
FM_indiv$GCM<-factor(FM_indiv$GCM,levels=c("WW",GCM_sub2040[2]))

## Aggregate
RCP.P<-aggregate(Precip_mm~emissions+per,Future_Means,mean)
RCP.T<-aggregate(Tmean_C~emissions+per,Future_Means,mean)

Quad.P<-aggregate(Precip_mm~CF+per,FM_quad,mean)
Quad.T<-aggregate(Tmean_C~CF+per,FM_quad,mean)

Indiv.P<-aggregate(Precip_mm~GCM+per,FM_indiv,mean)
Indiv.T<-aggregate(Tmean_C~GCM+per,FM_indiv,mean)

# Reformat tables
rcp_t<-dcast(RCP.T,emissions~per)
rownames(rcp_t)<-rcp_t[,1];rcp_t<-rcp_t[,-1]
quad_t<-dcast(Quad.T,CF~per)
rownames(quad_t)<-quad_t[,1];quad_t<-quad_t[,-1]
indiv_t<-dcast(Indiv.T,GCM~per)
rownames(indiv_t)<-indiv_t[,1];indiv_t<-indiv_t[,-1]
# indiv_t;GCM_sub #make sure ordering correct

rcp_p<-dcast(RCP.P,emissions~per)
rownames(rcp_p)<-rcp_p[,1];rcp_p<-rcp_p[,-1]
quad_p<-dcast(Quad.P,CF~per)
rownames(quad_p)<-quad_p[,1];quad_p<-quad_p[,-1]
indiv_p<-dcast(Indiv.P,GCM~per)
rownames(indiv_p)<-indiv_p[,1];indiv_p<-indiv_p[,-1]

# Assign to tables
TDiff<-Diffs_table
PDiff<-Diffs_table

TDiff[1,]<-rcp_t[2,]
TDiff[2,]<-rcp_t[1,]
TDiff[4,]<-quad_t[2,]
TDiff[5,]<-quad_t[1,]
TDiff[7,]<-indiv_t[2,]
TDiff[8,]<-indiv_t[1,]
TDiff<-round(TDiff,digits=2) #rounding in middle avoids rounding errors after subtracting
TDiff[3,]<-abs(TDiff[1,]-TDiff[2,])
TDiff[6,]<-abs(TDiff[4,]-TDiff[5,])
TDiff[9,]<-abs(TDiff[7,]-TDiff[8,])

PDiff[1,]<-rcp_p[2,]
PDiff[2,]<-rcp_p[1,]
PDiff[4,]<-quad_p[2,]
PDiff[5,]<-quad_p[1,]
PDiff[7,]<-indiv_p[2,]
PDiff[8,]<-indiv_p[1,]
PDiff<-PDiff*365
PDiff<-round(PDiff,digits=2) #rounding in middle avoids rounding errors after subtracting
PDiff[3,]<-abs(PDiff[1,]-PDiff[2,])
PDiff[6,]<-abs(PDiff[4,]-PDiff[5,])
PDiff[9,]<-abs(PDiff[7,]-PDiff[8,])

write.csv(TDiff,"T_DiffsTable.csv",row.names = T)
write.csv(PDiff,"P_DiffsTable.csv",row.names = T)

# Hist values
HistT<-mean(Baseline_Means$Tmean_C)
HistP<-mean(Baseline_Means$Precip_mm)*365
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
FA_GCM<-subset(Future_all,GCM %in% GCM_sub2080)
FA_I<-aggregate(cbind(Precip_mm,Tmean_C)~Year+GCM,FA_GCM,mean)
FA_I$Precip_mm<-FA_I$Precip_mm*365
FA_I<-subset(FA_I,Year>2019 & Year<2098)
FA_I$Year<-as.Date(FA_I$Year, format = "%Y")
FA_I$GCM<-factor(FA_I$GCM,levels = GCM_sub2080)

# GCM shading
ITemp<-aggregate(Tmean_C~Year,FA_I,min);colnames(ITemp)[2]<-"Tymin"
ITemp2<-aggregate(Tmean_C~Year,FA_I,max);colnames(ITemp2)[2]<-"Tymax"
ITemp<-merge(ITemp,ITemp2,by="Year");rm(ITemp2)
FA_I<-merge(FA_I,ITemp,by="Year",all.x = T)

IPr<-aggregate(Precip_mm~Year,FA_I,min);colnames(IPr)[2]<-"Pymin"
IPr2<-aggregate(Precip_mm~Year,FA_I,max);colnames(IPr2)[2]<-"Pymax"
IPr<-merge(IPr,IPr2,by="Year");rm(IPr2)
FA_I<-merge(FA_I,IPr,by="Year",all.x = T)
FA_I$GCM<-revalue(FA_I$GCM, c("inmcm4.rcp45"="inmcm4 RCP 4.5", "IPSL-CM5A-MR.rcp85"="IPSL-CM5A-MR RCP 8.5"))

a<-ggplot(FA_R, aes(x=Year, y=Tmean_C, group=emissions, colour = emissions)) +
  geom_ribbon(aes(x=Year,ymin=Tymin,ymax=Tymax), fill="grey",colour="white") +
  geom_line(colour = "black",size=1.25, stat = "identity") +
  geom_line(size = 1, stat = "identity") +
  geom_point(colour= "black", size=2, aes(fill = factor(emissions), shape = factor(emissions))) +
  theme(axis.text=element_text(size=10),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=12,vjust=1.0),
        plot.title=element_text(size=13,hjust=0),
        legend.text=element_text(size=9), legend.title=element_text(size=9),
        legend.position = c(.2,1), legend.direction = "vertical",legend.text.align = 0,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  labs(title = "(a)", 
       x = "Year", y = "Temperature (˚C)") +
  scale_color_manual(name="",values = col.RCP2) +
  scale_fill_manual(name="",values = col.RCP2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(min(FA_I$Tmean_C),max(FA_I$Tmean_C))) +
  guides(color=guide_legend(override.aes = list(size=3.5)))  
a

c<-ggplot(FA_R, aes(x=Year, y=Precip_mm, group=emissions, colour = emissions)) +
  geom_ribbon(aes(x=Year,ymin=Pymin,ymax=Pymax), fill="grey",colour="white") +
  geom_line(colour = "black",size=1.25, stat = "identity") +
  geom_line(size = 1, stat = "identity") +
  geom_point(colour= "black", size=2, aes(fill = factor(emissions), shape = factor(emissions))) +
  theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=10,vjust=-0.2),
        axis.title.y=element_text(size=10,vjust=1.0),
        plot.title=element_text(size=13,hjust=0),
        legend.text=element_text(size=12), legend.title=element_text(size=12),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  labs(title = "(c)", 
       x = "Year", y = "Precipitation (mm)") +
  scale_color_manual(name="",values = col.RCP2) +
  scale_fill_manual(name="",values = col.RCP2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(min(FA_I$Precip_mm), max(FA_I$Precip_mm))) +
  guides(color=guide_legend(override.aes = list(size=3.5)))
c

b<-ggplot(FA_I, aes(x=Year, y=Tmean_C, group=GCM, colour = GCM)) +
  geom_ribbon(aes(x=Year,ymin=Tymin,ymax=Tymax), fill="grey",colour="white") +
  geom_line(colour = "black",size=1.25, stat = "identity") +
  geom_line(size = 1, stat = "identity") +
  geom_point(colour= "black", size=2, aes(fill = factor(GCM), shape = factor(GCM))) +
  theme(axis.text=element_text(size=10),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=10,vjust=1.0),
        plot.title=element_text(size=13,hjust=0),
        legend.text=element_text(size=9), legend.title=element_text(size=9),
        legend.position = c(.35,1), legend.direction = "vertical",legend.text.align = 0,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  labs(title = "(b)", 
       x = "Year", y = " ") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(min(FA_I$Tmean_C), max(FA_I$Tmean_C))) +
  guides(color=guide_legend(override.aes = list(size=3.5))) 
b
d<-ggplot(FA_I, aes(x=Year, y=Precip_mm, group=GCM, colour = GCM)) +
  geom_ribbon(aes(x=Year,ymin=Pymin,ymax=Pymax), fill="grey",colour="white") +
  geom_line(colour = "black",size=1.25, stat = "identity") +
  geom_line(size = 1, stat = "identity") +
  geom_point(colour= "black", size=2, aes(fill = factor(GCM), shape = factor(GCM))) +
  theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=10,vjust=-0.2),
        axis.title.y=element_text(size=10,vjust=1.0),
        plot.title=element_text(size=13,hjust=0),
        legend.text=element_text(size=12), legend.title=element_text(size=12),
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
  guides(color=guide_legend(override.aes = list(size=3.5)))
d

grid.arrange(a,b,c,d, nrow=2,ncol=2)

g <- arrangeGrob(a,b,c,d, nrow=2,ncol=2)
g$vp = grid::viewport(height=0.9, width=0.9) 
ggsave("Fig4.eps", g,width = 9, height = 7)
ggsave("Fig4.jpg", g,width = 9, height = 7)

############### FIGURE 5 -- BIBE FLOW ##############################
# model data y = 0.8134x + 10.529
xint = 0.8134
yint = 10.529

# Copy ALL_FUTURE
AH<-ALL_HIST
A<-AH
AH$GCM<-paste(AH$GCM,"rcp45",sep=".")
A$GCM<-paste(A$GCM,"rcp85",sep=".")
AH<-rbind(AH,A);rm(A)

AF<-ALL_FUTURE
all<-rbind(AH, AF)

all$Month<-format(all$Date,"%m")
all$Year<-format(all$Date,"%Y")

all.mon<-aggregate(Precip_mm~GCM+Month+Year,all,sum,na.rm=TRUE)
all.mon$Date<-as.Date(paste(all.mon$Year,all.mon$Month,1,sep="-"),format="%Y-%m-%d")
all.mon$pr.corrected<-all.mon$Precip_mm+correction #bias correction - delta method
# all.mon$CFGCM<-as.factor(paste0(as.character(all.mon$CF)," ", as.character(all.mon$GCM)))

### Need to do this in loop by CF then merge back together
CF.split<-split(all.mon,all.mon$GCM)

for (i in 1:length(CF.split)){
  CF.split[[i]]$Pr_3mo<-rollmean(CF.split[[i]]$Precip_mm,k=3,fill=NA,align="right")
  CF.split[[i]]$PrLag<-lag(CF.split[[i]]$Pr_3mo,2)
  CF.split[[i]]$modeled<- xint*(CF.split[[i]]$PrLag) + yint
  # CF.split[[i]]$modeled<- lm$coefficients[2]*(CF.split[[i]]$PrLag) + lm$coefficients[2]
}

all2<- ldply(CF.split, data.frame)

all2$Flow.below<-0
all2$Flow.below[which(all2$modeled<17)]<-1
all2$Year<-as.numeric(all2$Year)
all2$decade<-all2$Year - all2$Year%%10

# all.per<-subset(all2, Year >=2050 & Year <2080)
all.per2<-aggregate(Flow.below~GCM+Month+Year+decade,all2,mean)

decade.mean<-aggregate(Flow.below~GCM+decade,all.per2,sum)
decade.mean$emissions[grep("rcp85",decade.mean$GCM)] = "RCP 8.5"
decade.mean$emissions[grep("rcp45",decade.mean$GCM)] = "RCP 4.5"
decade.mean$emissions[which(decade.mean$decade<2010)] <- "Historical"

decade.mean$GCM[which(decade.mean$decade<2010)] <- "Historical"
decade.mean$Flow.below[which(decade.mean$decade == 2000 | decade.mean$decade == 2010)] <- NA


# SUBSET AND PLOT
emissions.flow<-aggregate(Flow.below~emissions+decade,decade.mean,mean)
CF.flow<-subset(decade.mean,GCM %in% GCM_sub2080 | GCM == "Historical")
CF.flow<-aggregate(Flow.below~GCM+decade,CF.flow,mean)
CF.flow$GCM<-revalue(CF.flow$GCM, c("inmcm4.rcp45"="INMCM4 RCP 4.5", "IPSL-CM5A-MR.rcp85"="IPSL-CM5A-MR RCP 8.5"))


### 2-Fig plot

# bars same size @ https://stackoverflow.com/questions/38101512/the-same-width-of-the-bars-in-geom-barposition-dodge

a<-ggplot(emissions.flow, aes(x=decade, y=Flow.below, group=emissions, colour = emissions)) +
  # geom_rect(xmin=2050, xmax=2080,
  #           ymin=-Inf, ymax=Inf,fill="grey",alpha=0.2,colour="black") +
  geom_bar(colour = "black", stat = "identity",aes(fill=emissions),position="dodge") +
  geom_hline(yintercept=17,linetype=2,colour="black",size=1) +
  theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=10,vjust=-0.2),
        axis.title.y=element_text(size=10,vjust=1.0),
        plot.title=element_text(size=13,hjust=0),
        legend.text=element_text(size=9), legend.title=element_text(size=9),
        legend.position=c(0,1), 
        legend.justification='left',
        legend.direction='horizontal',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  scale_color_manual(name="",values = c("grey",col.RCP2)) +
  scale_fill_manual(breaks = c("RCP 4.5", "RCP 8.5"),name="",values = c("grey",col.RCP2)) +
  labs(title = "(a) ",
       y="Months / Decade",x=" ") + guides(color=guide_legend(override.aes = list(size=7))) +
  scale_y_continuous(limits=c(0,48))
a


b<-ggplot(CF.flow, aes(x=decade, y=Flow.below, group=GCM, colour = GCM)) +
  # geom_rect(xmin=2050, xmax=2080,
  #           ymin=-Inf, ymax=Inf,fill="grey",alpha=0.2,colour="black") +
  geom_bar(colour = "black", stat = "identity",aes(fill=GCM),position="dodge") +
  geom_hline(yintercept=17,linetype=2,colour="black",size=1) +
  theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=10,vjust=-0.2),
        axis.title.y=element_text(size=10,vjust=1.0),
        plot.title=element_text(size=13,hjust=0),
        legend.text=element_text(size=12), legend.title=element_text(size=12),
        legend.position=c(0,1), 
        legend.justification='left',
        legend.direction='horizontal',
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  scale_color_manual(name="",values = c("grey",colors2)) +
  scale_fill_manual(breaks = c("INMCM4 RCP 4.5", "IPSL-CM5A-MR RCP 8.5"),name="",values = c("grey",colors2)) +
  labs(title = "(b) ",
       y="Months / Decade",x=" ") + guides(color=guide_legend(override.aes = list(size=7))) +
  scale_y_continuous(limits=c(0,48))
b

grid.arrange(a,b, nrow=2,ncol=1)

g <- arrangeGrob(a,b, nrow=2,ncol=1)
g<- annotate_figure(g,  top = text_grob("Months/decade with reconstructed flow below 20 GPM"))
g$vp = grid::viewport(height=0.9, width=0.9) 
ggsave("Fig5a.eps", g,width = 9, height = 7)
ggsave("Fig5a.jpg", g,width = 9, height = 7)


### 4-Fig plot
a<-ggplot(subset(emissions.flow,emissions !="RCP 8.5"), 
          aes(x=decade, y=Flow.below, group=emissions, colour = emissions)) +
  # geom_rect(xmin=2050, xmax=2080,
  #           ymin=-Inf, ymax=Inf,fill="grey",alpha=0.2,colour="black") +
  geom_bar(colour = "black", stat = "identity",aes(fill=emissions),position="dodge") +
  geom_hline(yintercept=17,linetype=2,colour="black",size=1) +
  theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=10,vjust=-0.2),
        axis.title.y=element_text(size=10,vjust=1.0),
        plot.title=element_text(size=13,hjust=0),
        legend.text=element_text(size=9), legend.title=element_text(size=9),
        legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  scale_color_manual(name="",values = c("grey",col.RCP2[1])) +
  scale_fill_manual(name="",values = c("grey",col.RCP2[1])) +
  labs(title = "(a) RCP 4.5",
       y="Months / Decade",x=" ") + guides(color=guide_legend(override.aes = list(size=7))) +
  scale_y_continuous(limits=c(0,48))
a


b<-ggplot(subset(emissions.flow,emissions !="RCP 4.5"), 
          aes(x=decade, y=Flow.below, group=emissions, colour = emissions)) +
  # geom_rect(xmin=2050, xmax=2080,
  #           ymin=-Inf, ymax=Inf,fill="grey",alpha=0.2,colour="black") +
  geom_bar(colour = "black", stat = "identity",aes(fill=emissions),position="dodge") +
  geom_hline(yintercept=17,linetype=2,colour="black",size=1) +
  theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=10,vjust=-0.2),
        axis.title.y=element_text(size=10,vjust=1.0),
        plot.title=element_text(size=13,hjust=0),
        legend.text=element_text(size=9), legend.title=element_text(size=9),
        legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  scale_color_manual(name="",values = c("grey",col.RCP2[2])) +
  scale_fill_manual(name="",values = c("grey",col.RCP2[2])) +
  labs(title = "(b) RCP 8.5",
       y="Months / Decade",x=" ") + guides(color=guide_legend(override.aes = list(size=7))) +
  scale_y_continuous(limits=c(0,48))
b

c<-ggplot(subset(CF.flow,GCM !="IPSL-CM5A-MR RCP 8.5"), 
          aes(x=decade, y=Flow.below, group=GCM, colour = GCM)) +
  # geom_rect(xmin=2050, xmax=2080,
  #           ymin=-Inf, ymax=Inf,fill="grey",alpha=0.2,colour="black") +
  geom_bar(colour = "black", stat = "identity",aes(fill=GCM),position="dodge") +
  geom_hline(yintercept=17,linetype=2,colour="black",size=1) +
  theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=10,vjust=-0.2),
        axis.title.y=element_text(size=10,vjust=1.0),
        plot.title=element_text(size=13,hjust=0),
        legend.text=element_text(size=9), legend.title=element_text(size=9),
        legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  scale_color_manual(name="",values = c("grey",colors2[1])) +
  scale_fill_manual(name="",values = c("grey",colors2[1])) +
  labs(title = "(c) INMCM4 RCP 4.5",
       y=" ",x=" ") + guides(color=guide_legend(override.aes = list(size=7))) +
  scale_y_continuous(limits=c(0,48))
c

d<-ggplot(subset(CF.flow,GCM !="INMCM4 RCP 4.5"), 
          aes(x=decade, y=Flow.below, group=GCM, colour = GCM)) +
  # geom_rect(xmin=2050, xmax=2080,
  #           ymin=-Inf, ymax=Inf,fill="grey",alpha=0.2,colour="black") +
  geom_bar(colour = "black", stat = "identity",aes(fill=GCM),position="dodge") +
  geom_hline(yintercept=17,linetype=2,colour="black",size=1) +
  theme(axis.text=element_text(size=10),
        axis.title.x=element_text(size=10,vjust=-0.2),
        axis.title.y=element_text(size=10,vjust=1.0),
        plot.title=element_text(size=13,hjust=0),
        legend.text=element_text(size=9), legend.title=element_text(size=9),
        legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(0,1,0,1), "cm")) + 
  scale_color_manual(name="",values = c("grey",colors2[2])) +
  scale_fill_manual(name="",values = c("grey",colors2[2])) +
  labs(title = "(d) IPSL-CM5A-MR RCP 8.5",
       y=" ",x=" ") + guides(color=guide_legend(override.aes = list(size=7))) +
  scale_y_continuous(limits=c(0,48))
d

grid.arrange(a,c,b,d, nrow=2,ncol=2)

g <- arrangeGrob(a,c,b,d, nrow=2,ncol=2)
g<- annotate_figure(g,  top = text_grob("Months/decade with reconstructed flow below 20 GPM"))
g$vp = grid::viewport(height=0.9, width=0.9) 
ggsave("Fig5b.eps", g,width = 9, height = 7)
ggsave("Fig5b.jpg", g,width = 9, height = 7)


