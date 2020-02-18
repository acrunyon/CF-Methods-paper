# Read in BIBE data
#Create df that is 

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

rm(list=ls())
setwd("C:/Users/achildress/DOI/Schuurman, Gregor W - Assistants_2016 onward/CF Methods article/")
load("BIBE-data.RData")

# Threshold percentages for defining Climate futures. Default low/high:  0.25, 0.75
CFLow = 0.25     
CFHigh = 0.75
CFs = c("Warm Wet", "Hot Wet", "Central", "Warm Dry", "Hot Dry") #Use spaces and characters only
Range = 30  #Number of years to summarize (should be at least 30)

CF_sub<-c("Warm Wet","Hot Dry")
GCM_sub<-c("inmcm4.rcp45", "IPSL-CM5A-MR.rcp85")

col.RCP2 = c("blue", "red")

### Create DFs for summarizing ###
ALL_HIST$Year<-format(ALL_HIST$Date,"%Y")
ALL_FUTURE$Year<-format(ALL_FUTURE$Date,"%Y")
ALL_HIST$TmeanCustom<-(ALL_HIST$TminCustom+ALL_HIST$TmaxCustom)/2
ALL_FUTURE$TmeanCustom<-(ALL_FUTURE$TminCustom+ALL_FUTURE$TmaxCustom)/2

Baseline_all<-subset(ALL_HIST,Year<2000)
Baseline_all$per<-"Historical"
Future_all<-ALL_FUTURE
Future_all$per<-NA
Future_all$per[which(Future_all$Year>2024 & Future_all$Year <2056)]<-"2040"
Future_all$per[which(Future_all$Year>2044 & Future_all$Year <2076)]<-"2060"
Future_all$per[which(Future_all$Year>2064 & Future_all$Year <2096)]<-"2080"


####Set Average values for all four weather variables, using all baseline years and all climate models
BaseMeanPr = mean(Baseline_all$PrecipCustom)
BaseMeanTmean = mean(Baseline_all$TmeanCustom)

####Create Future/Baseline means data tables, with averages for all four weather variables, organized by GCM
Future_Means = data.frame(aggregate(cbind(PrecipCustom, TmeanCustom)
                                    ~ GCM+per, Future_all, mean,na.rm=F))   # , Future_all$Wind

Baseline_Means = data.frame(aggregate(cbind(PrecipCustom, TmeanCustom)~GCM+per, 
                                      Baseline_all, mean))    

#### add delta columns in order to classify CFs
Future_Means$DeltaPr = Future_Means$PrecipCustom - Baseline_Means$PrecipCustom
Future_Means$DeltaTmean = Future_Means$TmeanCustom - Baseline_Means$TmeanCustom

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
# 2040
dualscatter = ggplot(FM40, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM40$DeltaTmean,.25), 
                               xmax=quantile(FM40$DeltaTmean,.75), 
                               ymin=quantile(FM40$DeltaPr,.25)*365, 
                               ymax=quantile(FM40$DeltaPr,.75)*365))
dualscatter  + geom_text(aes(label=GCM)) + 
  geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title ="Changes in climate means centered on 2040 (2025-2055)\n relative to historical period (1950-2000) by GCM run", 
            x = "Changes in annual average temperature (F)", # Change
            y = "Changes in annual average precipitation (in)") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) #change

ggsave("2040-Scatter-.png", width = 15, height = 9)


# 2060
dualscatter = ggplot(FM60, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM60$DeltaTmean,.25), 
                               xmax=quantile(FM60$DeltaTmean,.75), 
                               ymin=quantile(FM60$DeltaPr,.25)*365, 
                               ymax=quantile(FM60$DeltaPr,.75)*365))
dualscatter  + geom_text(aes(label=GCM)) + 
  geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title ="Changes in climate means centered on 2060 (2045-2075)\n relative to historical period (1950-2000) by GCM run", 
       x = "Changes in annual average temperature (F)", # Change
       y = "Changes in annual average precipitation (in)") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) #change

ggsave("2060-Scatter-.png", width = 15, height = 9)

# 2080
dualscatter = ggplot(FM80, aes(DeltaTmean, DeltaPr*365, 
                               xmin=quantile(FM80$DeltaTmean,.25), 
                               xmax=quantile(FM80$DeltaTmean,.75), 
                               ymin=quantile(FM80$DeltaPr,.25)*365, 
                               ymax=quantile(FM80$DeltaPr,.75)*365))
dualscatter  + geom_text(aes(label=GCM)) + 
  geom_point(colour="black",size=4) +
  geom_point(aes(color=emissions),size=3.5) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title ="Changes in climate means centered on 2080 (2065-2095)\n relative to historical period (1950-2000) by GCM run", 
       x = "Changes in annual average temperature (F)", # Change
       y = "Changes in annual average precipitation (in)") + #change
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTmean)),linetype=2) #change

ggsave("2080-Scatter-.png", width = 15, height = 9)

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

RCP.P<-aggregate(PrecipCustom~emissions+per,Future_Means,mean)
RCP.T<-aggregate(TmeanCustom~emissions+per,Future_Means,mean)

Quad.P<-aggregate(PrecipCustom~CF+per,FM_quad,mean)
Quad.T<-aggregate(TmeanCustom~CF+per,FM_quad,mean)

Indiv.P<-aggregate(PrecipCustom~GCM+per,FM_indiv,mean)
Indiv.T<-aggregate(TmeanCustom~GCM+per,FM_indiv,mean)

R1<-
R2
Q1
Q2
X1
X2
Diffs_table$xx 


