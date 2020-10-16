## Download MACA data, temp in K, precip in mm.

library(cft)
library(tibble)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggrepel)
library(dplyr)
library(sp)
library(sf)

rm(list=ls())

## Park data
proj_dir <- "~/CFT_BIBE"
Lat <- 29.278302
Lon <- -103.336537
PARK <- "BIBE"

## Download data
#Variable and scenario names corresponding to MACA data directory structure
# vars = c("pr", "tasmax", "tasmin","rhsmax","rhsmin")
vars = c("pr", "tasmax", "tasmin")
scens = c("rcp45", "rcp85")

#Variable names for output tables
# VarNames = c("PrecipCustom", "TmaxCustom", "TminCustom","RHmaxCustom","RHminCustom")
VarNames = c("PrecipCustom", "TmaxCustom", "TminCustom")

# GCMs to be extracted
# GCMs = c('bcc-csm1-1','bcc-csm1-1-m','BNU-ESM','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0',
#          'GFDL-ESM2G','GFDL-ESM2M','HadGEM2-CC365','HadGEM2-ES365',
#          'inmcm4','IPSL-CM5A-MR','IPSL-CM5A-LR','IPSL-CM5B-LR',
#          'MIROC5','MIROC-ESM','MIROC-ESM-CHEM','MRI-CGCM3','NorESM1-M')
GCMs = c('bcc-csm1-1','bcc-csm1-1-m')

#Date ranges to be extracted
Future_StartYear = 2006   #2006-2099
Future_EndYear = 2099   #2006-2099
Hist_StartYear = 1950     #1950-2005
Hist_EndYear = 2005      #1950-2005
HistYears = 50  

############################## END INITIALS ##################################################

# Now can only use spatial object (not park name)
BIBE_coords <- data.frame(PARK=PARK,lat=Lat,lon=Lon)
coordinates(BIBE_coords) <- ~lon+lat
proj4string(BIBE_coords) <- "+proj=longlat +datum=NAD83 +no_defs " #same proj4string used in NPS_boundary_centroids.shp

# # Load Data - from centroids
# nps_boundary_centroids <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary_centroids/nps_boundary_centroids.shp')
# centroid <- filter(nps_boundary_centroids, UNIT_CODE == "BIBE")
# Sp_centroid <- as_Spatial(centroid) # Does not accept sf objects 

# download data
file_refs <- cftdata(aoi = BIBE_coords, 
                     area_name = PARK,
                     years = c(Hist_StartYear, Future_EndYear),
                     models = GCMs,
                     local_dir = proj_dir,
                     parameters = vars,
                     scenarios = scens,
                     ncores = parallel::detectCores() / 2)
glimpse(file_refs)
df <- cft_df(file_refs, ncores = parallel::detectCores() / 2)
glimpse(df)

######################## MANIPULATE INTO DF FOR CF-MS #####################
df$PrecipCustom <- df$pr/25.4
df$TmaxCustom <- 9/5 * (df$tasmax-273) + 32
df$TminCustom <- 9/5 * (df$tasmin-273) + 32
df$TavgCustom <- (df$TmaxCustom + df$TminCustom) / 2
df$GCM<-paste(df$model,df$rcp,sep=".")
df$Date<-as.POSIXlt(df$date,format="%Y-%m-%d")

# GCM, Date, PrecipCustom, TmaxCustom, TminCustom, TavgCustom
BIBE_missing_models<-as.data.frame(subset(df,select=c("GCM","Date","PrecipCustom", "TmaxCustom", "TminCustom", "TavgCustom")))
write.csv(BIBE_missing_models,paste(proj_dir,"BIBE_missing_models.csv",sep="/"),row.names=F)
