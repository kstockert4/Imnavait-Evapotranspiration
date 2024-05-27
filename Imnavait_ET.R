#NIWR Sublimation Project 
setwd("D:/OldDrive/SublimationR/ECdata") #choose your own if reviewing this code with .csv files on github 

# Load the required packages
warning=FALSE 
library(data.table)
library(bit64)
library(plyr)
library(ggplot2)
library(lubridate)
library(RCurl)
library(forecast)
library(dplyr)
library(naniar)
library(lfstat)
library(ggstatsplot)
library(hrbrthemes)
library(viridis)
library(patchwork)
library(scales)
remotes::install_github("USGS-R/smwrData")
remotes::install_github("USGS-R/smwrBase")
library("gridExtra")
library(ggridges)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(rticles)
library(rmarkdown)
library(openxlsx)



#import all sites all data and convert to data table
Fen <- setDT(read.csv("ALL_IC_1523_gapfilled_20211231.csv", na.strings = -9999)) 
Tussock <- setDT(read.csv("ALL_IC_1993_gapfilled_20211231.csv", na.strings = -9999)) 
Ridge <- setDT(read.csv("ALL_IC_1991_gapfilled_20211231.csv", na.strings = -9999)) 


#CREATE AND FORMAT A DATE COLUMN 
#CONVERT "TIMESTAMPEND" TO DATE 
Fen$Date <- as.Date(as.character(Fen$TIMESTAMP_END), format = "%Y%m%d%H%M")
Tussock$Date <- as.Date(as.character(Tussock$TIMESTAMP_END), format = "%Y%m%d%H%M")
Ridge$Date <- as.Date(as.character(Ridge$TIMESTAMP_END), format = "%Y%m%d%H%M")

#water year function from smwrData and smwrBase (USGS GitHub, remote calls in package lines). 
library(smwrData)
data(QW05078470)
#'## Return an ordered factor
#waterYear(QW05078470$DATES)
waterYear <- function(x, numeric=FALSE) {
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for water year
  yr <- yr + ifelse(mn < 10L, 0L, 1L)
  if(numeric)
    return(yr)
  ordered(yr)
}
Fen$WaterYear <- waterYear(Fen$Date, numeric = FALSE)
Tussock$WaterYear <- waterYear(Tussock$Date, numeric = FALSE)
Ridge$WaterYear <- waterYear(Ridge$Date, numeric = FALSE)

#selecting only columns that are needed for analysis 
Fen <- select(Fen, Year, DoY, Hour, Date, WaterYear, LE_F, TA, P, D_SNOW, WS, RH, VPD_F, NETRAD, TS_1_1_1, TS_2_1_1)
Ridge <- select(Ridge, Year, DoY, Hour, Date, WaterYear, LE_F,  TA, P, D_SNOW, WS, RH, VPD_F, NETRAD, TS_1_1_1, TS_2_1_1)
Tussock <- select(Tussock, Year, DoY, Hour, Date, WaterYear, LE_F,  TA, P, WS, RH, VPD_F, NETRAD, TS_1_1_1, TS_2_1_1)


#rename columns for simplicity and convert from character to numeric data 
# the pattern is new_col_name = old_col_name.....................................
attach(Fen)
Fen <- data.frame(Year = as.numeric(Year), 
                  DoY = as.numeric(DoY),
                  Hour = as.numeric(Hour), 
                  Date = Date,
                  WaterYear = WaterYear,
                  LE = as.numeric(LE_F),
                  AirTemp = as.numeric(TA),
                  Precip = as.numeric(P),
                  SnowD = as.numeric(D_SNOW), 
                  WindSpeed = as.numeric(WS),
                  RH = as.numeric(RH),
                  VPD = as.numeric(VPD_F),
                  NetRadiation = as.numeric(NETRAD), 
                  SoilTemp = as.numeric(TS_1_1_1))
Fen <- Fen %>%
  mutate(month = month(Date, label = TRUE))
detach(Fen)

attach(Tussock)
Tussock <- data.frame(Year = as.numeric(Year), 
                      DoY = as.numeric(DoY),
                      Hour = as.numeric(Hour), 
                      Date = Date,
                      WaterYear = WaterYear,
                      LE = as.numeric(LE_F),
                      AirTemp = as.numeric(TA),
                      Precip = as.numeric(P),
                      WindSpeed = as.numeric(WS),
                      RH = as.numeric(RH),
                      VPD = as.numeric(VPD_F),
                      NetRadiation = as.numeric(NETRAD), 
                      SoilTemp = as.numeric(TS_1_1_1))
Tussock <- Tussock %>%
  mutate(month = month(Date, label = TRUE))
detach(Tussock)

attach(Ridge)
Ridge <- data.frame(Year = as.numeric(Year), 
                    DoY = as.numeric(DoY),
                    Hour = as.numeric(Hour), 
                    Date = Date,
                    WaterYear = WaterYear,
                    LE = as.numeric(LE_F),
                    AirTemp = as.numeric(TA),
                    Precip = as.numeric(P),
                    SnowD = as.numeric(D_SNOW),
                    WindSpeed = as.numeric(WS),
                    RH = as.numeric(RH),
                    VPD = as.numeric(VPD_F),
                    NetRadiation = as.numeric(NETRAD), 
                    SoilTemp = as.numeric(TS_1_1_1))
Ridge <- Ridge %>%
  mutate(month = month(Date, label = TRUE))
detach(Ridge)


#===============================================================================
#CALCULATE WATER VAPOR FLUXES===================================================
#===============================================================================
#Sublimation definition: sublimation is calculated when the latent heat flux is positive and when snowpack is present. 

#snowpack presence is determined from albedo measurements. (determination conducted by E. Euskirchen)

#Lv = 2.454 MJ/kg
#Ls = 2.838 MJ/kg

#--------------------------------------
#IDENTIFY DATE RANGE OF SNOW PRESENCE. CREATE 'NOSNOW' COLUMN WHERE 1 = SNOW FREE AND 0 = SNOW COVER PRESENT  

#IMMAVAIT CREEK SITES
attach(Fen)
Fen$nosnow <- ifelse(Year == 2010 & (DoY >= 147 & DoY <= 268), 1,
                     ifelse(Year == 2011 & (DoY >= 145 & DoY <= 263), 1,
                            ifelse(Year == 2012 & (DoY >= 152 & DoY <= 271), 1,
                                   ifelse(Year == 2013 & (DoY >= 163 & DoY <= 275), 1,
                                          ifelse(Year == 2014 & (DoY >= 161 & DoY <= 263), 1,
                                                 ifelse(Year == 2015 & (DoY >= 156 & DoY <= 239), 1,
                                                        ifelse(Year == 2016 & (DoY >= 133 & DoY <= 256), 1,
                                                               ifelse(Year == 2017 & (DoY >= 154 & DoY <= 262), 1,
                                                                      ifelse(Year == 2018 & (DoY >= 170 & DoY <= 265), 1,
                                                                             ifelse(Year == 2019 & (DoY >= 140 & DoY <= 266), 1,
                                                                                    ifelse(Year == 2020 & (DoY >= 149 & DoY <= 254), 1,
                                                                                           ifelse(Year==2009 & (DoY <= 259), 1, 
                                                                                                  ifelse(Year==2021 & (DoY >= 159 & DoY <= 262), 1, 0)))))))))))))
detach(Fen)

attach(Ridge)
Ridge$nosnow <- ifelse(Year == 2010 & (DoY >= 147 & DoY <= 268), 1,
                       ifelse(Year == 2011 & (DoY >= 145 & DoY <= 263), 1,
                              ifelse(Year == 2012 & (DoY >= 152 & DoY <= 271), 1,
                                     ifelse(Year == 2013 & (DoY >= 163 & DoY <= 275), 1,
                                            ifelse(Year == 2014 & (DoY >= 161 & DoY <= 263), 1,
                                                   ifelse(Year == 2015 & (DoY >= 156 & DoY <= 239), 1,
                                                          ifelse(Year == 2016 & (DoY >= 133 & DoY <= 256), 1,
                                                                 ifelse(Year == 2017 & (DoY >= 154 & DoY <= 262), 1,
                                                                        ifelse(Year == 2018 & (DoY >= 170 & DoY <= 265), 1,
                                                                               ifelse(Year == 2019 & (DoY >= 140 & DoY <= 266), 1,
                                                                                      ifelse(Year == 2020 & (DoY >= 149 & DoY <= 254), 1,
                                                                                             ifelse(Year==2009 & (DoY <= 259), 1, 
                                                                                                    ifelse(Year==2021 & (DoY >= 159 & DoY <= 262), 1, 0)))))))))))))
detach(Ridge)


attach(Tussock)
Tussock$nosnow <- ifelse(Year == 2010 & (DoY >= 147 & DoY <= 268), 1,
                         ifelse(Year == 2011 & (DoY >= 145 & DoY <= 263), 1,
                                ifelse(Year == 2012 & (DoY >= 152 & DoY <= 271), 1,
                                       ifelse(Year == 2013 & (DoY >= 163 & DoY <= 275), 1,
                                              ifelse(Year == 2014 & (DoY >= 161 & DoY <= 263), 1,
                                                     ifelse(Year == 2015 & (DoY >= 156 & DoY <= 239), 1,
                                                            ifelse(Year == 2016 & (DoY >= 133 & DoY <= 256), 1,
                                                                   ifelse(Year == 2017 & (DoY >= 154 & DoY <= 262), 1,
                                                                          ifelse(Year == 2018 & (DoY >= 170 & DoY <= 265), 1,
                                                                                 ifelse(Year == 2019 & (DoY >= 140 & DoY <= 266), 1,
                                                                                        ifelse(Year == 2020 & (DoY >= 149 & DoY <= 254), 1,
                                                                                               ifelse(Year==2009 & (DoY <= 259), 1, 
                                                                                                      ifelse(Year==2021 & (DoY >= 159 & DoY <= 262), 1, 0)))))))))))))
detach(Tussock)

#CALCULATE HALF-HOURLY WATER VAPOR FLUXES
#If flux is positive, sublimation or evaporation; if NOSNOW == 1, evaporation or condensation

#IMNAVAIT CREEK FEN:
Fen$flux_hourly <- ifelse(Fen$nosnow == 1, (Fen$LE/2454000)*1800, (Fen$LE/2838000)*1800) #actually, it's half hour averages, not hourly

attach(Fen)
Fen$Flux_hourly_type <- ifelse(flux_hourly > 0, ifelse(nosnow == 1, "Evaporation", "Sublimation"), 
                               ifelse(nosnow == 1, "Condensation", "Deposition"))
detach(Fen)

Fen$Sublimation_hourly <- ifelse(Fen$Flux_hourly_type == "Sublimation", Fen$flux_hourly, 0) 
Fen$Evaporation_hourly <- ifelse(Fen$Flux_hourly_type == "Evaporation", Fen$flux_hourly, 0)
Fen$Condensation_hourly <- ifelse(Fen$Flux_hourly_type == "Condensation", Fen$flux_hourly, 0)
Fen$Deposition_hourly <- ifelse(Fen$Flux_hourly_type == "Deposition", Fen$flux_hourly, 0)

#IMNAVAIT CREEK RIDGE
Ridge$flux_hourly <- ifelse(Ridge$nosnow == 1, (Ridge$LE/2454000)*1800, (Ridge$LE/2838000)*1800)

attach(Ridge)
Ridge$Flux_hourly_type <- ifelse(flux_hourly > 0, ifelse(nosnow == 1, "Evaporation", "Sublimation"), 
                                 ifelse(nosnow == 1, "Condensation", "Deposition"))
detach(Ridge)

Ridge$Sublimation_hourly <- ifelse(Ridge$Flux_hourly_type == "Sublimation", Ridge$flux_hourly, 0) 
Ridge$Evaporation_hourly <- ifelse(Ridge$Flux_hourly_type == "Evaporation", Ridge$flux_hourly, 0)
Ridge$Condensation_hourly <- ifelse(Ridge$Flux_hourly_type == "Condensation", Ridge$flux_hourly, 0)
Ridge$Deposition_hourly <- ifelse(Ridge$Flux_hourly_type == "Deposition", Ridge$flux_hourly, 0)

#IMNAVAIT CREEK TUSSOCK
Tussock$flux_hourly <- ifelse(Tussock$nosnow == 1, (Tussock$LE/2454000)*1800, (Tussock$LE/2838000)*1800) 

attach(Tussock)
Tussock$Flux_hourly_type <- ifelse(flux_hourly > 0, ifelse(nosnow == 1, "Evaporation", "Sublimation"), 
                                   ifelse(nosnow == 1, "Condensation", "Deposition"))
detach(Tussock)

Tussock$Sublimation_hourly <- ifelse(Tussock$Flux_hourly_type == "Sublimation", Tussock$flux_hourly, 0)
Tussock$Evaporation_hourly <- ifelse(Tussock$Flux_hourly_type == "Evaporation", Tussock$flux_hourly, 0)
Tussock$Condensation_hourly <- ifelse(Tussock$Flux_hourly_type == "Condensation", Tussock$flux_hourly, 0)
Tussock$Deposition_hourly <- ifelse(Tussock$Flux_hourly_type == "Deposition", Tussock$flux_hourly, 0)



#IMNAVAIT CREEK FEN
flux <- length(Fen$flux_hourly)
flux2 <- flux - 48
fenflux <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fenflux[i]<-sum(Fen$flux_hourly[i:(i+47)], na.rm = TRUE)} 
Fen$flux_daily <- fenflux

fensubl <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fensubl[i]<-sum(Fen$Sublimation_hourly[i:(i+47)], na.rm = TRUE)} 
Fen$sublimation_daily <- fensubl

fenevap <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fenevap[i]<-sum(Fen$Evaporation_hourly[i:(i+47)], na.rm = TRUE)} 
Fen$Evaporation_daily <- fenevap

fendep <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fendep[i]<-sum(Fen$Deposition_hourly[i:(i+47)], na.rm = TRUE)} 
Fen$Deposition_daily <- fendep

fencond <- rep(0,flux)  # of obs
for (i in 1:flux2){
  fencond[i]<-sum(Fen$Condensation_hourly[i:(i+47)], na.rm = TRUE)} 
Fen$Condensation_daily <- fencond

#----------------------------------
Fen_daily <- filter(Fen, Hour == 0) #dataframe with daily VALUES

Fen_daily <- Fen_daily %>%
  mutate(month = month(Date, label = TRUE),
         day = day(Date))


Fen_daily <- Fen_daily %>%
  mutate(month_num = month(Date, label = FALSE))

Fen_daily$Date_POSIXct <- as.POSIXct(Fen_daily$Date+1)

Fen_daily$DoWY <- ifelse(Fen_daily$DoY > 274, Fen_daily$DoY-274, 91 + Fen_daily$DoY)

#IMNAVAIT CREEK RIDGE
flux <- length(Ridge$flux_hourly)
flux2 <- flux - 48
Ridgeflux <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgeflux[i]<-sum(Ridge$flux_hourly[i:(i+47)], na.rm = TRUE)} 
Ridge$flux_daily <- Ridgeflux

Ridgesubl <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgesubl[i]<-sum(Ridge$Sublimation_hourly[i:(i+47)], na.rm = TRUE)} 
Ridge$sublimation_daily <- Ridgesubl

Ridgeevap <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgeevap[i]<-sum(Ridge$Evaporation_hourly[i:(i+47)], na.rm = TRUE)} 
Ridge$Evaporation_daily <- Ridgeevap

Ridgedep <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgedep[i]<-sum(Ridge$Deposition_hourly[i:(i+47)], na.rm = TRUE)} 
Ridge$Deposition_daily <- Ridgedep

Ridgecond <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Ridgecond[i]<-sum(Ridge$Condensation_hourly[i:(i+47)], na.rm = TRUE)} 
Ridge$Condensation_daily <- Ridgecond

#---------------------------------------
Ridge_daily <- filter(Ridge, Hour == 0) #dataframe with daily VALUES

Ridge_daily <- Ridge_daily %>%
  mutate(month = month(Date, label = TRUE),
         day = day(Date))

Ridge_daily <- Ridge_daily %>%
  mutate(month_num = month(Date, label = FALSE))

Ridge_daily$Date_POSIXct <- as.POSIXct(Ridge_daily$Date+1)

Ridge_daily$DoWY <- ifelse(Ridge_daily$DoY > 274, Ridge_daily$DoY-274, 91 + Ridge_daily$DoY)

# LOOK AT DATA 
Ridge_daily %>%
  filter(WaterYear == 2014) %>%
  ggplot() +
  geom_line(aes(x=Date_POSIXct, y = Evaporation_daily))+
  #scale_x_datetime(date_labels = "%b-%Y", date_breaks = "2 years") + 
  labs(title = "Ridge Daily Evaporation",
       y = "Water Flux (mm H2O)",
       x = " ")+
  geom_abline(slope = 0, intercept = 0, color = "darkgreen") +
  theme_bw(base_size = 15)+
  ylim(-0.1,3)


#IMNAVAIT CREEK TUSSOCK
flux <- length(Tussock$flux_hourly)
flux2 <- flux - 48
Tussockflux <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockflux[i]<-sum(Tussock$flux_hourly[i:(i+47)], na.rm = TRUE)} 
Tussock$flux_daily <- Tussockflux

Tussocksubl <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussocksubl[i]<-sum(Tussock$Sublimation_hourly[i:(i+47)], na.rm = TRUE)} 
Tussock$sublimation_daily <- Tussocksubl

Tussockevap <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockevap[i]<-sum(Tussock$Evaporation_hourly[i:(i+47)], na.rm = TRUE)} 
Tussock$Evaporation_daily <- Tussockevap

Tussockdep <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockdep[i]<-sum(Tussock$Deposition_hourly[i:(i+47)], na.rm = TRUE)} 
Tussock$Deposition_daily <- Tussockdep

Tussockcond <- rep(0,flux)  # of obs
for (i in 1:flux2){
  Tussockcond[i]<-sum(Tussock$Condensation_hourly[i:(i+47)], na.rm = TRUE)} 
Tussock$Condensation_daily <- Tussockcond

#-------------------------------------------
Tussock_daily <- filter(Tussock, Hour == 0) #dataframe with daily values

Tussock_daily <- Tussock_daily %>%
  mutate(month = month(Date, label = TRUE),
         day = day(Date))

Tussock_daily <- Tussock_daily %>%
  mutate(month_num = month(Date, label = FALSE))

Tussock_daily$Date_POSIXct <- as.POSIXct(Tussock_daily$Date+1)

Tussock_daily$DoWY <- ifelse(Tussock_daily$DoY > 274, Tussock_daily$DoY-274, 91 + Tussock_daily$DoY)

#LOOK AT DATA
Tussock_daily %>%
  # filter(WaterYear == 2016) %>%
  ggplot() +
  geom_line(aes(x=Date_POSIXct, y = sublimation_daily))+
  #scale_x_datetime(date_labels = "%b-%Y", date_breaks = "2 years") + 
  labs(title = "Tussock Daily Sublimation",
       y = "Water Flux (mm H2O)",
       x = " ")+
  geom_abline(slope = 0, intercept = 0, color = "darkgreen") +
  theme_bw(base_size = 15)+
  ylim(-0.1,1.5)

### ----------- SUMMARIZE ET DATA ---------------------------------------
#FEN:
Fen_ET_Month_Stats <- Fen_daily %>%  
  filter(WaterYear > 2008 & WaterYear < 2022) %>%
  group_by(month, WaterYear) %>%
  summarise(daily_ET_sum = sum(Evaporation_daily, na.rm = TRUE))

Fen_ET2_Month_Stats <- Fen_ET_Month_Stats %>%
  filter(WaterYear != 2013) %>% #missing data, see BASIC QC section below (line 473)
  group_by(month) %>%
  summarise(Fen_ET = round(mean(daily_ET_sum, na.rm = TRUE), 0))

Fen_ET_Annual_Stats <- Fen_ET_Month_Stats %>%
  group_by(WaterYear) %>%
  summarise(Fen_ET = round(sum(daily_ET_sum, na.rm = TRUE), 0))

Fen_ET_Annual_Stats[5,2] <- NA #remove 2013 with missing data

#RIDGE:
Ridge_ET_Month_Stats <- Ridge_daily %>%  
  filter(WaterYear > 2008 & WaterYear < 2022) %>%
  group_by(month, WaterYear) %>%
  summarise(daily_ET_sum = sum(Evaporation_daily, na.rm = TRUE))

Ridge_ET2_Month_Stats <- Ridge_ET_Month_Stats %>%
  group_by(month) %>%
  summarise(Ridge_ET = round(mean(daily_ET_sum, na.rm = TRUE), 0))

Ridge_ET_Annual_Stats <- Ridge_ET_Month_Stats %>%
  group_by(WaterYear) %>%
  summarise(Ridge_ET = round(sum(daily_ET_sum, na.rm = TRUE),0))

#Tussock:
Tussock_ET_Month_Stats <- Tussock_daily %>%  
  filter(WaterYear > 2008 & WaterYear < 2022) %>%
  group_by(month, WaterYear) %>%
  summarise(daily_ET_sum = sum(Evaporation_daily, na.rm = TRUE))

Tussock_ET2_Month_Stats <- Tussock_ET_Month_Stats %>%
  group_by(month) %>%
  summarise(Tussock_ET = round(mean(daily_ET_sum, na.rm = TRUE), 0))

Tussock_ET_Annual_Stats <- Tussock_ET_Month_Stats %>%
  group_by(WaterYear) %>%
  summarise(Tussock_ET = round(sum(daily_ET_sum, na.rm = TRUE),0))

#summary dataframes: 

annual_ET <- cbind(Tussock_ET_Annual_Stats, Ridge_ET_Annual_Stats$Ridge_ET, Fen_ET_Annual_Stats$Fen_ET)
annual_ET$mean_ET <- round(rowMeans(annual_ET[,2:4], na.rm=T), 0)

mean_monthly_ET <- cbind(Tussock_ET2_Month_Stats, Ridge_ET2_Month_Stats$Ridge_ET, Fen_ET2_Month_Stats$Fen_ET)
mean_monthly_ET$mean_ET <- round(rowMeans(mean_monthly_ET[,2:4], na.rm=T), 0)




#send to excel if desired. Goes to working directory set in Line #2
write.xlsx(annual_ET, file = "IB_AnnnualET_fromEC.xlsx", sheetName = "CumulativeAnnualET", colNames = TRUE)
write.xlsx(mean_monthly_ET, file = "IB_MonthlyET_fromEC.xlsx", sheetName = "MeanMonthlyET", colNames = TRUE)





#===========================BASIC QC ====================================================================
#IMNAVAIT CREEK FEN
wateryears <- c(2009:2021)
FenQuality <- data.frame(wateryears)
FenQuality$NA_flux_hourly <- c(sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2009 & Fen_daily$nosnow == 1])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2010 & Fen_daily$nosnow == 1])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2011 & Fen_daily$nosnow == 1])),                  
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2012 & Fen_daily$nosnow == 1])),                  
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2013 & Fen_daily$nosnow == 1])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2014 & Fen_daily$nosnow == 1])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2015 & Fen_daily$nosnow == 1])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2016 & Fen_daily$nosnow == 1])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2017 & Fen_daily$nosnow == 1])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2018 & Fen_daily$nosnow == 1])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2019 & Fen_daily$nosnow == 1])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2020 & Fen_daily$nosnow == 1])),
                               sum(is.na(Fen_daily$flux_hourly[Fen_daily$WaterYear == 2021 & Fen_daily$nosnow == 1])))
FenQuality
#missing 111 days in 2013 (2009 has missing years but due to non-inclusion of snow data)

#determine complete water years by # of records 
Fen_records <- Fen %>%
  group_by(WaterYear) %>%
  tally()
Fen_records


RidgeQuality <- data.frame(wateryears)
RidgeQuality$NA_flux_hourly <- c(sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2009 & Ridge_daily$nosnow == 1])),
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2010 & Ridge_daily$nosnow == 1])),
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2011 & Ridge_daily$nosnow == 1])),                  
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2012 & Ridge_daily$nosnow == 1])),                  
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2013 & Ridge_daily$nosnow == 1])),
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2014 & Ridge_daily$nosnow == 1])),
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2015 & Ridge_daily$nosnow == 1])),
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2016 & Ridge_daily$nosnow == 1])),
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2017 & Ridge_daily$nosnow == 1])),
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2018 & Ridge_daily$nosnow == 1])),
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2019 & Ridge_daily$nosnow == 1])),
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2020 & Ridge_daily$nosnow == 1])),
                               sum(is.na(Ridge_daily$flux_hourly[Ridge_daily$WaterYear == 2021 & Ridge_daily$nosnow == 1])))
RidgeQuality
#all good

TussockQuality <- data.frame(wateryears)
TussockQuality$NA_flux_hourly <- c(sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2009 & Tussock_daily$nosnow == 1])),
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2010 & Tussock_daily$nosnow == 1])),
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2011 & Tussock_daily$nosnow == 1])),                  
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2012 & Tussock_daily$nosnow == 1])),                  
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2013 & Tussock_daily$nosnow == 1])),
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2014 & Tussock_daily$nosnow == 1])),
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2015 & Tussock_daily$nosnow == 1])),
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2016 & Tussock_daily$nosnow == 1])),
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2017 & Tussock_daily$nosnow == 1])),
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2018 & Tussock_daily$nosnow == 1])),
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2019 & Tussock_daily$nosnow == 1])),
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2020 & Tussock_daily$nosnow == 1])),
                               sum(is.na(Tussock_daily$flux_hourly[Tussock_daily$WaterYear == 2021 & Tussock_daily$nosnow == 1])))
TussockQuality
#all good