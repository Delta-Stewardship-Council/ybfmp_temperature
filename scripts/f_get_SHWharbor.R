# get Sherwood harbor (Sacramento river) ybfmp water temperature data
library(dplyr)

f_get_SHWharbor <- function()
{
  # load data
    shw_temp <- read.csv("data_raw/SHWHarbor_Temp.csv") #made for Goertler et al 2017
    head(shw_temp)
    colSums(is.na(shw_temp))
    shw_temp <- shw_temp[,-3]
    shw_temp$Date <- as.Date(shw_temp$Date) #1998-01-23 to 2014-12-31
    colnames(shw_temp) <- c("date", "time", "water_temp")

  # qc checks
    hist(shw_temp$water_temp)# 2003 and 2007 have very high unrealistic values, but not outliers
    shw_temp_2003 <- subset(shw_temp, date >= '2003-01-01' & date <='2003-12-31')
    plot(shw_temp_2003$date, shw_temp_2003$water_temp)# last 18 days (remove 2003-05-10 to 2003-05-28)

    shw_temp_2007 <- subset(shw_temp, date >= '2007-01-01' & date <='2007-12-31')
    plot(shw_temp_2007$date, shw_temp_2007$water_temp)# first five days (remove 2007-05-10 to 2007-05-16)

    exclude = seq(as.Date('2003-05-10'),as.Date('2003-05-28'),by='day')
    data_qc_03 = shw_temp[!(shw_temp$date %in% exclude), ]

    exclude = seq(as.Date('2007-05-10'),as.Date('2007-05-16'),by='day')
    data_qc_07 = data_qc_03[!(data_qc_03$date %in% exclude), ]

  # remove less than zero (zeros are actually NAs)
    shw_temp_qc <- subset(data_qc_07, water_temp > 0)
    hist(shw_temp_qc$water_temp)

  # load more recent data
  # meta data notes copied below if available
  # 2015
    shw_15 <- read.csv("data_raw/Sherwood_2015Master.csv")
    colnames(shw_15) <- c("date_time", "water_temp_F", "water_temp", "Batt_V")
    shw_15$date_time <- as.POSIXct(shw_15$date_time, format = "%m/%d/%Y %H:%M")
    shw_15$time <- format(shw_15$date_time, format = "%H:%M")
    shw_15$date <- as.Date(shw_15$date)
    shw_15 <- shw_15[,-c(1,2,4)]

  # 2016
    shw_16<- read.csv("data_raw/SHR_WTMaster_2016.csv", check.names = F)
    colnames(shw_16) <- c("date_time", "water_temp_F", "water_temp", "Batt_V")
    shw_16$date_time <- as.POSIXct(shw_16$date_time, format = "%m/%d/%Y %H:%M")
    shw_16$time <- format(shw_16$date_time, format = "%H:%M")
    shw_16$date <- as.Date(shw_16$date)
    shw_16 <- shw_16[,-c(1,2,4:7)]

  # 2017
    shw_17 <- read.csv("data_raw/Sherwood_2017_WTMASTER.csv", check.names = F)
  #Data file from 1/10/2017 11:15 - 4/4/2017 9:30 was lost during download - data was replaced w/ FPT CDEC temp data (text in blue)
  #Data files 12/14/17-1/18/18 had date and time error that had to be corrected - due to HOBO shuttle low battery and incorrect internal date and time
    colnames(shw_17) <- c("date_time", "water_temp_F", "water_temp")
    shw_17$date_time<- as.POSIXct(shw_17$date_time, format = "%m/%d/%Y %H:%M")
    shw_17$time <- format(shw_17$date_time, format = "%H:%M")
    shw_17$date <- as.Date(shw_17$date)
    shw_17 <- shw_17[,-c(1,2)]

  # 2018
    shw_18 <- read.csv("data_raw/Sherwood_2018_WTMASTER.csv", check.names = F)
    colnames(shw_18) <- c("date_time", "water_temp_F", "water_temp")
    shw_18$date_time<- as.POSIXct(shw_18$date_time, format = "%m/%d/%Y %H:%M")
    shw_18$time <- format(shw_18$date_time, format = "%H:%M")
    shw_18$date <- as.Date(shw_18$date)
    shw_18 <- shw_18[,-c(1,2,4)]

  # 2019
    shw_19 <- read.csv("data_raw/Sherwood_2019_WaterTempMaster.csv", check.names = F)
  #Battery in HOBO died 9/12/2016 and was removed and replaced with new HOBO logger 9/15/16
  #Inserted Water Temperature Data from CDEC site SRH due to missing data - missing data was mostly due to downloading errors onto the HOBO shuttle
    colnames(shw_19) <- c("date_time", "water_temp_F", "water_temp")
    shw_19$date_time<- as.POSIXct(shw_19$date_time, format = "%m/%d/%Y %H:%M")
    shw_19$time <- format(shw_19$date_time, format = "%H:%M")
    shw_19$date <- as.Date(shw_19$date)
    shw_19 <- shw_19[,-c(1,2,4:9)]

  # 2020
    shw_20 <- read.csv("data_raw/Sherwood_2020_WaterTempMaster.csv", check.names = F)
    colnames(shw_20) <- c("date_time", "water_temp_F", "water_temp")
    shw_20$date_time<- as.POSIXct(shw_20$date_time, format = "%m/%d/%Y %H:%M")
    shw_20$time <- format(shw_20$date_time, format = "%H:%M")
    shw_20$date <- as.Date(shw_20$date)
    shw_20 <- shw_20[,-c(1,2,4:9)]


    shw_15_20 <- rbind(shw_15, shw_16, shw_17, shw_18, shw_19, shw_20)

  #qc
    hist(shw_15_20$water_temp) # negative values June 2018
    shw_temp_beg <- subset(shw_15_20, date >= '2015-01-01' & date <='2016-12-31')
    plot(shw_temp_beg$date, shw_temp_beg$water_temp)#looks good

    shw_temp_mid <- subset(shw_15_20, date >= '2018-01-01' & date <='2018-12-31')
    plot(shw_temp_mid$date, shw_temp_mid$water_temp)# remove -17.78

    shw_temp_end <- subset(shw_15_20, date >= '2019-01-01' & date <='2020-12-31')
    plot(shw_temp_end$date, shw_temp_end$water_temp) # remove 10/14/2019

    shw_15_20_qc = subset(shw_15_20, date != as.Date("2019-10-14"))
    shw_15_20_qc <- subset(shw_15_20_qc, water_temp > 0)

  # check if same weird duplicates as YB
    length(which(table(shw_15_20_qc$water_temp)>1))

    shw_15_20_check <- shw_15_20_qc[,c(1,3)]
    shw_15_20_dup <- data.frame(table(shw_15_20_check))

  # combine
    shw_98_20 <- rbind(shw_temp_qc, shw_15_20_qc)

    hist(shw_98_20$water_temp)

    write.csv(shw_98_20, "data_clean/shw_98_20.csv", row.names = FALSE)

}

#1998 1998-01-23 to 1998-05-06, daily
#1999 1999-01-14 to 1999-07-08, hourly
#2000 2000-01-05 to 2000-12-31, hourly
#2001 2001-01-01 to 2001-12-31, 30 minute
#2002 2002-01-01 to 2002-06-24, 30 minute
#2003 2003-01-03 to 2003-05-28, 30 minute
#2004 2004-01-08 to 2004-12-31, 30 minute
#2005 2005-01-01 to 2005-03-29, 30 minute
#2006 2006-01-10 to 2006-06-30, hourly
#2007 2007-05-10 to 2007-11-27, hourly
#2008 2008-01-16 to 2008-06-04, hourly
#2009 2009-01-07 to 2009-06-18, 15 minute
#2010 2010-01-06 to 2010-12-31, 15 minute
#2011 2011-01-01 2011-12-31, 15 minute
#2012 2012-01-01 to 2012-12-31, 15 minute
#2013 2013-01-01 to 2013-12-31, 15 minute
#2014 2014-01-01 to 2014-12-31, 15 minute
#2015 2015-01-01 to 2015-12-31, 15 minute
#2016 2016-01-01 to 2016-12-31, 15 minute
#2017 2017-01-01 to 2017-12-31, 15 minute
#2018 2018-01-01 to 2018-12-31, 15 minute
#2019 2019-01-01 to 2019-12-31, 15 minute
#2020 2020-01-01 to 2020-12-31, 15 minute

