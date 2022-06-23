# get rstr ybfmp water temperature data

# meta data provided by DWR:
#WHO
#The water temperature data on the following worksheets (years 1998-2005) was collected by
#California Department of Water Resources - Division of Environmental Services - Aquatic Ecology Section

#Contact: Ted Sommer; (916) 376-9772, tsommer@water.ca.gov

#WHY
#This data was collected as part of the Yolo Bypass Aquatic Ecology Study funded by the Interagency Ecological Program and CALFED.
#For more information on this study visit: http://www.iep.water.ca.gov/AES/Yolo_Bypass.html

#WHERE
#This data was collected at out rotary screw trap site (RSTR) located in the Yolo Bypass Toe Drain,
#which is located near the north-east tip of Little Holland Tract at:
#  Lat/Lon hddd mm.mmm (WGS 84):  N38 21.210  W121 38.58

#WHAT
#This water temperature data was collected using Onset Computer Corp "Optic StowAway Temp" loggers.
#The Loggers were set in a steel sleeve and submerged at about 0.5 meter below surface behind our RSTR
#Collection of data occurred at 60 minute intervals from 1997 through 2002, at 30 minute intervals from 2003 through 2006, at 60 minute intervals from 2006 through spring of 2009.
#In the spring 2009, new data loggers were deployed and the interval was increased to 15 minutes.
#Typically, the readings were taken between January and June each year, but readings from December and July also occurred.
#In some years, gaps in the data occurred do to logger malfunction or other event.
#In 1998, only daily average temperature is available.

#*2011 - RSTR was destroyed during inundation event - Large data gap (3/17/11 - 5/4/2011)

#*2013 - RSTR data collected year-round

################################################################################
# load data from ybfmp rotary screw trap (rstr)

f_get_rstr <- function()
    {

  # load data
    rstr <- read.csv("data_raw/1998-2014_STTD Temp.csv", header = FALSE)
    head(rstr) # starts on 1/23/98
    colnames(rstr) <- c("date", "time", "water_temp")

  # need to deal with formatting
    rstr_98_08 <- rstr[-1,c(1:3)]
    rstr_98_08$date <- as.Date(rstr_98_08$date)

    rstr_09_12 <- rstr[,c(9:11)]
    colnames(rstr_09_12) <- c("date", "time", "water_temp")
    rstr_09_12$date <- as.Date(rstr_09_12$date)

    rstr_13_14 <- rstr[,c(14:16)]
    colnames(rstr_13_14) <- c("date", "time", "water_temp")
    rstr_09_12$date <- as.Date(rstr_13_14$date)# max date is 2013-12-20

  # rstr temperature logger data 1998-2013
    rstr_98_13 <- rbind(rstr_98_08, rstr_09_12, rstr_09_12)
    rstr_98_13$water_temp <- as.numeric(rstr_98_13$water_temp)

  # add 2014-present
  # 2014
    rstr_14 <- read.csv("data_raw/RSTR_2014Master.csv")
    rstr_14$date <- as.Date(rstr_14$Date.1)
    rstr_14 <- rstr_14[,-c(1,4)]
    colnames(rstr_14) <- c("time", "water_temp", "date")

  # 2015
  # meta data notes copied below if available
    rstr_2015 <- read.csv("data_raw/RSTR_2015Master.csv", check.names = F)
    colnames(rstr_2015) <- c("date_time", "water_temp_F", "water_temp", "Batt_V", "date")
    rstr_2015$date_time <- as.POSIXct(rstr_2015$date_time, format = "%m/%d/%Y %H:%M")
    rstr_2015$time <- format(rstr_2015$date_time, format = "%H:%M")
    rstr_2015 <- rstr_2015[,-c(1,2,4)]
    rstr_2015$date <- as.Date(rstr_2015$date)

  # 2016
    rstr_2016 <- read.csv("data_raw/RSTR_WTMaster2016.csv", check.names = F)
    #Data from 9/20 12:00 thru 12/31 23:45 inserted from data collected with YSI sonde installed at STTD site
    colnames(rstr_2016) <- c("date_time", "water_temp_F", "water_temp", "date")
    rstr_2016$date_time<- as.POSIXct(rstr_2016$date_time, format = "%m/%d/%Y %H:%M")
    rstr_2016$time <- format(rstr_2016$date_time, format = "%H:%M")
    rstr_2016 <- rstr_2016[,-c(1,2)]
    rstr_2016$date <- as.Date(rstr_2016$date)

  # 2017
    rstr_2017 <- read.csv("data_raw/RSTR_2017_WTMASTER.csv", check.names = F)
    #1/1/2017 - 4/5/2017 - Data is from YSI sonde near RSTR - hobo DATA was lost during downloading process
    colnames(rstr_2017) <- c("date_time", "water_temp_F", "water_temp", "date")
    rstr_2017$date_time <- as.POSIXct(rstr_2017$date_time, format = "%m/%d/%Y %H:%M")
    rstr_2017$time <- format(rstr_2017$date_time, format = "%H:%M")
    rstr_2017 <- rstr_2017[,-c(1,2)]
    rstr_2017$date <- as.Date(rstr_2017$date)

  # 2018
    rstr_2018 <- read.csv("data_raw/RSTR_2018_WTMASTER.csv", check.names = F)
    # 1/1/2017 - 4/5/2017 - Data is from YSI sonde near RSTR - hobo DATA was lost during downloading process (seems to be a duplicate from the previous year?)
    colnames(rstr_2018) <- c("date_time", "water_temp_F", "water_temp", "date")
    rstr_2018$date_time <- as.POSIXct(rstr_2018$date_time, format = "%m/%d/%Y %H:%M")
    rstr_2018$time <- format(rstr_2018$date_time, format = "%H:%M")
    rstr_2018 <- rstr_2018[,-c(1,2)]
    rstr_2018$date <- as.Date(rstr_2018$date)

  # combine
    rstr_98_18 <- rbind(rstr_98_13, rstr_14, rstr_2015, rstr_2016, rstr_2017, rstr_2018)
    write.csv(rstr_98_18, "data_clean/rstr_98_18.csv", row.names = FALSE)
  }


# meta data given the data in this file
#1998, 1/23/1998 - 6/30/1998, collected @ daily interval
#1999, 1/12/1999 - 7/8/1999, collected @ hourly interval
#2000, 1/3/2000 - 7/19/2000, collected @ hourly interval
#2001, 12/28/2000 - 6/11/2001, collected @ hourly interval
#2002, 12/12/2001 - 6/24/2002, collected @ 30 minute interval
#2003, 12/30/2002 - 6/30/2003, collected @ 30 minute interval
#2004, 1/8/2004 - 6/30/2004, collected @ 30 minute interval
#2005, 12/17/2004 - 7/8/2005, collected @ 30 minute interval
#2006, 1/13/2006 - 6/30/2006, collected @ hourly interval
#2007, 1/5/2007 - 6/22/2007, collected @ hourly interval
#2008, 1/11/2008 - 6/26/2008, collected @ hourly interval
#2009, 1/6/2009 - 6/26/2009, collected @ 15 minute interval
#2010, 1/5/2010 - 6/22/2010, collected @ 15 minute interval
#2011, 12/27/2010 - 7/26/2011, collected @ 15 minute interval
#2012, 1/3/2012 - 6/25/2012, collected @ 15 minute interval
#2013, 1/1/2013 - 12/20/2013, collected @ 15 minute interval
#2014, 1/1/2014 - 12/30/2014, collected @ 15 minute intervals
#2015, 1/1/2015 - 12/31/2015, collected @ 15 minute interval
#2016, 1/1/2016 - 12/31/2016, collected @ 15 minute interval
#2017, 1/1/2017 - 12/31/2017, collected @ 15 minute interval
#2018, 1/1/2018 - 6/8/2018, collected @ 15 minute interval






