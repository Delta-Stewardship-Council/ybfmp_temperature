# bring in ybfmp temperature data collected with fish data
# this will be used to fill in missing summer temperatures for yb rstr temp logger time series

# library
require(dplyr)

f_get_WQ_w_fish <- function()
{
  # data
    #enviro_dat <- read.csv("data_raw/Enviro_w_fish_98_14.csv") # need > 2014-11-13
  # now on EDI!
  #Interagency Ecological Program: Fish catch and water quality data from the Sacramento River floodplain and tidal slough, collected by the Yolo Bypass Fish Monitoring Program, 1998-2021.
    filtered_data_URL = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=4488201fee45953b001f70acf30f7734"
    stations_URL = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=89146f1382d7dfa3bbf3e4b1554eb5cc"

  # store hash values:
    WQ_w_fish_event <- contentid::store(filtered_data_URL)
    WQ_w_fish_stations <- contentid::store(stations_URL)

  # retrieve data using hash:
    WQ_w_fish_data <- read.csv(contentid::retrieve(WQ_w_fish_event))
    WQ_w_fish_stations <- read.csv(contentid::retrieve(WQ_w_fish_stations))

  # clean dates
    head(WQ_w_fish_data)
    WQ_w_fish_data$Datetime <- as.POSIXct(WQ_w_fish_data$Datetime, format = "%m/%d/%Y %H:%M")
    WQ_w_fish_data$time <- format(WQ_w_fish_data$Datetime, format = "%H:%M")
    WQ_w_fish_data$date <- as.Date(WQ_w_fish_data$Datetime)
    max(WQ_w_fish_data$date)
    WQ_w_fish <- WQ_w_fish_data[,c(2,5,25:26)]

  # qc
    colSums(is.na(WQ_w_fish)) #190 NAs
    WQ_w_fish_dup <- WQ_w_fish[!duplicated(WQ_w_fish), ] #14 duplicates

    WQ_w_fish_daily <- WQ_w_fish_dup %>%
      group_by(date) %>%
      summarize(n())

    max(WQ_w_fish_daily$`n()`) #22 (flooded beach seine + multiple-check RSTR day)

  # make daily
    cv <- function(x) 100*( sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

    WQ_w_fish_daily <- WQ_w_fish_dup[,c(2,4)] %>%
      group_by(date) %>%
      summarise_each(funs(mean = mean(., na.rm = TRUE), max = max(., na.rm = TRUE), min = min(., na.rm = TRUE), sd = sd(., na.rm = TRUE), cv, n = sum(!is.na(.))))

  # investigate
   check <- subset(WQ_w_fish_daily, sd == 0 & n > 1)# different sites and times, but exactly the same temperature (to the .00)... unlikely, but not impossible? (impacts 124 days)

  # remove zero data dates
   WQ_w_fish_final <- subset(WQ_w_fish_daily, n > 0)

  # add data description columns
   WQ_w_fish_final$method <- "WQ_w_fish"
   WQ_w_fish_final$category <- "data"

   unique(WQ_w_fish$StationCode)
   stations_dat <- WQ_w_fish[,c(1,4)]
   stations_dat <- stations_dat[!duplicated(stations_dat), ]
   stations <- stations_dat %>%
     group_by(date) %>%
     mutate(site = paste0(StationCode, collapse = ","))

   stations <- stations[,c(2:3)]
   stations <- stations[!duplicated(stations), ]

    WQ_w_fish_final <- merge(WQ_w_fish_final, stations, by = "date", all.x = TRUE)

    write.csv(rstr_daily_final, "data_clean/WQ_w_fish_daily.csv", row.names = FALSE)


}
