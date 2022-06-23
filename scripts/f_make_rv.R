# make RIV and RVB hourly data from Cat's EDI publication daily
# combine time series
# fill remaining NAs within target timeline (to make continuous daily water temperature data) - 1998-01-16 to 2019-12-31

# library
library(dplyr)
library(imputeTS)

f_make_rv <- function() {
  # data
    hourly_data <- read.csv("data_clean/hourly_CDEC_stations.csv")
    riv <- subset(hourly_data, Station == "RIV")
    rvb <- subset(hourly_data, Station == "RVB")

  # make daily
    cv <- function(x) 100*( sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

    riv_daily <- riv[,-c(1:3)] %>%
      group_by(Date) %>%
      summarise_each(funs(mean = mean(., na.rm = TRUE), max = max(., na.rm = TRUE), min = min(., na.rm = TRUE), sd = sd(., na.rm = TRUE), cv, n = sum(!is.na(.))))

    rvb_daily <- rvb[,-c(1:3)] %>%
      group_by(Date) %>%
      summarise_each(funs(mean = mean(., na.rm = TRUE), max = max(., na.rm = TRUE), min = min(., na.rm = TRUE), sd = sd(., na.rm = TRUE), cv, n = sum(!is.na(.))))

  # add data description columns
    riv_daily$site <- "RIV"
    riv_daily$category = "data"
    riv_daily$method = "Pien_2020"

    rvb_daily$site <- "RVB"
    rvb_daily$category = "data"
    rvb_daily$method = "Pien_2020"

  # combine
    continous_dates <- data.frame (x = 1:8020, Date = seq(as.Date('1998-01-16'),as.Date('2019-12-31'),by='day')) # target dates
    riv_daily <- merge(riv_daily, continous_dates, by = "Date", all = TRUE)
    riv_daily_n <- riv_daily[is.na(riv_daily$mean),] # missing dates
    rvb_fill <- rvb_daily[rvb_daily$Date %in% as.Date(riv_daily_n$Date),] # use rvb to fill
    rv_daily <- rbind(na.omit(riv_daily[,-11]), rvb_fill)

  # impute NAs within seven days of consecutive NAs
    min(rv_daily$Date) # 1999-02-24
    # not including beginning because greater than a seven day gap in days of consecutive missing data
    continous_dates_new <- data.frame (x = 1:7623, Date = seq(as.Date('1999-02-17'),as.Date('2019-12-31'),by='day')) # missing 397 days in 1998
    rv_cont <- merge(rv_daily, continous_dates_new, by = "Date", all = TRUE) # less than 7 continuous missing days

    # order by date
    rv_cont <- rv_cont[order(rv_cont$Date),]
    # separate
    rv_cont_data <- rv_cont[!is.na(rv_cont$mean),]
    rv_cont_NA <- rv_cont[is.na(rv_cont$mean),]
    head(rv_cont_NA) # 47 days

    # assigns id to consecutive date groups
    rv_cont_NA$group <- cumsum(c(1, diff.Date(rv_cont_NA$Date)) >= 2)

    rv_cont_NA_sum <- rv_cont_NA %>%
      group_by(group) %>%
      summarise(length = length(group)) %>%
      as.data.frame(rv_cont_NA_sum) # all 7 and under

    rv_cont_NA_complete <- merge(rv_cont_NA, rv_cont_NA_sum, by="group", all.x = TRUE)
    head(rv_cont_NA_complete)

    rv_cont_NA_complete$method <- "imputeTS"
    rv_cont_NA_complete$category <- "7&Under"
    rv_cont_data$length = NA
    rv_cont_data$group = NA

    imput_rv <- rbind(rv_cont_NA_complete, rv_cont_data)
    imput_rv <- imput_rv[order(imput_rv$Date),]

    # 7 day moving average
    imput_rv$mean <- na_ma(imput_rv$mean, k = 7, weighting = "exponential", maxgap = Inf)
    imput_rv$max <- na_ma(imput_rv$max, k = 7, weighting = "exponential", maxgap = Inf)
    imput_rv$min <- na_ma(imput_rv$min, k = 7, weighting = "exponential", maxgap = Inf)

    write.csv(imput_rv[,-12], "data_clean/clean_rv.csv", row.names = FALSE)
    }
