# make LIS hourly data from Cat's EDI publication daily
# impute NAs within target timeline (to make continuous daily water temperature data) - 1998-01-16 to 2019-12-31

f_make_lis <- function() {
  # data
    hourly_data <- read.csv("data_clean/hourly_CDEC_stations.csv")
    lis <- subset(hourly_data, Station == "LIS")
    lis$Date <- as.Date(lis$Date)
    min(lis$Date) # 2008-07-18

  # make daily
    cv <- function(x) 100*( sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

    lis_daily <- lis[,-c(1:4)] %>%
      group_by(Date) %>%
      summarise_each(funs(mean = mean(., na.rm = TRUE), max = max(., na.rm = TRUE), min = min(., na.rm = TRUE), sd = sd(., na.rm = TRUE), cv, n = sum(!is.na(.))))

  # add data description columns
    lis_daily$site <- "LIS"
    lis_daily$category = "data"
    lis_daily$method = "Pien_2020"

  # impute NAs within seven days of consecutive NAs
    # not including beginning because greater than a seven day gap in days of consecutive missing data
    continous_dates <- data.frame (x = 1:4191, Date = seq(as.Date('2008-07-11'),as.Date('2019-12-31'),by='day')) # missing 397 days in 1998
    lis_cont <- merge(lis_daily, continous_dates, by = "Date", all = TRUE)

    # order by date
    lis_cont <- lis_cont[order(lis_cont$Date),]
    # separate
    lis_cont_data <- lis_cont[!is.na(lis_cont$mean),]
    lis_cont_NA <- lis_cont[is.na(lis_cont$mean),]
    head(lis_cont_NA) # 506 days

    # assigns id to consecutive date groups
    lis_cont_NA$group <- cumsum(c(1, diff.Date(lis_cont_NA$Date)) >= 2)

    lis_cont_NA_sum <- lis_cont_NA %>%
      group_by(group) %>%
      summarise(length = length(group)) %>%
      as.data.frame(lis_cont_NA_sum) # Feb 2010-2011 missing

    lis_cont_NA_complete <- merge(lis_cont_NA, lis_cont_NA_sum, by="group", all.x = TRUE)
    lis_cont_NA_complete <- transform(lis_cont_NA_complete, category = ifelse(length > 7, "Over7", "7&Under"))
    lis_cont_NA_complete <- transform(lis_cont_NA_complete, method = ifelse(length <= 7, "imputeTS", "NA"))
    head(lis_cont_NA_complete)

    lis_cont_data$length = NA
    lis_cont_data$group = NA

    imput_lis <- rbind(lis_cont_NA_complete, lis_cont_data)
    imput_lis <- imput_lis[order(imput_lis$Date),]

    # 7 day moving average
    imput_lis$mean <- na_ma(imput_lis$mean, k = 7, weighting = "exponential", maxgap = Inf)
    imput_lis$max <- na_ma(imput_lis$max, k = 7, weighting = "exponential", maxgap = Inf)
    imput_lis$min <- na_ma(imput_lis$min, k = 7, weighting = "exponential", maxgap = Inf)

    # need to remove data that is not within parameters
    imput_lis$mean <- ifelse(imput_lis$category == "Over7", NA, imput_lis$mean)
    imput_lis$max <- ifelse(imput_lis$category == "Over7", NA, imput_lis$max)
    imput_lis$min <- ifelse(imput_lis$category == "Over7", NA, imput_lis$min)

    write.csv(imput_lis[,-12], "data_clean/clean_lis.csv", row.names = FALSE)

}
