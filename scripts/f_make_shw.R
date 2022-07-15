# make Sherwood Harbor logger data from YBFMP daily and fill in missing data
# impute NAs within target timeline (to make continuous daily water temperature data) - 1998-01-16 to 2019-12-31
# if gap in consecutive days is greater than 7, use lm with Rio Vista

# library
library(dplyr)
library(imputeTS)

f_make_shw <- function() {
  # data
    shw_98_20 <- read.csv("data_clean/shw_98_20.csv")
    rv <- read.csv("data_clean/clean_rv.csv")

    shw_98_20$date <- as.Date(shw_98_20$date)

  # summarize (true) interval (e.g., how many measurements per day)
    shw_daily <- shw_98_20 %>%
      group_by(date) %>%
      summarize(n()) # some is already daily and some is not

    duplicates <- subset(shw_daily, `n()`>96) # 2017-12-14 is 97, shouldn't be greater than 96 (15 minute interval)
    shw_12_14_17 = subset(shw_98_20, date == as.Date("2017-12-14"))

    # make daily
    cv <- function(x) 100*( sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

    shw_daily <- shw_98_20[,-2] %>%
      group_by(date) %>%
      summarise_each(funs(mean = mean(., na.rm = TRUE), max = max(., na.rm = TRUE), min = min(., na.rm = TRUE), sd = sd(., na.rm = TRUE), cv, n = sum(!is.na(.))))


    # add data description columns
    shw_daily <- transform(shw_daily, method = ifelse(n > 1, "ybfmp_logger", "daily_mean"))
    shw_daily$category <- "data"
    shw_daily$site <- "SHR"

    # need to pull out max and min that doesn't apply
    shw_daily$max <- ifelse(shw_daily$method == "daily_mean", "NA", shw_daily$max)
    shw_daily$min <- ifelse(shw_daily$method == "daily_mean", "NA", shw_daily$min)

    write.csv(shw_daily, "data_clean/SHWharbor_98_20_daily_logger.csv", row.names = FALSE)

    # need to remove suspect dates (2017-12-14) and 2007-10-31 to 2007-11-27 (see qc_plots.Rmd)

    shw_daily_qc <- subset(shw_daily, date != as.Date("2017-12-14"))
    exclude = seq(as.Date('2007-10-31'),as.Date('2007-11-27'),by='day')
    shw_daily_qc = shw_daily_qc[!(shw_daily_qc$date %in% exclude), ]

    # impute NAs within seven days of consecutive NAs
    continous_dates <- data.frame (x = 1:8020, date = seq(as.Date('1998-01-16'),as.Date('2019-12-31'),by='day')) # missing 397 days in 1998

    shw_cont <- merge(shw_daily_qc, continous_dates, by = "date", all.y = TRUE)

    # separate
    shw_cont_data <- shw_cont[!is.na(shw_cont$mean),]
    shw_cont_NA <- shw_cont[is.na(shw_cont$mean),]
    head(shw_cont_NA) # 2521 days

    # assigns id to consecutive date groups
    shw_cont_NA$group <- cumsum(c(1, diff.Date(shw_cont_NA$date)) >= 2)

    shw_cont_NA_sum <- shw_cont_NA %>%
      group_by(group) %>%
      summarise(length = length(group)) %>%
      as.data.frame(shw_cont_NA_sum)

    shw_cont_NA_complete <- merge(shw_cont_NA, shw_cont_NA_sum, by="group", all.x = TRUE)
    shw_cont_NA_complete <- transform(shw_cont_NA_complete, category = ifelse(length > 7, "Over7", "7&Under"))
    shw_cont_NA_complete <- transform(shw_cont_NA_complete, method = ifelse(length <= 7, "imputeTS", "lm_rv"))
    head(shw_cont_NA_complete)

    shw_cont_data$length = NA
    shw_cont_data$group = NA

    imput_shw <- rbind(shw_cont_NA_complete, shw_cont_data)
    imput_shw <- imput_shw[order(imput_shw$date),]
    imput_shw$max <- as.numeric(imput_shw$max)
    imput_shw$min <- as.numeric(imput_shw$min)

    # 7 day moving average
    imput_shw$mean <- na_ma(imput_shw$mean, k = 7, weighting = "exponential", maxgap = Inf)
    imput_shw$max <- na_ma(imput_shw$max, k = 7, weighting = "exponential", maxgap = Inf)
    imput_shw$min <- na_ma(imput_shw$min, k = 7, weighting = "exponential", maxgap = Inf)

    # need to remove data that is not within parameters
    imput_shw$mean <- ifelse(imput_shw$category == "Over7", NA, imput_shw$mean)
    imput_shw$max <- ifelse(imput_shw$category == "Over7" | imput_shw$date <= as.Date("1999-01-13"), NA, imput_shw$max)
    imput_shw$min <- ifelse(imput_shw$category == "Over7" | imput_shw$date <= as.Date("1999-01-13"), NA, imput_shw$min)# first year is all daily means data

    # make data for lm
    input <- imput_shw[,c(2:5)]
    colnames(input) <- c("date", "shw_mean", "shw_max", "shw_min")
    rv$Date <- as.Date(rv$Date)
    colnames(rv)[2] <- "date"
    dat4model <- merge(input, rv[,c(2:5)], by = "date", all.x = TRUE)
    dat4model_na <- na.omit(dat4model)

    # lm
    fit_mean <- lm(shw_mean~mean, data = dat4model_na)# Adjusted R-squared:  0.9745
    fit_max <- lm(shw_max~max, data = dat4model_na)# Adjusted R-squared: 0.9691
    fit_min <- lm(shw_min~min, data = dat4model_na)# Adjusted R-squared:  0.9713

    df_fill <- dat4model %>%
      mutate(pred_mean = predict(fit_mean, .), pred_max = predict(fit_max, .), pred_min = predict(fit_min, .))

    # check
    plot(df_fill$shw_mean, df_fill$pred_mean)
    plot(df_fill$shw_max, df_fill$pred_max)
    plot(df_fill$shw_min, df_fill$pred_min) # cluster where shw is higher than predicted, ybfmp team should investigate further when raw data is evaluated

    # add predictions
    imput_dat_Over7 <- merge(imput_shw, df_fill[,c(1,8:10)], by = "date", all = TRUE)
    imput_dat_Over7$mean <- ifelse(is.na(imput_dat_Over7$mean), imput_dat_Over7$pred_mean, imput_dat_Over7$mean)
    imput_dat_Over7$max <- ifelse(is.na(imput_dat_Over7$max), imput_dat_Over7$pred_max, imput_dat_Over7$max)
    imput_dat_Over7$min <- ifelse(is.na(imput_dat_Over7$min), imput_dat_Over7$pred_min, imput_dat_Over7$min)

    imput_dat_Over7 <- imput_dat_Over7[,-c(12,14:16)]

    # cant fill NAs greater than seven before 1999-02-17 (no rv data either)
    imput_dat_Over7$method <- ifelse(is.na(imput_dat_Over7$mean) == TRUE,
                                     NA, imput_dat_Over7$method)
    imput_dat_Over7$category <- ifelse(is.na(imput_dat_Over7$mean) == TRUE,
                                     NA, imput_dat_Over7$category)

    write.csv(imput_dat_Over7, "data_clean/clean_shw.csv", row.names = FALSE)
}

# double checking...

imput_dat_Over7 <- within(imput_dat_Over7, year <- format(imput_dat_Over7$date, "%Y"))
head(imput_dat_Over7)

shw_2007 <- subset(imput_dat_Over7, year =="2007")
plot(shw_2007$date, shw_2007$mean)
