# make yolo bypass data using the rotary screw trap daily data, water quality collected with fish and a lm with LIS and RV data sets (LIS doesn't go back far enough, but is in the Yolo Bypass, unlike Rio Vista, which is below the confluence with the Sacramento River)

# library
library(imputeTS)
library(dplyr)

f_get_hourly <- function() {

  # data
    rstr <- read.csv("data_clean/rstr_98_18_daily_logger.csv")
    wq <- read.csv("data_clean/WQ_w_fish_daily.csv")
    lis <- read.csv("data_clean/clean_lis.csv") # lisbon weir
    rv <- read.csv("data_clean/clean_rv.csv") # rio vista

  # combine with temperature logger at screw trap
    head(rstr)
    head(wq)

    yb_master <- rbind(rstr, wq)
    yb_master$date <- as.Date(yb_master$date)

  # opted to keep logger data when there was a duplicate, substitute wq for NAs only
  # create columns for IDing duplicates in both directions
    yb_master$dup1 <- duplicated(yb_master$date)
    yb_master$dup2 <- duplicated(yb_master$date, fromLast = TRUE)
    yb_master$dup <- ifelse(yb_master$dup1 == TRUE | yb_master$dup2 == TRUE, 1, 0)

  # subset
    yb_master$drop <- ifelse(yb_master$dup == 1 & yb_master$method == "WQ_w_fish", 1, 0)

    yb_master_dup <- subset(yb_master, drop == 0)

    yb_master_dup <- yb_master_dup[,-c(10:14)]

    write.csv(yb_master_dup, "data_clean/yb_98_18_daily_combo.csv")

  # impute NAs within seven days of consecutive NAs
    continous_dates <- data.frame (x = 1:8020, date = seq(as.Date('1998-01-16'),as.Date('2019-12-31'),by='day')) #

    yb_cont <- merge(yb_master_dup, continous_dates, by = "date", all.y = TRUE)

  # separate
    yb_cont_data <- yb_cont[!is.na(yb_cont$mean),]
    yb_cont_NA <- yb_cont[is.na(yb_cont$mean),]
    head(yb_cont_NA) # 2882 days

  # assigns id to consecutive date groups
    yb_cont_NA$group <- cumsum(c(1, diff.Date(yb_cont_NA$date)) >= 2)

    yb_cont_NA_sum <- yb_cont_NA %>%
      group_by(group) %>%
      summarise(length = length(group)) %>%
      as.data.frame(yb_cont_NA_sum)

    yb_cont_NA_complete <- merge(yb_cont_NA, yb_cont_NA_sum, by="group", all.x = TRUE)
    yb_cont_NA_complete <- transform(yb_cont_NA_complete, category = ifelse(length > 7, "Over7", "7&Under"))

    head(yb_cont_NA_complete)

    yb_cont_data$length = NA
    yb_cont_data$group = NA

    imput_yb <- rbind(yb_cont_NA_complete, yb_cont_data)
    imput_yb <- imput_yb[order(imput_yb$date),]

    # 7 day moving average
    imput_yb$mean <- na_ma(imput_yb$mean, k = 7, weighting = "exponential", maxgap = Inf)
    imput_yb$max <- na_ma(imput_yb$max, k = 7, weighting = "exponential", maxgap = Inf)
    imput_yb$min <- na_ma(imput_yb$min, k = 7, weighting = "exponential", maxgap = Inf)

    imput_yb$method <- ifelse(imput_yb$category == "7&Under", "imputeTS", imput_yb$method)

    # need to remove data that is not within parameters
    imput_yb$mean <- ifelse(imput_yb$category == "Over7", NA, imput_yb$mean)
    imput_yb$max <- ifelse(imput_yb$category == "Over7" | imput_yb$method == "daily_mean", NA, imput_yb$max)
    imput_yb$min <- ifelse(imput_yb$category == "Over7" | imput_yb$method == "daily_mean", NA, imput_yb$min)

    # make data for lm
    input <- imput_yb[,c(2:5)]
    colnames(input) <- c("date", "yb_mean", "yb_max", "yb_min")

    colnames(lis)[2] <- "date"
    lis$date <- as.Date(lis$date)
    lis_input <- merge(input, lis[,c(2:5)], by = "date", all = TRUE)
    lis_input <- na.omit(lis_input)
    min(lis$date)# 2008-07-11
    max(lis$date)# 2019-12-31

    fit_lis_mean <- lm(yb_mean~mean, data = lis_input)# Adjusted R-squared:  0.7967
    fit_lis_max <- lm(yb_max~max, data = lis_input)# Adjusted R-squared: 0.7838
    fit_lis_min <- lm(yb_min~min, data = lis_input)# Adjusted R-squared: 0.7958

    lis_input <- merge(input, lis[,c(2:5)], by = "date", all = TRUE) # can't have na.omit for this step
    lis_fill <- lis_input %>%
      mutate(pred_mean = predict(fit_lis_mean, .), pred_max = predict(fit_lis_max, .), pred_min = predict(fit_lis_min, .))

    # check
    plot(lis_fill$yb_mean, lis_fill$pred_mean)
    plot(lis_fill$yb_max, lis_fill$pred_max)
    plot(lis_fill$yb_min, lis_fill$pred_min) # some noise to be expected when flooding occurs

    # add predictions
    imput_dat_Over7 <- merge(imput_yb, lis_fill[,c(1,8:10)], by = "date", all = TRUE)

    imput_dat_Over7$method <- ifelse(is.na(imput_dat_Over7$mean) & !is.na(imput_dat_Over7$pred_mean), "lm_lis", imput_dat_Over7$method)

    imput_dat_Over7$mean <- ifelse(is.na(imput_dat_Over7$mean), imput_dat_Over7$pred_mean, imput_dat_Over7$mean)
    imput_dat_Over7$max <- ifelse(is.na(imput_dat_Over7$max), imput_dat_Over7$pred_max, imput_dat_Over7$max)
    imput_dat_Over7$min <- ifelse(is.na(imput_dat_Over7$min), imput_dat_Over7$pred_min, imput_dat_Over7$min)

    # remaining dates need other data source - rv

    colnames(rv)[2] <- "date"
    rv$date <- as.Date(rv$date)
    rv_input <- merge(input, rv[,c(2:5)], by = "date", all = TRUE)
    rv_input <- na.omit(rv_input)

    fit_rv_mean <- lm(yb_mean~mean, data = rv_input)# Adjusted R-squared:  0.8106
    fit_rv_max <- lm(yb_max~max, data = rv_input)# Adjusted R-squared: 0.8072
    fit_rv_min <- lm(yb_min~min, data = rv_input)# Adjusted R-squared: 0.7983

  # remaining NAs
    input_rv <- subset(imput_dat_Over7, is.na(mean))
    input_rv <- input_rv[,c(1,3:5)]
    colnames(input_rv) <- c("date", "yb_mean", "yb_max", "yb_min")

    rv_input <- merge(input_rv, rv[,c(2:5)], by = "date", all = TRUE)

    rv_fill <- rv_input %>%
      mutate(pred_mean = predict(fit_rv_mean, .), pred_max = predict(fit_rv_max, .), pred_min = predict(fit_rv_min, .))

  # add predictions
    imput_dat_Over7 <- imput_dat_Over7[,-c(13:15)]
    imput_dat_Over7_rv <- merge(imput_dat_Over7, rv_fill[,c(1,8:10)], by = "date", all = TRUE)

    imput_dat_Over7_rv$method <- ifelse(is.na(imput_dat_Over7_rv$mean) & !is.na(imput_dat_Over7_rv$pred_mean), "lm_rv", imput_dat_Over7_rv$method)

    imput_dat_Over7_rv$mean <- ifelse(is.na(imput_dat_Over7_rv$mean), imput_dat_Over7_rv$pred_mean, imput_dat_Over7_rv$mean)
    imput_dat_Over7_rv$max <- ifelse(is.na(imput_dat_Over7_rv$max), imput_dat_Over7_rv$pred_max, imput_dat_Over7_rv$max)
    imput_dat_Over7_rv$min <- ifelse(is.na(imput_dat_Over7_rv$min), imput_dat_Over7_rv$pred_min, imput_dat_Over7_rv$min)


    sum(is.na(imput_dat_Over7_rv$mean)) # still 188

    write.csv(imput_dat_Over7_rv[,c(1:10,12)], "data_clean/clean_yb.csv", row.names = FALSE)
    }


