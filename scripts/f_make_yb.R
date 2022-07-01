# make yolo bypass data using the rotary screw trap daily data, water quality collected with fish and a lm with LIS and RV data sets (LIS doesn't go back far enough, but is in the Yolo Bypass, unlike Rio Vista, which is below the confluence with the Sacramento River)

# library

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

}


