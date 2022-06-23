# make Sherwood Harbor logger data from YBFMP daily and fill in missing data

# library
library(dplyr)
library(imputeTS)

f_make_shw <- function() {
  # data
    shw_98_20 <- read.csv("data_clean/shw_98_18.csv")
    rv <- read.csv("data_clean/clean_rv.csv")

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
    shw_daily$site <- "shw_harbor"

    # need to pull out max and min that doesn't apply
    shw_daily$max <- ifelse(shw_daily$method == "daily_mean", "NA", shw_daily$max)
    shw_daily$min <- ifelse(shw_daily$method == "daily_mean", "NA", shw_daily$min)

    write.csv(shw_daily, "SHWharbor_98_20_daily_logger.csv", row.names = FALSE)

    #

}
