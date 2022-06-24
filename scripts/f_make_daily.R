# make rstr and sherwood harbor ybfmp data daily and qc

# library
require(dplyr)

f_make_daily <- function()
    {
  # data
    rstr_98_18 <- read.csv("data_clean/rstr_98_18.csv")
    rstr_98_18$date <- as.Date(rstr_98_18$date)

  # check for duplicates
    rstr_98_18_dup <- rstr_98_18[!duplicated(rstr_98_18), ] # 91566 duplicates

    # issues with 2013
    duplicates_2013 <- subset(rstr_98_18, date >= '2013-01-01' & date <= '2013-12-31')#67856
    duplicates_2013_rm <- duplicates_2013[!duplicated(duplicates_2013), ]
    # the rest are NA dates
    exclude = seq(as.Date('2013-01-01'),as.Date('2013-12-31'),by='day')
    rstr_qc_03 = rstr_98_18[!(rstr_98_18$date %in% exclude), ]
    rstr_98_18_new <- rbind(rstr_qc_03, duplicates_2013_rm)

  # summarize to lowest common denominator (daily)
  # (true) interval (e.g., how many measurements per day)
    rstr_daily <- rstr_98_18_new %>%
      group_by(date) %>%
      summarize(n())

  # check against expectations
    time.check = seq(as.Date('1998-01-23'),as.Date('2018-06-08'),by='day') # 3746 dates are missing

    duplicates <- subset(rstr_daily, `n()`>96) # shouldn't be greater than 96 (15 minute interval)

  # make daily
    cv <- function(x) 100*( sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

    rstr_daily <- rstr_98_18_new[,-2] %>%
      group_by(date) %>%
      summarise_each(funs(mean = mean(., na.rm = TRUE), max = max(., na.rm = TRUE), min = min(., na.rm = TRUE), sd = sd(., na.rm = TRUE), cv, n = sum(!is.na(.))))

  # investigate
  # 2018, SD and CV are zero, the 15 minute intervals are repeats of the same value (after Jan 30)
    temp_weird <- subset(rstr_daily, sd == 0 & n > 1) # also 2000-01-24, 2001-12-28, 2005-01-22, 2006-01-13 (from what I can tell these are not substitutions from fish sampling)

    rstr_daily_2018 <- subset(rstr_daily, date <= '2018-01-30')

    rstr_daily_final <- subset(rstr_daily_2018, date != "2000-01-24" & date != "2001-12-28" & date != "2005-01-22" & date != "2006-01-13")

  # remove zero data dates
    rstr_daily_final <- subset(rstr_daily_final, n > 0)

  # add data description columns
    rstr_daily_final <- transform(rstr_daily_final, method = ifelse(n > 1, "ybfmp_logger", "daily_mean"))
    rstr_daily_final$category <- "data"
    rstr_daily_final$site <- "STTD"

  # need to pull out max and min that doesn't apply
    rstr_daily_final$max <- ifelse(rstr_daily_final$method == "daily_mean", "NA", rstr_daily_final$max)
    rstr_daily_final$min <- ifelse(rstr_daily_final$method == "daily_mean", "NA", rstr_daily_final$min)

    write.csv(rstr_daily_final, "data_clean/rstr_98_18_daily_logger.csv", row.names = FALSE)
}

