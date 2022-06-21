# bring in ybfmp temperature data collected with fish data
# this will be used to fill in missing summer temperatures for yb time series

# library
require(dplyr)

f_get_WQ_w_fish <- function()
{
  # data
    enviro_dat <- read.csv("data_raw/Enviro_w_fish_98_14.csv")
    enviro_dat$date <- as.Date(enviro_dat$SampleDate)
    head(enviro_dat)
    max(enviro_dat$date)
    enviro_dat <- enviro_dat[,-c(1,2,4:7)]

  # qc
    colSums(is.na(enviro_dat)) #314 NAs
    enviro_dup <- enviro_dat[!duplicated(enviro_dat), ] #822 duplicates

    enviro_daily <- enviro_dup %>%
      group_by(date) %>%
      summarize(n())

    max(enviro_daily$`n()`) #15 (beach seine day?)

  # make daily
    cv <- function(x) 100*( sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

    enviro_98_14_daily <- enviro_dup %>%
      group_by(date) %>%
      summarise_each(funs(mean = mean(., na.rm = TRUE), max = max(., na.rm = TRUE), min = min(., na.rm = TRUE), sd = sd(., na.rm = TRUE), cv, n = sum(!is.na(.))))

  # investigate
   subset(enviro_98_14_daily, sd == 0 & n > 1)#0

  # remove zero data dates
   rstr_daily_final <- subset(rstr_daily_final, n > 0)

  # add data description columns
    enviro_98_14_daily$method <- "WQ_w_fish"
    enviro_98_14_daily$category <- "data"
    enviro_98_14_daily$site <- "yb"

  # remove zero data dates
    enviro_98_14_daily <- subset(enviro_98_14_daily, n > 0)


# combine with temperature logger at screw trap
temp_98_18_daily_master <- rbind(enviro_98_14_daily, temp_98_18_daily_final)

# NAs where some WQ but not temp taken and summer 2014
temp_98_18_daily_master <- temp_98_18_daily_master[!is.na(temp_98_18_daily_master$mean),]

write.csv(temp_98_18_daily_master, "temp_98_18_daily_master.csv")

colSums(is.na(temp_98_18_daily_master))
head(temp_98_18_daily_master)
min(temp_98_18_daily_master$date)
max(temp_98_18_daily_master$date)

sum(duplicated(temp_98_18_daily_master$date))#1138
temp_dup <- temp_98_18_daily_master[duplicated(temp_98_18_daily_master$date), ]# both logger and fish WQ on the same day

## opted to keep logger data when there was a duplicate, this can have a more complex rule set?
# create columns for IDing duplicates in both directions
temp_98_18_daily_master$dup1 <- duplicated(temp_98_18_daily_master$date)
temp_98_18_daily_master$dup2 <- duplicated(temp_98_18_daily_master$date, fromLast = TRUE)
temp_98_18_daily_master$dup <- ifelse(temp_98_18_daily_master$dup1 == TRUE | temp_98_18_daily_master$dup2 == TRUE, 1, 0)

# subset
temp_98_18_daily_master$drop <- ifelse(temp_98_18_daily_master$dup == 1 & temp_98_18_daily_master$method == "RSTR_logger", 1, 0)

temp_98_18_daily_master_dup <- subset(temp_98_18_daily_master, drop == 0)

temp_98_18_daily_master_dup <- temp_98_18_daily_master_dup[,-c(9:13)]

write.csv(temp_98_18_daily_master_dup, "temp_98_18_daily_master_du_removed.csv")


