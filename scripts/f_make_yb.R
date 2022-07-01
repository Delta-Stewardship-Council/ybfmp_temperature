# make yolo bypass data using the rotary screw trap daily data, water quality collected with fish and a lm with LIS and RV data sets (LIS doesn't go back far enough, but is in the Yolo Bypass, unlike Rio Vista, which is below the confluence with the Sacramento River)

# library

# data
rstr <- read.csv("data_clean/rstr_98_18_daily_logger.csv")
wq <- read.csv("data_clean/WQ_w_fish_daily.csv")
lis <- read.csv("data_clean/clean_lis.csv") # lisbon weir
rv <- read.csv("data_clean/clean_rv.csv") # rio vista

# combine with temperature logger at screw trap
head(rstr)
head(wq)

yb_master <- rbind(rstr, wq)

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


