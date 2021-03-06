# ybfmp_temperature

The Yolo Bypass Fish Monitoring Program (YBFMP) has not yet published its water temperature data collected by loggers (Onset Computer Corp "Optic StowAway Temp" loggers) at the rotary screw trap (STTD) and Sherwood Harbor. This repository includes the raw logger data as well as relevant CDEC data (Pien et al. 2020) and water quality data collected during YBFMP fish collection (Pien and Kwan 2022) and creates a daily water temperature dataset. 

The f_get functions bring in the raw data from the data_raw folder and relevant EDI publications. The clean_ data is the final daily water temperature data for each location. Within the data_clean folder, the combined temperature logger data from the Yolo Bypass rotary screw trap and Sherwood Harbor locations is data_clean/rstr_98_18.csv and data_clean/shw_98_20.csv, respectively. Those data are made into daily QC'ed datasets - data_clean/rstr_98_18_daily_logger.csv and data_clean/SHWharbor_98_20_daily_logger.csv. A user can chose to follow the steps in the setup.Rmd to create a nearly complete daily dataset or use the raw data with different integration decsions. 

# Quick start 

setup.Rmd integrates the QC'ed daily water temperature data from each location and explains the columns 

### Lisbon Weir 
data_clean/clean_lis.csv 

### Rio Vista Bridge 
data_clean/clean_rv.csv 

### Sherwood Harbor 
data_clean/clean_shw.csv 

### Yolo Bypass
data_clean/clean_yb.csv 

Each clean_ data file includes columns date, mean, max, min, sd, cv, n, method, category, length and site.

# Data publication
