# get hourly CDEC data (that has been qc) from EDI (Hourly water temperature from the San Francisco Estuary, 1986 - 2019, https://portal.edirepository.org/nis/mapbrowse?packageid=edi.591.2)

# library
library(contentid)

f_get_hourly <- function() {

  # data:
    filtered_data_URL = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.591.2&entityid=fb147771773b8354667a0b43e3f8e0e4"
    stations_URL = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.591.2&entityid=a059f5eea4f8500fe1a43566451ec593"

  # store hash values:
    hourly_temp_data_id <- contentid::store(filtered_data_URL)
    hourly_temp_stations_id <- contentid::store(stations_URL)

  # retrieve data using hash:
    hourly_temp_data <- read.csv(contentid::retrieve(hourly_temp_data_id))
    hourly_temp_stations <- read.csv(contentid::retrieve(hourly_temp_stations_id))

  # check datetime and data formats
    str(hourly_temp_data)
    hourly_temp_data$Date <- as.Date(hourly_temp_data$Date)
    unique(hourly_temp_data$Station)

  # dates and stations of interest
    hourly_temp_data <- subset(hourly_temp_data, Date >= as.Date("1998-01-16") & Date <= as.Date("2019-12-31"))
  # Rio Vista and Lisbon Weir
    hourly_data <- subset(hourly_temp_data, Station == "LIS" | Station == "RIV" | Station == "RVB")

    write.csv(hourly_data, "data_clean/hourly_CDEC_stations.csv")

}
