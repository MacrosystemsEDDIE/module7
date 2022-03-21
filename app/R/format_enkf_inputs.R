format_enkf_inputs <- function(siteID, neon_vars){
  
  #Drivers
  wtemp <- read_neon_data(siteID, "Surface water temperature") %>%
    dplyr::filter(value == min(value, na.rm = TRUE) ) %>%
    dplyr::rename(wtemp = V1) %>%
    dplyr::select(Date,wtemp)
  
  par <- read_neon_data(siteID, "Underwater PAR") %>%
    dplyr::rename(par = value) %>%
    dplyr::select(Date,par)
  
  #States
  chla <- read_neon_data(siteID, "Chlorophyll-a") %>%
    dplyr::rename(chla = value) %>%
    dplyr::select(Date,chla)
  
  din <- read_neon_data(siteID, "Nitrate sensor") %>%
    dplyr::rename(nitrate = value) %>%
    dplyr::select(Date, nitrate)
  
  airt <- read_neon_data(siteID, "Air temperature") %>%
    dplyr::rename(airt = value) %>%
    dplyr::select(Date, airt)
  
  swr <- read_neon_data(siteID, "Shortwave radiation") %>%
    dplyr::rename(swr = value) %>%
    dplyr::select(Date, swr)
  
  lake_data_00 <- dplyr::left_join(par,wtemp,by = "Date")
  lake_data_0 <- dplyr::left_join(lake_data_00,chla,by = "Date")
  lake_data_0 <- dplyr::left_join(lake_data_0,din,by = "Date")
  lake_data_0 <- dplyr::left_join(lake_data_0,airt,by = "Date")
  lake_data <- dplyr::left_join(lake_data_0,swr,by = "Date") %>%
    dplyr::rename(datetime = Date)
  lake_data$Date <- as.character(lake_data$datetime)
  return(lake_data)
}