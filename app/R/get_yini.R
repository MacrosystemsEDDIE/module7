#' specify initial conditions as either the observation from the first day of the forecast or the most recent observation
#' @param lake_data NEON lake dataset that has been formatted using the format_enkf_inputs function
#' @param start_date start date of forecast (either 2020-09-25 or 2020-10-02)
get_yini <- function(lake_data, start_date){
  
  yini <- c(NA,NA)
  
  lake_data$datetime = as.Date(lake_data$datetime)
  
  closest<-function(xv, sv){
    xv[which.min(xv-sv)]}
  
  if(is.na(lake_data[lake_data[, "datetime"] == as.Date(start_date),"chla"])){
    startrow <- which(lake_data[, "datetime"] == as.Date(start_date))
    NotNA <- lake_data %>% 
      dplyr::mutate(rownum = c(1:length(lake_data$chla))) %>% 
      dplyr::filter(!is.na(chla) & datetime < start_date)
    yinirow <- which.min(abs(startrow-NotNA$rownum))
    yini[1] <- NotNA[yinirow,"chla"]
  } else {
    yini[1] <- lake_data[lake_data[, "datetime"] == as.Date(start_date),"chla"]
  }
  
  if(is.na(lake_data[lake_data[, "datetime"] == as.Date(start_date),"nitrate"])){
    startrow <- which(lake_data[, "datetime"] == as.Date(start_date))
    NotNA <- lake_data %>% 
      dplyr::mutate(rownum = c(1:length(lake_data$nitrate))) %>% 
      dplyr::filter(!is.na(nitrate) & datetime < start_date)
    yinirow <- which.min(abs(startrow-NotNA$rownum))
    yini[2] <- NotNA[yinirow,"nitrate"]
  } else {
    yini[2] <- lake_data[lake_data[, "datetime"] == as.Date(start_date),"nitrate"]
  }
  
  return(yini)
}