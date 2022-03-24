#' calculate RMSE of ensemble mean vs. observations 
#' 
#' @param est_out forecast output from EnKF wrapper 
#' @param lake_data NEON data for selected site formatted using format_enkf_inputs function
#' @param var Can be 'chla', 'nitrate' or 'maxUptake'

rmse <- function(est_out, lake_data, var){
  
  #get ensemble mean
  dat <- plyr::ddply(est_out[[var]]$ens, "Date", function(x) data.frame(mean = mean(x$value)))
  
  #limit obs to forecast dates
  lake_data1 <- lake_data %>%
    mutate(datetime = as.Date(datetime)) %>%
    filter(datetime %in% est_out[[var]]$dist$Date)
  
  #calculate RMSE
  val <- round(sqrt(mean((lake_data1$chla - dat$mean)^2, na.rm = TRUE)), 2)

  return(val)
}
