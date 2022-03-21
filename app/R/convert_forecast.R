#' convert NOAA GEFS forecast to water temperature and uPAR forecast using
#' output from linear regression models of NEON variables
#'
#' @param start_date start date for forecast
#' @param lm_wt output of get_NEON_lm function for air temperature and water temperature
#' @param lm_upar output of get_NEON_lm function for shortwave radiation and uPAR
#' @param noaa_fc NOAA GEFS forecast with a start date equal to start_date

convert_forecast <- function(lm_wt, lm_upar, noaa_fc, start_date){
  
  m_wt <- lm_wt$m
  b_wt <- lm_wt$b
  sigma_wt <- lm_wt$sigma
  
  m_upar <- lm_upar$m
  b_upar <- lm_upar$b
  sigma_upar <- lm_upar$sigma
  
  fc_data = noaa_fc
  
  fc_idx <- fc_data[[start_date]]
  
  fc_conv_list <- lapply(1:30, function(x) {
    df <- noaa_fc$list[[start_date]]
    sub <- df[(df[, 2] %in% c("air_temperature",
                              "surface_downwelling_shortwave_flux_in_air")), c(1, 2, 2 + x)]
    df2 <- tidyr::pivot_wider(data = sub, id_cols = time, names_from = L1, values_from = 3)
    df2$air_temperature <- df2$air_temperature - 273.15
    df2$date <- as.Date(df2$time)
    df2$time <- NULL
    df3 <- plyr::ddply(df2, "date", function(y){
      colMeans(y[, 1:2], na.rm = TRUE)
    })
    # df3 <- df3[2:16, ]
    fc_out_dates <<- df3$date
    df3$wtemp <- lm_wt$m * df3$air_temperature + lm_wt$b
    df3$upar <- lm_upar$m * df3$surface_downwelling_shortwave_flux_in_air + lm_upar$b

    df3 <- df3[, c("date", "wtemp", "upar")]
    df3$fc_date <- "2020-09-25"
    # progress$set(value = x/30)
    return(df3)
  })
  
  l1 <- fc_conv_list
  idvars <- colnames(l1[[1]])
  mlt1 <- reshape::melt(l1, id.vars = idvars)
  
  return(mlt1)
}
