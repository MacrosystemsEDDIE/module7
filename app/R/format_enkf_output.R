#' Format EnKF output
#' 
#' @param est_out forecast output from EnKF wrapper 
#' @param lake_data NEON data for selected site formatted using format_enkf_inputs function

format_enkf_output <- function(est_out, lake_data) {
  
  lake_data <- lake_data %>%
    dplyr::mutate(datetime = as.Date(datetime)) %>%
    dplyr::filter(datetime %in% est_out$dates)
  
  out <- list(chla = list(), nitrate = list(), maxUptake = list())
  
  for(i in 1:length(out)) {
    mean_est = apply(est_out$Y[i, , ] , 1, FUN = mean)
    # Calculate distributions
    dat <- apply(est_out$Y[1, , ], 1, function(x){
      quantile(x, c(0.05, 0.5, 0.95))
    })
    dat <- as.data.frame(t(dat))
    colnames(dat) <- paste0("p", gsub("%", "", colnames(dat)))
    dat$Date <- est_out$dates
    out[[i]]$dist <- dat
    
    df2 <- as.data.frame(est_out$Y[1, , ])
    df2$Date <- est_out$dates
    out[[i]]$ens <- reshape::melt(df2, id.vars = "Date")
    
    if(i < 3) {
      out[[i]]$obs <- data.frame(Date = est_out$dates, obs = est_out$obs[i,,])
      out[[i]]$state_sd <- est_out$state_sd[i]
    }
  }
  return(out)
}
