#' Plot EnKF output
#' @param no_da output from `EnKF()` formatted with `format_enkf_output()` for No DA
#' @param chla output from `EnKF()` formatted with `format_enkf_output()` for No Chl-a
#' @param nitrate output from `EnKF()` formatted with `format_enkf_output()` for nitrate
#' @param both output from `EnKF()` formatted with `format_enkf_output()` for both
#' @param var Can be 'chla', 'nitrate' or 'maxUptake'
#' @param obs_plot list of historical and future dataframes of observations
#' @param add_obs Add future observations. Defaults to FALSE

plot_four_forecasts <- function(no_da, chla, nitrate, both, var = "chla", obs_plot, add_obs = FALSE) {
  
  if(var == "chla") {
    y_lab <- "Chlorophyll-a (μg/L)"
  } else if(var == "nitrate") {
    y_lab <- "Nitrate (μm L-1)"
  } else if(var == "maxUptake") {
    y_lab <- "Max Uptake (-)"
  }
  
  
  p <- ggplot() +
    ylab(y_lab) +
    xlab("Date") +
    {if(!is.null(no_da[[var]]$dist)) geom_ribbon(data = no_da[[var]]$dist, aes(Date, ymin = p5, ymax = p95, fill = "No DA"), alpha = 0.2)} +
    {if(!is.null(chla[[var]]$dist)) geom_ribbon(data = chla[[var]]$dist, aes(Date, ymin = p5, ymax = p95, fill = "Chl-a"), alpha = 0.2)} +
    {if(!is.null(nitrate[[var]]$dist)) geom_ribbon(data = nitrate[[var]]$dist, aes(Date, ymin = p5, ymax = p95, fill = "Nitrate"), alpha = 0.2)} +
    {if(!is.null(both[[var]]$dist)) geom_ribbon(data = both[[var]]$dist, aes(Date, ymin = p5, ymax = p95, fill = "Both"), alpha = 0.2)} +
    theme_bw(base_size = 12)
  p
  
  if(add_obs) {
    dat3 <- obs_plot$future
    if(var == "chla") {
      dat3$col <- "Chlorophyll-a"
    } else if(var == "nitrate") {
      dat3$col <- "Nitrate"
    } else if(var == "maxUptake") {
      dat3$col <- "Max uptake"
    }
    p <- p +
      geom_point(data = dat3, aes_string("Date", var, color = "col"))
  }
  
  # if(var != "maxUptake") {
  #   if(!is.null(no_da[[var]]$dist)) dat3 <- no_da[[var]]$obs
  #   if(!is.null(chla[[var]]$dist)) dat3 <- chla[[var]]$obs
  #   if(!is.null(nitrate[[var]]$dist)) dat3 <- nitrate[[var]]$obs
  #   if(!is.null(both[[var]]$dist)) dat3 <- both[[var]]$obs
  #   
  #   dat3[[var]][dat3$Date > (as.Date(start_date) + n_days)] <- NA
  #   dat3$col <- "Assimilated"
  #   p <- p +
  #     geom_errorbar(data = dat3, aes(Date, ymin = obs - est_out[[var]]$state_sd, ymax = obs + est_out[[var]]$state_sd,
  #                                    width = 1)) +
  #     geom_point(data = dat3, aes_string("Date", "obs", color = "col"))
  # }
  
  p <- p +
    scale_color_manual(values = c("Chlorophyll-a" = cols[1], "Nitrate" = cols[7], "Max uptake" = cols[4],
                                  "Member" = l.cols[8], "median" = "black", "95%" = p.cols[5], "75%" = p.cols[6], "Obs" = p.cols[4],
                                  "Assimilated" = cols[4])) +
    labs(fill = "DA method") +
    da_method_fill_scale
  
  gp <- ggplotly(p, dynamicTicks = TRUE)
  for (i in 1:length(gp$x$data)){
    if (!is.null(gp$x$data[[i]]$name)){
      gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
    }
  }
  return(gp)
}