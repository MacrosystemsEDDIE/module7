#' Plot EnKF output
#' @param obs_plot list of historical and future dataframes of observations
#' @param start_date forecast date in "YYYY-mm-dd" format
#' @param plot_type either 'Line' or 'Distribution'
#' @param est_out output from `EnKF()` formatted with `format_enkf_output()`
#' @param var Can be 'chla', 'nitrate' or 'maxUptake'

plot_enkf_out <- function(obs_plot, start_date, plot_type, est_out, var) {
  
  dat2 <- obs_plot$hist
  if(var == "chla") {
    dat2$col <- "Chlorophyll-a"
    y_lab <- "Chlorophyll-a (μg/L)"
  } 
  if(var == "nitrate") {
    dat2$col <- "Nitrate"
    y_lab <- "Nitrate (μm L-1)"
  } 
  if(var == "maxUptake") {
    dat2$col <- "Max uptake"
    y_lab <- "Max Uptake (-)"
  }
  
  
  p <- ggplot() +
    geom_point(data = dat2, aes_string("Date", var, color = "col")) +
    geom_vline(xintercept = as.Date(start_date), linetype = "dashed") +
    ylab(y_lab) +
    xlab("Date") +
    theme_bw(base_size = 12)
  
  if(plot_type == "Line") {
    p <- p +
      geom_line(data = est_out[[var]]$ens, aes(Date, value, group = variable, color = "Member"), color = "gray", alpha = 0.6)
  } else if (plot_type == "Distribution") {
    p <- p +
      geom_ribbon(data = est_out[[var]]$dist, aes(Date, ymin = p5, ymax = p95, fill = "95%"), alpha = 0.3) +
      geom_line(data = est_out[[var]]$dist, aes(Date, p50, color = "median"))
  }
  
  if(var != "maxUptake") {
    dat3 <- est_out[[var]]$obs
    dat3$col <- "Obs"
    p <- p +
      geom_errorbar(data = dat3, aes(Date, ymin = obs - est_out[[var]]$state_sd, ymax = obs + est_out[[var]]$state_sd,
                                                   width = 1)) +
      geom_point(data = dat3, aes_string("Date", "obs", color = "col"))
  }
  
  p <- p +
    scale_color_manual(values = c("Chlorophyll-a" = cols[1], "Nitrate" = cols[7], "Max uptake" = cols[4],
                                  "Member" = l.cols[8], "median" = "black", "95%" = l.cols[8], "Obs" = p.cols[6]))
  
  gp <- ggplotly(p, dynamicTicks = TRUE)
  for (i in 1:length(gp$x$data)){
    if (!is.null(gp$x$data[[i]]$name)){
      gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
    }
  }
  return(gp)
}