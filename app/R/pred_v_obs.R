#' Plot predicted vs observed
#' @param obs_plot list of historical and future dataframes of observations
#' @param est_out output from `EnKF()` formatted with `format_enkf_output()`
#' @param var Can be 'chla', 'nitrate' or 'maxUptake'
#' @param add_errorbar Add errorbars to predictions. Defaults to FALSE.

pred_v_obs <- function(obs_plot, est_out, var, add_errorbar = FALSE, use_plotly = FALSE) {
  
  dat2 <- obs_plot$future
  if(var == "chla") {
    dat2$col <- "Chlorophyll-a"
    lab <- "Chlorophyll-a (μg/L)"
  } else if(var == "nitrate") {
    dat2$col <- "Nitrate"
    lab <- "Nitrate (μm L-1)"
  } else if(var == "maxUptake") {
    dat2$col <- "Max uptake"
    lab <- "Max Uptake (-)"
  }
  
  df <- merge(dat2[, c("Date", var)], est_out[[var]]$dist, by = "Date")
  mn <- plyr::ddply(est_out[[var]]$ens, "Date", function(x) data.frame(mean = mean(x$value)))
  df <- merge(df, mn, by = "Date")
  
  seg_df <- data.frame(x = c(min(df$chla, df$mean, na.rm = TRUE)), xend = max(df$chla, df$mean, na.rm = TRUE),
                       y = c(min(df$chla, df$mean, na.rm = TRUE)), yend = max(df$chla, df$mean, na.rm = TRUE))
  
  p <- ggplot(df) +
    geom_segment(data = seg_df, aes(x, y, xend = xend, yend = yend), linetype = "dashed") +
    geom_point(aes(chla, mean), size = 4) +
    {if(add_errorbar) geom_errorbar(aes(chla, ymin = p5, ymax = p95))} +
    xlab(paste0("Observed - ", lab)) +
    ylab(paste0("Predicted - ", lab)) +
    coord_equal() +
    theme_bw(base_size = 12)
  
  
  # p <- p +
  #   scale_color_manual(values = c("Chlorophyll-a" = cols[1], "Nitrate" = cols[7], "Max uptake" = cols[4],
  #                                 "Member" = l.cols[8], "median" = "black", "95%" = l.cols[8], "Obs" = p.cols[6]))
  if(use_plotly) {
    gp <- ggplotly(p, dynamicTicks = FALSE)
    for (i in 1:length(gp$x$data)){
      if (!is.null(gp$x$data[[i]]$name)){
        gp$x$data[[i]]$name =  gsub("\\(","", stringr::str_split(gp$x$data[[i]]$name,",")[[1]][1])
      }
    }
    return(gp)
  } else {
    p <- p +
      theme_bw(base_size = 22)
    return(p)
  }
}
