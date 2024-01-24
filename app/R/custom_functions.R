# Functions needed to complete Module 7: Using Data to Improve Ecological Forecasts
# Author: Mary Lofton
# Date: 05Jan24

# Load packages
library(tidyverse)
library(lubridate)
library(zoo)
library(mvtnorm)
library(see) #see annotation below on how to avoid using this package if it becomes an issue

##Notes on see package:
#' This is literally only used for one plotting command: geom_violinhalf().
#' To completely avoid use of this package, ctrl+F for geom_violinhalf() and
#' replace with geom_violin() - VOILA!

## Define functions----

EnKF <- function(forecast, new_observation, ic_sd){
  
  #Allocate matrices - everything has to be converted to 
  #a matrix to do the matrix math required for the ensemble Kalman filter
  x_corr <- matrix(forecast)
  y <- matrix(new_observation)
  h_matrix <- matrix(0, nrow = 1, ncol = 1)
  R_matrix <- matrix(0, nrow = 1, ncol = 1)
  dit <- matrix(NA, nrow = length(x_corr[,1]), ncol = 1) 
  y_corr <- matrix(NA, nrow =  length(x_corr[,1]), ncol = length(y))
  x_update <- matrix(NA, nrow = length(x_corr[,1]), ncol = 1)
  
  #Only do EnKF if observations are present that day
  #there has to be at least 1 non-NA observation.
  if(length(which(!is.na(y))) > 0){
    
    #Assign observations to depths
    h_matrix[1, 1] <- 1

    #Create observational uncertainty matrix
    R_matrix[1,1] <- ic_sd^2

    #Calculate mean prediction for each depth
    ens_mean <- colMeans(x_corr)
    
    #Loop through ensemble members
    for(m in 1:length(x_corr[,1])){  
      #Ensemble specific deviation
      dit[m, ] <- x_corr[m, ] - ens_mean
      
      #if the first ensemble then create the matrix that is then averaged
      if(m == 1){
        p_it <- dit[m, ] %*% t(dit[m, ]) 
      }else{
        #if not the first ensemble then add the matrix to the previous matrix
        p_it <- dit[m, ] %*% t(dit[m, ]) +  p_it 
      }
    }
    
    #Calculate Cxx matrix
    Cxx_matrix <- p_it / (length(x_corr[,1]) - 1)
    
    #Add noise to observations
    for(m in 1:length(x_corr[,1])){
      y_corr[m, ] <- y + t(rmvnorm(n = 1, mean = c(0), sigma = R_matrix))
    }
    
    #Calculate Kalman Gain
    K <- Cxx_matrix %*% t(h_matrix) %*% solve(h_matrix %*% Cxx_matrix %*% t(h_matrix) + R_matrix)
    
    #Update model states based on Kalman Gain and devivations
    for(m in 1:length(x_corr[,1])){
      x_update[m, ] <- x_corr[m,] + K %*% (y_corr[m,] - h_matrix %*% x_corr[m,])
    }
  }else{
    #Only add noise if observations are missing
    x_update <- x_corr
  }
  
  ic_update <- c(x_update[,1])
  return(ic_update)
}

### Plotting functions ----
#### Function to plot NEON chl-a data----

# Plot chl-a data + 1 day lag: timeseries
#' @param plot_data chlorophyll-a dataframe with datetime, chla and chla_lag

plot_chla_lag <- function(plot_data){
  p <- ggplot(data = plot_data)+
    geom_line(aes(x = datetime, y = chla_lag, color = "1 day lag of chlorophyll"))+
    geom_line(aes(x = datetime, y = chla, color = "chlorophyll"))+
    xlab("2018")+
    ylab("Chlorophyll-a (ug/L)")+
    scale_color_manual(values = c("chlorophyll" = "darkgreen","1 day lag of chlorophyll" = "lightgreen"))+
    labs(color = NULL)+
    theme_bw()+
    theme(legend.position = "bottom")
  return(p)
}

#### Function to plot AR model fit ----
#' @param model_fit_plot_data data frame of lake observations and model predictions
#'  
plot_mod_predictions <- function(model_fit_plot_data, variable_name){
  cols <- RColorBrewer::brewer.pal(8, "Dark2") # Set custom color palette for our plot - ooh we are fancy!! :-)
  
  ggplot(data = model_fit_plot_data) +
    geom_point(aes(date, chla, color = "Observed")) +
    geom_line(aes(date, model, color = "Modeled")) +
    ylab(variable_name) +
    xlab("Time") +
    scale_color_manual(values = c( "Observed" = "black", "Modeled" = cols[4]),
                       name = "",
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid","blank"),
                         shape = c(NA,16)))) +
    theme_bw(base_size = 12) 
}

#### Function to plot distribution of initial conditions ----
#'@param curr_chla mean initial condition of chla
#'@param ic_uc vector draws from a distribution of initial conditions
#'
plot_ic_dist <- function(curr_chla, ic_uc){
  
  #Set colors
  l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)] # Defining another custom color palette :-)
  
  #Build plot
  ggplot() +
    geom_vline(xintercept = curr_chla) +
    geom_density(aes(ic_uc), fill = l.cols[2], alpha = 0.3) +
    xlab("Chlorophyll-a (ug/L)")+
    ylab("Density") +
    theme_bw(base_size = 18)+
    ggtitle("Initial condition distribution")
}

#### Function to plot distribution of process uncertainty ----
#'@param proc_uc vector draws from a distribution of proc uc
#'
plot_process_dist <- function(proc_uc){
  
  #Set colors
  l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)] # Defining another custom color palette :-)
  
  #Build plot
  ggplot() +
    geom_vline(xintercept = 0) +
    geom_density(aes(proc_uc), fill = l.cols[1], alpha = 0.3) +
    xlab("Chlorophyll-a (ug/L)") +
    ylab("Density") +
    theme_bw(base_size = 18)+
    ggtitle("Process uncertainty distribution")
}

#### Function to plot distribution of a forecast ----
#'@param forecast_dist vector of forecast distribution
#'
plot_fc_dist <- function(forecast_dist){
  
  #Set colors
  l.cols <- RColorBrewer::brewer.pal(8, "Set2")[-c(1, 2)] # Defining another custom color palette :-)
  #Build plot
  ggplot() +
    geom_density(aes(forecast_dist), fill = l.cols[3], alpha = 0.3) +
    xlab("Chlorophyll-a (ug/L)") +
    ylab("Density") +
    theme_bw(base_size = 18)+
    ggtitle("Chl-a forecast distribution")
}



#### Functions to plot chl-a forecast----

#' One-day forecast plot
#' 

plot_fc_1day <- function(curr_chla, start_date, forecast_date, ic_distribution, forecast_chla, n_members){
  
  ens <- tibble(date = c(rep(start_date, times = length(ic_distribution)),
                         rep(forecast_date, times = length(forecast_chla))),
                ens = c(ic_distribution, forecast_chla),
                ensemble_member = rep(1:n_members, times = 2))
  ic <- ens %>%
    filter(date == start_date)
  fc <- ens %>%
    filter(date == forecast_date)
  obs <- tibble(date = start_date,
                obs = curr_chla)
  
  p <- ggplot()+
    geom_line(data = ens, aes(x = date, y = ens, group = ensemble_member, color = "Ensemble members"))+
    geom_violinhalf(data = fc, aes(x = date, y = ens, fill = "Forecast"), color = "black",
                scale = "width", width = 0.7)+
    geom_violinhalf(data = ic, aes(x = date, y = ens, fill = "Initial condition"), color = "cornflowerblue", alpha = 0.4, scale = "width", width = 0.7)+
    geom_point(data = obs, aes(x = date, y = obs, color = "Observation"), size = 3)+
    ylab("Chlorophyll-a (ug/L)")+
    xlab("")+
    theme_bw(18)+
    theme(panel.grid.major.x = element_line(colour = "black", linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_color_manual(values = c("Ensemble members" = "lightgray",
                                  "Observation" = "orange"), 
                       name = "",
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid","blank"),
                         shape = c(NA, 16))))+
    scale_fill_manual(values = c("Forecast" = "white", "Initial condition" = "cornflowerblue"),
                      name = "",
                      guide = guide_legend(override.aes = list(
                        color = c("black","cornflowerblue"))))+
    ggtitle("1-day-ahead forecast")
  
  return(p)
}

#' One-day forecast plot with new observation
#' 
plot_fc_new_obs <- function(previous_plot, new_obs, forecast_date){
  
  obs <- tibble(date = forecast_date,
                      obs = new_obs)
  p <- previous_plot +
    geom_point(data = obs, aes(x = date, y = obs, color = "Observation"), size = 3)
}

#' One-day forecast plot with updated initial condition
#' 

plot_fc_update <- function(chla_obs, start_date, forecast_date, ic_distribution, ic_update, forecast_chla, n_members){
  
  ens <- tibble(date = c(rep(start_date, times = length(ic_distribution)),
                         rep(forecast_date, times = length(forecast_chla)*2)),
                ens = c(ic_distribution, forecast_chla, ic_update),
                ensemble_member = rep(1:n_members, times = 3),
                data_type = c(rep("ic", times = length(ic_distribution)),
                              rep("fc", times = length(forecast_chla)),
                              rep("ic", times = length(ic_update))))
  ic <- ens %>%
    filter(data_type == "ic")
  fc <- ens %>%
    filter(data_type == "fc")
  obs <- tibble(date = c(start_date, forecast_date),
                obs = chla_obs)
  
  p <- ggplot()+
    geom_line(data = ens, aes(x = date, y = ens, group = ensemble_member, color = "Ensemble members"))+
    geom_violinhalf(data = fc, aes(x = date, y = ens, fill = "Forecast"), color = "black",
                scale = "width", width = 0.7)+
    geom_violinhalf(data = ic, aes(x = date, y = ens, fill = "Initial condition"), color = "cornflowerblue", alpha = 0.4, scale = "width", width = 0.7)+
    geom_point(data = obs, aes(x = date, y = obs, color = "Observation"), size = 3)+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    xlab("")+
    theme_bw(18)+
    theme(panel.grid.major.x = element_line(colour = "black", linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_color_manual(values = c("Ensemble members" = "lightgray",
                                  "Observation" = "orange"), 
                       name = "",
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid","blank"),
                         shape = c(NA, 16))))+
    scale_fill_manual(values = c("Forecast" = "white", "Initial condition" = "cornflowerblue"),
                      name = "",
                      guide = guide_legend(override.aes = list(
                        color = c("black","cornflowerblue"))))+
    ggtitle("1-day-ahead forecast")
  
  return(p)
}

#' Two-day forecast plot with updated initial condition
#' 
plot_second_forecast <- function(chla_obs, start_date, forecast_dates, ic_distribution, ic_update, forecast_chla, second_forecast, n_members){
  
  ens <- tibble(date = c(rep(start_date, times = length(ic_distribution)),
                         rep(forecast_dates[1], times = length(forecast_chla)*2),
                         rep(forecast_dates[2], times = length(second_forecast))),
                ens = c(ic_distribution, forecast_chla, ic_update, second_forecast),
                ensemble_member = rep(1:n_members, times = 4),
                data_type = c(rep("ic", times = length(ic_distribution)),
                              rep("fc", times = length(forecast_chla)),
                              rep("ic", times = length(ic_update)),
                              rep("fc", times = length(second_forecast))))
  ic <- ens %>%
    filter(data_type == "ic")
  fc <- ens %>%
    filter(data_type == "fc")
  obs <- tibble(date = c(start_date, forecast_dates[1]),
                obs = chla_obs)
  
  p <- ggplot()+
    geom_line(data = ens, aes(x = date, y = ens, group = ensemble_member, color = "Ensemble members"))+
    geom_violinhalf(data = fc, aes(x = date, y = ens, fill = "Forecast"), color = "black",
                    scale = "width", width = 0.7)+
    geom_violinhalf(data = ic, aes(x = date, y = ens, fill = "Initial condition"), color = "cornflowerblue", alpha = 0.4, scale = "width", width = 0.7)+
    geom_point(data = obs, aes(x = date, y = obs, color = "Observation"), size = 3)+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    xlab("")+
    theme_bw(18)+
    theme(panel.grid.major.x = element_line(colour = "black", linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_color_manual(values = c("Ensemble members" = "lightgray",
                                  "Observation" = "orange"), 
                       name = "",
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid","blank"),
                         shape = c(NA, 16))))+
    scale_fill_manual(values = c("Forecast" = "white", "Initial condition" = "cornflowerblue"),
                      name = "",
                      guide = guide_legend(override.aes = list(
                        color = c("black","cornflowerblue"))))+
    ggtitle("Two 1-day-ahead forecasts")
  
  return(p)
}

#' Multiple-day forecast plot with updated initial conditions
#' 
plot_many_forecasts <- function(forecast_data, forecast_series){

  forecast_dates <- unique(forecast_series$date)
  forecast_series <- forecast_series %>%
    mutate(datefactor = as.factor(format(date, "%m-%d")),
           chla = ifelse((date == last(forecast_dates) & data_type == "ic"),NA,chla))
  ic <- forecast_series %>%
    filter(data_type == "ic")
  fc <- forecast_series %>%
    filter(data_type == "fc")
  obs <- tibble(date = forecast_data$datetime,
                obs = forecast_data$chla) %>%
    mutate(datefactor = as.factor(format(date, "%m-%d")),
           obs = ifelse(date == last(forecast_dates),NA,obs))
  
  p <- ggplot()+
    geom_line(data = forecast_series, aes(x = datefactor, y = chla, group = ensemble_member, color = "Ensemble members"))+
    geom_violinhalf(data = fc, aes(x = datefactor, y = chla, fill = "Forecast"), color = "black",
                    scale = "width", width = 0.7)+
    geom_violinhalf(data = ic, aes(x = datefactor, y = chla, fill = "Initial condition"), color = "cornflowerblue", alpha = 0.4, scale = "width", width = 0.7)+
    geom_point(data = obs, aes(x = datefactor, y = obs, color = "Observations"), size = 3)+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    xlab("")+
    theme_bw(18)+
    theme(panel.grid.major.x = element_line(colour = "black", linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_color_manual(values = c("Ensemble members" = "lightgray",
                                  "Observations" = "orange"), 
                       name = "",
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid","blank"),
                         shape = c(NA, 16))))+
    scale_fill_manual(values = c("Forecast" = "white", "Initial condition" = "cornflowerblue"),
                      name = "",
                      guide = guide_legend(override.aes = list(
                        color = c("black","cornflowerblue"))))+
    ggtitle("A series of 1-day-ahead forecasts")
  
  return(suppressWarnings(print(p)))
}

#' Multiple-day forecast plot with updated initial condition and all observations plotted
#' 
plot_many_forecasts_with_obs <- function(forecast_data, forecast_series, observations){
  
  forecast_dates <- unique(forecast_series$date)
  forecast_series <- forecast_series %>%
    mutate(datefactor = as.factor(format(date, "%m-%d")),
           chla = ifelse((date == last(forecast_dates) & data_type == "ic"),NA,chla))
  ic <- forecast_series %>%
    filter(data_type == "ic")
  fc <- forecast_series %>%
    filter(data_type == "fc")
  obs_assim <- tibble(date = forecast_data$datetime,
                obs = forecast_data$chla) %>%
    mutate(datefactor = as.factor(format(date, "%m-%d")),
           obs = ifelse(date == last(forecast_dates),NA,obs))
  obs_not_assim <- observations %>%
    mutate(datefactor = as.factor(format(datetime, "%m-%d")))
  
  p <- ggplot()+
    geom_line(data = forecast_series, aes(x = datefactor, y = chla, group = ensemble_member, color = "Ensemble members"))+
    geom_violinhalf(data = fc, aes(x = datefactor, y = chla, fill = "Forecast"), color = "black",
                    scale = "width", width = 0.7)+
    geom_violinhalf(data = ic, aes(x = datefactor, y = chla, fill = "Initial condition"), color = "cornflowerblue", alpha = 0.4, scale = "width", width = 0.7)+
    geom_point(data = obs_not_assim, aes(x = datefactor, y = chla, color = "Obs. - not assimilated"), size = 3, shape = 21, stroke = 2)+
    geom_point(data = obs_assim, aes(x = datefactor, y = obs, color = "Obs. - assimilated"), size = 3)+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    xlab("")+
    theme_bw(18)+
    theme(panel.grid.major.x = element_line(colour = "black", linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_color_manual(values = c("Ensemble members" = "lightgray",
                                  "Obs. - not assimilated" = "orange",
                                  "Obs. - assimilated" = "orange"), 
                       name = "",
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid","blank", "blank"),
                         shape = c(NA, 16, 21))))+
    scale_fill_manual(values = c("Forecast" = "white", "Initial condition" = "cornflowerblue"),
                      name = "",
                      guide = guide_legend(override.aes = list(
                        color = c("black","cornflowerblue"))))+
    ggtitle("A series of 1-day-ahead forecasts")
  
  return(suppressWarnings(print(p)))
}

#' Scenario forecast plots with water quality threshold
#' 
plot_scenario_forecasts <- function(forecast_data, forecast_series, show_final_obs=NULL){
  
  forecast_dates <- unique(forecast_series$date)
  forecast_series <- forecast_series %>%
    mutate(datefactor = as.factor(format(date, "%m-%d")),
           chla = ifelse((date == last(forecast_dates) & data_type == "ic"),NA,chla))
  ic <- forecast_series %>%
    filter(data_type == "ic")
  fc <- forecast_series %>%
    filter(data_type == "fc")
  obs <- tibble(date = forecast_data$datetime,
                obs = forecast_data$chla) %>%
    mutate(datefactor = as.factor(format(date, "%m-%d")))
  
  if(is.null(show_final_obs)){
    obs <- obs %>%
      mutate(obs = ifelse(date == last(forecast_dates),NA,obs))
  }
  
  p <- ggplot()+
    geom_line(data = forecast_series, aes(x = datefactor, y = chla, group = ensemble_member, color = "Ensemble members"))+
    geom_violinhalf(data = fc, aes(x = datefactor, y = chla, fill = "Forecast"), color = "black",
                    scale = "width", width = 0.7)+
    geom_violinhalf(data = ic, aes(x = datefactor, y = chla, fill = "Initial condition"), color = "cornflowerblue", alpha = 0.4, scale = "width", width = 0.7)+
    geom_point(data = obs, aes(x = datefactor, y = obs, color = "Observations"), size = 3)+
    geom_hline(aes(yintercept = 10, color = "Water quality threshold"))+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    xlab("")+
    theme_bw()+
    theme(panel.grid.major.x = element_line(colour = "black", linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    scale_color_manual(values = c("Ensemble members" = "lightgray",
                                  "Observations" = "orange",
                                  "Water quality threshold" ="red"), 
                       name = "",
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid","blank","solid"),
                         shape = c(NA, 16, NA))))+
    scale_fill_manual(values = c("Forecast" = "white", "Initial condition" = "cornflowerblue"),
                      name = "",
                      guide = guide_legend(override.aes = list(
                        color = c("black","cornflowerblue"))))+
    ggtitle("A series of 1-day-ahead forecasts")
  
  return(suppressWarnings(print(p)))
}