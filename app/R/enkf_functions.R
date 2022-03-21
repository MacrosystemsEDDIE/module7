#' vector for holding states and parameters for updating
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_params_est number of parameters we're calibrating
#' @param n_step number of model timesteps
#' @param n_en number of ensembles
get_Y_vector = function(n_states, n_params_est, n_step, n_en){
  
  Y = array(dim = c(n_states + n_params_est, n_step, n_en))
  
  return(Y)
}

#' observation error matrix, should be a square matrix where
#'   col & row = the number of states and params for which you have observations
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_param_obs number of parameters for which we have observations
#' @param n_step number of model timesteps
#' @param state_sd vector of state observation standard deviation; assuming sd is constant through time
#' @param param_sd vector of parameter observation standard deviation; assuming sd is constant through time
get_obs_error_matrix = function(n_states, n_params_obs, n_step, state_sd, param_sd){
  
  R = array(0, dim = c(n_states + n_params_obs, n_states + n_params_obs, n_step))
  
  state_var = state_sd^2 #variance of temperature observations
  
  param_var = param_sd^2
  
  if(n_params_obs > 0){
    all_var = c(state_var, param_var)
  }else{
    all_var = state_var
  }
  
  for(i in 1:n_step){
    # variance is the same for each depth and time step; could make dynamic or varying by time step if we have good reason to do so
    R[,,i] = diag(all_var, n_states + n_params_obs, n_states + n_params_obs)
  }
  
  return(R)
}

#' Measurement operator matrix saying 1 if there is observation data available, 0 otherwise
#'
#' @param n_states number of states we're updating in data assimilation routine
#' @param n_param_obs number of parameters for which we have observations
#' @param n_params_est number of parameters we're calibrating
#' @param n_step number of model timesteps
#' @param obs observation matrix created with get_obs_matrix function
get_obs_id_matrix = function(n_states, n_params_obs, n_params_est, n_step, obs){
  
  H = array(0, dim=c(n_states + n_params_obs, n_states + n_params_est, n_step))
  
  # order goes 1) states, 2)params for which we have obs, 3) params for which we're estimating but don't have obs
  
  for(t in 1:n_step){
    H[1:(n_states + n_params_obs), 1:(n_states + n_params_obs), t] = diag(ifelse(is.na(obs[,,t]),0, 1), n_states + n_params_obs, n_states + n_params_obs)
  }
  
  return(H)
}

#' turn observation dataframe into matrix
#'
#' @param obs_df observation data frame
#' @param model_dates dates over which you're modeling
#' @param n_step number of model time steps
#' @param n_states number of states we're updating in data assimilation routine
#' @param states character string vector of state names in obs_file

get_obs_matrix = function(obs_df, model_dates, n_step, n_states, states) {
  
  # need to know location and time of observation
  
  obs_df_filtered = obs_df %>%
    dplyr::filter(as.Date(datetime) %in% model_dates) %>%
    mutate(date = as.Date(datetime)) %>%
    select(date, chla, nitrate) %>%
    mutate(date_step = which(model_dates %in% date))
  
  obs_matrix = array(NA, dim = c(n_states, 1, n_step))
  
  for(i in 1:n_states){
    for(j in obs_df_filtered$date_step){
      obs_matrix[i, 1, j] = dplyr::filter(obs_df_filtered,
                                          date_step == j) %>%
        dplyr::pull(states[i])
    }}
  
  return(obs_matrix)
}

##' @param Y vector for holding states and parameters you're estimating
##' @param R observation error matrix
##' @param obs observations at current timestep
##' @param H observation identity matrix
##' @param n_en number of ensembles
##' @param cur_step current model timestep
kalman_filter = function(Y, R, obs, H, n_en, cur_step){
  
  cur_obs = obs[ , , cur_step]
  
  cur_obs = ifelse(is.na(cur_obs), 0, cur_obs) # setting NA's to zero so there is no 'error' when compared to estimated states
  
  ###### estimate the spread of your ensembles #####
  Y_mean = matrix(apply(Y[ , cur_step, ], MARGIN = 1, FUN = mean), nrow = length(Y[ , 1, 1])) # calculating the mean of each temp and parameter estimate
  delta_Y = Y[ , cur_step, ] - matrix(rep(Y_mean, n_en), nrow = length(Y[ , 1, 1])) # difference in ensemble state/parameter and mean of all ensemble states/parameters
  
  ###### estimate Kalman gain #########
  K = ((1 / (n_en - 1)) * delta_Y %*% t(delta_Y) %*% t(H[, , cur_step])) %*%
    qr.solve(((1 / (n_en - 1)) * H[, , cur_step] %*% delta_Y %*% t(delta_Y) %*% t(H[, , cur_step]) + R[, , cur_step]))
  
  ###### update Y vector ######
  for(q in 1:n_en){
    Y[, cur_step, q] = Y[, cur_step, q] + K %*% (cur_obs - H[, , cur_step] %*% Y[, cur_step, q]) # adjusting each ensemble using kalman gain and observations
  }
  return(Y)
}

#' initialize Y vector with draws from distribution of obs
#'
#' @param Y Y vector
#' @param obs observation matrix
initialize_Y = function(Y, obs, init_params, n_states_est, n_params_est, n_params_obs, n_step, n_en, state_sd, param_sd, yini){
  
  # initializing states with earliest observations and parameters
  first_obs = yini #%>% # turning array into list, then using coalesce to find first obs in each position.
  #ifelse(is.na(.), mean(., na.rm = T), .) # setting initial temp state to mean of earliest temp obs from other sites if no obs
  #MEL omitting this for now - can build back in later if needed
  
  if(n_params_est > 0){
    first_params = init_params
  }else{
    first_params = NULL
  }
  
  Y[ , 1, ] = array(abs(rnorm(n = n_en * (n_states_est + n_params_est),
                              mean = c(first_obs, first_params),
                              sd = c(state_sd, param_sd))),
                    dim = c(c(n_states_est + n_params_est), n_en))
  
  return(Y)
}

#' matrix for holding driver data
#'
#' @param drivers_df dataframe which holds all the driver data 
#' @param model_dates dates for model run 
#' @param n_drivers number of model drivers 
#' @param driver_colnames column names of the drivers in the driver dataframe 
#' @param driver_cv coefficient of variation for each driver data 
#' @param n_step number of model timesteps
#' @param n_en number of ensembles
get_drivers = function(fc_conv, n_drivers, driver_colnames, model_dates, n_en){
  
  drivers_out = array(NA, dim = c(length(model_dates), n_drivers, n_en))
  
  for(i in 1:n_drivers){
    for(j in 1:length(model_dates)){
      fc1 <- fc_conv %>% filter(date == model_dates[j])
      drivers_out[j,i,] = sample(fc1[,driver_colnames[i]], n_en, replace = TRUE)
    }
  }
  
  return(drivers_out) 
}

#' wrapper for running EnKF 
#' 
#' @param n_en number of model ensembles 
#' @param start start date of model run 
#' @param stop date of model run
#' @param time_step model time step, defaults to days 
#' @param obs_file observation file 
#' @param driver_file driver data file 
#' @param n_states_est number of states we're estimating 
#' @param n_params_est number of parameters we're estimating 
#' @param n_params_obs number of parameters for which we have observations 
#' @param decay_init initial decay rate of DOC 
#' @param obs_cv coefficient of variation of observations 
#' @param param_cv coefficient of variation of parameters 
#' @param init_cond_cv initial condition CV 
#' @param state_names character string vector of state names as specified in obs_file
#' @param yini vector of initial conditions for states (chla, nitrate)

EnKF = function(n_en = 30, 
                start = '2020-09-25', # start date 
                stop = '2020-10-29', 
                time_step = 'days', 
                obs_file = lake_data_no_assim,
                driver_file = fc_conv,
                n_states_est = 2, 
                n_params_est = 1,
                n_params_obs = 0, 
                maxUptake_init = 0.1, 
                obs_cv = c(0.2, 0.1),
                param_cv = 0.1,
                init_cond_cv = c(0.1, 0.1),
                state_names = c("chla","nitrate"),
                yini = c(7.21, 1.39)){
  
  
  n_en = n_en
  start = as.Date(start)
  stop = as.Date(stop)
  time_step = 'days' 
  dates = get_model_dates(model_start = start, model_stop = stop, time_step = time_step)
  n_step = length(dates)
  
  # get observation matrix
  obs_df = obs_file %>% 
    select(datetime, chla, nitrate) 
  
  n_states_est = n_states_est # number of states we're estimating 
  
  n_params_est = n_params_est # number of parameters we're calibrating
  
  n_params_obs = n_params_obs # number of parameters for which we have observations
  
  maxUptake_init = maxUptake_init # Initial estimate of DOC decay rate day^-1 
  
  yini <- c( #initial estimate of PHYTO and nitrate states, respectively
    PHYTO = yini[1], #mmolN m-3 which is the same as mg chl b/c ratio set to 1
    DIN = yini[2]) #mmolN m-3
  
  state_cv = obs_cv #coefficient of variation of chla and din observations, respectively 
  state_sd = state_cv * yini
  init_cond_sd = init_cond_cv * yini
  
  param_cv = param_cv #coefficient of variation of maxUptake 
  param_sd = param_cv * maxUptake_init
  
  # setting up matrices
  # observations as matrix
  obs = get_obs_matrix(obs_df = obs_df,
                       model_dates = dates,
                       n_step = n_step,
                       n_states = n_states_est,
                       states = state_names)
  
  # Y vector for storing state / param estimates and updates
  Y = get_Y_vector(n_states = n_states_est,
                   n_params_est = n_params_est,
                   n_step = n_step,
                   n_en = n_en)
  
  # observation error matrix
  R = get_obs_error_matrix(n_states = n_states_est,
                           n_params_obs = n_params_obs,
                           n_step = n_step,
                           state_sd = state_sd,
                           param_sd = param_sd)
  
  # observation identity matrix
  H = get_obs_id_matrix(n_states = n_states_est,
                        n_params_obs = n_params_obs,
                        n_params_est = n_params_est,
                        n_step = n_step,
                        obs = obs)
  
  # initialize Y vector
  Y = initialize_Y(Y = Y, obs = obs, init_params = maxUptake_init, n_states_est = n_states_est,
                   n_params_est = n_params_est, n_params_obs = n_params_obs,
                   n_step = n_step, n_en = n_en, state_sd = init_cond_sd, param_sd = param_sd, yini = yini)
  
  # get driver data with uncertainty - dim = c(n_step, driver, n_en) 
  drivers = get_drivers(fc_conv = driver_file, 
                        model_dates = dates,
                        n_drivers = 2, 
                        driver_colnames = c('wtemp', 'upar'), 
                        n_en = n_en) 
  
  # start modeling
  for(t in 2:n_step){
    for(n in 1:n_en){
      
      # run model; 
      model_output = NP_model(TEMP = drivers[t-1, 1, n], 
                              PHYTO = Y[1, t-1, n], 
                              DIN = Y[2, t-1, n], 
                              PAR = drivers[t-1, 2, n],
                              maxUptake = Y[3, t-1, n])
      
      ######quick hack to add in process error (ha!)########
      
      #specify Y_star (mean of multivariate normal)
      Y_star = matrix(c(model_output$PHYTO_pred, model_output$DIN_pred, model_output$maxUptake))
      
      #specify sigma (covariance matrix of states and updating parameters)
      residual_matrix <- matrix(NA, nrow = 4, ncol = 3)
      residual_matrix[1,] <- c(0.5, 0.3, 0.01)
      residual_matrix[2,] <- c(1, 0.2, 0.02)
      residual_matrix[3,] <- c(0.25, 0.1, 0.03)
      residual_matrix[4,] <- c(-0.30, -0.1, -0.03)
      
      sigma_proc <- cov(residual_matrix)
      
      #make a draw from Y_star
      Y_draw = abs(mvtnorm::rmvnorm(1, mean = Y_star, sigma = sigma_proc))
      Y[1 , t, n] = Y_draw[1] # store in Y vector
      Y[2 , t, n] = Y_draw[2]
      Y[3 , t, n] = Y_draw[3]
      
      #####end of hack######################################
    }
    # check if there are any observations to assimilate 
    if(any(!is.na(obs[ , , t]))){
      Y = kalman_filter(Y = Y,
                        R = R,
                        obs = obs,
                        H = H,
                        n_en = n_en,
                        cur_step = t) # updating params / states if obs available
    }
  }
  out = list(Y = Y, dates = dates, drivers = drivers, R = R, obs = obs, state_sd = state_sd)
  
  return(out)
}

#' calculate RMSE of ensemble mean vs. observations 
#' 
#' @param est_out forecast output from EnKF wrapper 
#' @param lake_data NEON data for selected site formatted using format_enkf_inputs function

rmse <- function(est_out, lake_data){
  
  #get ensemble mean
  mean_chla_est = apply(est_out$Y[1,,] , 1, FUN = mean)
  mean_din_est = apply(est_out$Y[2,,] , 1, FUN = mean)
  
  
  #limit obs to forecast dates
  lake_data1 <- lake_data %>%
    mutate(datetime = as.Date(datetime)) %>%
    filter(datetime %in% est_out$dates)
  
  #calculate RMSE
  rmse_chla <- sqrt(mean((lake_data1$chla - mean_chla_est)^2, na.rm = TRUE))
  rmse_nitrate <- sqrt(mean((lake_data1$din - mean_din_est)^2, na.rm = TRUE))
  
  return(list(rmse_chla = rmse_chla, rmse_nitrate = rmse_nitrate))
}
