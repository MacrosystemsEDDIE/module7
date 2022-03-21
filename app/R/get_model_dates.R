#' retreive the model time steps based on start and stop dates and time step
#'
#' @param model_start model start date in date class
#' @param model_stop model stop date in date class
#' @param time_step model time step, defaults to daily timestep
get_model_dates <- function(model_start, model_stop, time_step = 'days'){
  
  model_dates <- seq.Date(from = as.Date(model_start), to = as.Date(model_stop), by = time_step)
  
  return(model_dates)
}