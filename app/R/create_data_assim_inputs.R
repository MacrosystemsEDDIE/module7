#' students will choose a frequency of data assimilation ranging from monthly to #' daily
#' @param freq_chla frequency of chla data assimilation in days between 1-30
#' @param freq_din frequency of din data assimilation in days between 1-30
#' @param lake_data NEON lake dataset that has been formatted using the format_enkf_inputs function
#' @param start_date start date of forecast (either 2020-09-25 or 2020-10-02)
create_data_assim_inputs <- function(freq_chla, freq_din, lake_data, start_date){
  
  dates <- get_model_dates(as.Date(start_date), as.Date(start_date)+35, time_step = 'days')
  
  a <- 1:35
  b1 <- a[seq(1, length(a), freq_chla)]
  b2 <- a[seq(1, length(a), freq_din)]
  
  out <- lake_data %>%
    dplyr::select(datetime, chla, nitrate) %>%
    dplyr::mutate(datetime = as.Date(datetime)) %>%
    dplyr::filter(datetime %in% dates) %>%
    dplyr::mutate(rownum = dplyr::row_number(datetime)) %>%
    dplyr::mutate(chla = ifelse(rownum %in% b1,chla,NA),
           nitrate = ifelse(rownum %in% b2,nitrate,NA)) %>%
    dplyr::select(-rownum)
  
  return(out)
}