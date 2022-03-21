#' load NOAA GEFS forecast
#'
#' @param siteID name of NEON lake site
#' @param start_date start date for forecast

load_noaa_forecast <- function(siteID, start_date){
  
  fpath <- file.path("data", "NOAAGEFS_1hr", siteID)
  fpath2 <- file.path(fpath, start_date, "00")
  fils <- list.files(fpath2)
  fils <- fils[-c(grep("ens00", fils))]
  fid <- nc_open(file.path(fpath2, fils[1]))
  vars <- fid$var # Extract variable names for selection
  fc_vars <- c("air_temperature", "surface_downwelling_shortwave_flux_in_air") # names(vars)
  membs <- length(fils)
  
  out <- lapply(start_date, function(dat) {
    idx <- which(start_date == dat)
    
    fpath2 <- file.path(fpath, dat, "00")
    fils <- list.files(fpath2)
    fils <- fils[-c(grep("ens00", fils))]
    
    for( i in seq_len(length(fils))) {
      
      fid <- ncdf4::nc_open(file.path("data", "NOAAGEFS_1hr", siteID, dat,
                                      "00", fils[i]))
      tim = ncdf4::ncvar_get(fid, "time")
      tunits = ncdf4::ncatt_get(fid, "time")
      lnam = tunits$long_name
      tustr <- strsplit(tunits$units, " ")
      step = tustr[[1]][1]
      tdstr <- strsplit(unlist(tustr)[3], "-")
      tmonth <- as.integer(unlist(tdstr)[2])
      tday <- as.integer(unlist(tdstr)[3])
      tyear <- as.integer(unlist(tdstr)[1])
      tdstr <- strsplit(unlist(tustr)[4], ":")
      thour <- as.integer(unlist(tdstr)[1])
      tmin <- as.integer(unlist(tdstr)[2])
      origin <- as.POSIXct(paste0(tyear, "-", tmonth, 
                                  "-", tday, " ", thour, ":", tmin), 
                           format = "%Y-%m-%d %H:%M", tz = "UTC")
      if (step == "hours") {
        tim <- tim * 60 * 60
      }
      if (step == "minutes") {
        tim <- tim * 60
      }
      time = as.POSIXct(tim, origin = origin, tz = "UTC")
      var_list <- lapply(fc_vars, function(x) {
        data.frame(time = time, value = ncdf4::ncvar_get(fid, x))
      }) 
      
      
      ncdf4::nc_close(fid)
      names(var_list) <- fc_vars
      
      mlt1 <- reshape::melt(var_list, id.vars = "time")
      mlt1 <- mlt1[, c("time", "L1", "value")]
      
      # df <- get_vari(file.path("data", fils[i]), input$fc_var, print = F)
      cnam <- paste0("ens", formatC(i, width = 2, format = "d", flag = "0"))
      if(i == 1) {
        df2 <- mlt1
        colnames(df2)[3] <- cnam
      } else {
        df2 <- merge(df2, mlt1, by = c(1,2))
        colnames(df2)[ncol(df2)] <- cnam
      }
    }
    return(df2)
  })
  
  names(out) <- start_date
  return(out)
}