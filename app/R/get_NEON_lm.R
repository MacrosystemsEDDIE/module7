#' fit a linear model between selected NEON variables at a lake site
#'
#' @param siteID name of NEON lake site
#' @param x NEON variable for x axis; choose from NEON short names
#' @param y NEON variable for y axis; choose from NEON short names
#' @param start_date start date for forecast; linear model will be created for #' the 30 most recent observations prior to forecast start date


get_NEON_lm <- function(siteID,x,y,start_date){
  
  #grab x variable
  ref <- x
  x_var <- neon_vars$id[which(neon_vars$Short_name == ref)][1]
  x_units <- neon_vars$units[which(neon_vars$Short_name == ref)][1]
  x_file <- file.path("data/neon", paste0(siteID, "_", x_var, "_", x_units, ".csv"))
  if(!file.exists(x_file)) {
    stop(x_file, " does not exist.")
  }
  
  xvar <- read.csv(x_file)
  xvar[, 1] <- as.POSIXct(xvar[, 1], tz = "UTC")
  xvar$Date <- as.Date(xvar[, 1])
  xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
  xvar <- subset(xvar, xvar$Date < start_date)
  
  #grab y variable
  ref2 <- y
  y_var <- neon_vars$id[which(neon_vars$Short_name == ref2)][1]
  y_units <- neon_vars$units[which(neon_vars$Short_name == ref2)][1]
  y_file <- file.path("data/neon", paste0(siteID, "_", y_var, "_", y_units, ".csv"))
  if(!file.exists(y_file)) {
    stop(y_file, " does not exist.")
  }
  
  yvar <- read.csv(y_file)
  yvar[, 1] <- as.POSIXct(yvar[, 1], tz = "UTC")
  yvar$Date <- as.Date(yvar[, 1])
  if(ref2 == "Surface water temperature") {
    yvar <- yvar[yvar[, 2] == min(yvar[, 2], na.rm = TRUE), c(1, 3)] # subset to Surface water temperature
  }
  yvar <- plyr::ddply(yvar, c("Date"), function(y) mean(y[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
  yvar <- subset(yvar, yvar$Date < start_date)
  #ok need to merge first and then find rows with no NAs
  
  #merge x and y
  df <- merge(xvar, yvar, by = "Date") %>%
    dplyr::arrange(desc(Date)) %>% 
    dplyr::filter(!is.na(V1.x) & !is.na(V1.y)) %>%
    dplyr::slice(1:30)
  colnames(df)[-1] <- c("X", "Y")
  
  #fit model
  fit <- lm(df[, 3] ~ df[, 2])
  coeffs <- fit$coefficients
  m <- round(coeffs[2], 2)
  b <- round(coeffs[1], 2)
  r2 <- round(summary(fit)$r.squared, 2)
  
  sig <- sigma(fit)
  
  return(list(df = df, fit = fit, m = m, b = b, r2 = r2, sigma = sig))
}