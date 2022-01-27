#' Read in NEON data
#' @param siteID character; four-letter identifier for the NEON site
#' @param var character; short name used to identify NEON variables. See "data/neon_variables.csv"

read_neon_data <- function(siteID, var) {
  idx <- which(neon_vars$Short_name == var)
  read_var <- neon_vars$id[idx]
  units <- neon_vars$units[idx]
  file <- file.path("data", "neon", paste0(siteID, "_", read_var, "_", units, ".csv"))
  if(file.exists(file)) {
    df <- read.csv(file)
    df[, 1] <- as.POSIXct(df[, 1], tz =  "UTC")
    colnames(df)[2] <- "value"
    df$var <- neon_vars$id[idx]
    return(df)
  } else {
    stop("File: '", file, "' does not exist.")
  }
}
