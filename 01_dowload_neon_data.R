# Install packages
install.packages("duckdb")
remotes::install_github("cboettig/neonstore")

Sys.setenv("NEONSTORE_HOME" = "/some/filepath/on/computer")
Sys.setenv("NEONSTORE_DB" = "/some/filepath/on/computer")
Sys.setenv("NEON_TOKEN" = "See link...") # https://www.neonscience.org/resources/learning-hub/tutorials/neon-api-tokens-tutorial

products <- c(
  "DP1.20093.001", # - Chemical properties of surface water
  "DP1.20288.001", # - Water quality
  "DP1.00002.001", # - Air temperature
  "DP1.20219.001", # - Zooplankton
  "DP1.20033.001", # - Nitrate in surface water
  "DP1.20264.001", # - Temperature at specific depths
  "DP1.20048.001", # - Stream discharge field collection
  "DP1.20252.001", # - Secchi Depth
  "DP1.20046.001", # - Air temperature of lakes on buoy
  "DP4.00130.001", # - Stream discharge - Streams
  "DP1.20053.001", # - Temperature (PRT) in surface water
  "DP1.20264.001", # - Temperature at specific depth in lakes
  "DP1.20097.001", # - Dissolved gases in surface water
  "DP1.20261.001", # - Photosynthetically active radiation below water surface
  "DP1.20042.001", # - Photosynthetically active radiation at water surface
  "DP1.00006.001" # - Precipitation
)
sites <- c("BARC", "CRAM", "SUGG", "PRPO", "LIRO", "PRLA")

# Download data into the NEONSTOR_DB defined above
lapply(products, function(x){
  tryCatch({
    neonstore::neon_download(product = x, site = sites)
  })
})

# end
