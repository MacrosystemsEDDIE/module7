Sys.setenv("NEONSTORE_HOME" = "/some/filepath/on/computer") # Same as 01_download_neon_data.R
Sys.setenv("NEONSTORE_DB" = "/some/filepath/on/computer")

library(tidyverse)
library(neonstore)

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

# Loop through p
for(prod in products) {
  for(sit in sites) {
    
    message("[", Sys.time(), "] Starting ", prod, " for ", sit)
    
    vars <- neonstore::neon_read(site = sit, table = "variables",
                                 product = prod)
    if(is.null(vars)) {
      message("No data for ", prod, " at ", sit)
      next
    }
    
    if( prod == "DP1.20093.001"){
      idx <- which(vars$description == "Total dissolved nitrogen concentration")
      tbl <- vars$table[idx]
      
    }
    if( prod == "DP1.20288.001"){
      idx <- which(vars$description == "Chlorophyll a concentration in water")
      tbl <- vars$table[idx]
    }
    if( prod == "DP1.00002.001"){
      idx <- which(vars$description == "Arithmetic mean of single aspirated air temperature")
      tbl <- vars$table[idx]
      nums <- regmatches(tbl, regexpr("[[:digit:]]+", tbl))
      tbl <- tbl[which.max(nums)]
    }
    
    if( prod == "DP1.20033.001"){
      idx <- which(vars$description == "Arithmetic mean nitrate concentration in surface water in micromoles per liter")
      tbl <- vars$table[idx]
    }
    if( prod == "DP1.20252.001"){
      idx <- which(vars$description == "Mean secchi depth at sampling location")
      tbl <- vars$table[idx]
    }
    if( prod == "DP1.20097.001"){
      idx <- which(vars$description == "Dissolved Oxygen Concentration")
      tbl <- vars$table[idx]
    }
    if( prod == "DP1.20042.001"){
      idx <- which(vars$description == "Arithmetic mean of photosynthetically active radiation")
      tbl <- vars$table[idx]
      nums <- regmatches(tbl, regexpr("[[:digit:]]+", tbl))
      tbl <- tbl[which.max(nums)]
    }
    if( prod == "DP1.20261.001"){
      idx <- which(vars$description == "Arithmetic mean of PAR below water surface")
      tbl <- vars$table[idx]
      nums <- regmatches(tbl, regexpr("[[:digit:]]+", tbl))
      tbl <- tbl[which.max(nums)]
    }
    
    
    if( prod == "DP1.20264.001"){
      idx <- which(vars$description == "Arithmetic mean of tsdWaterTemp")
      tbl <- vars$table[idx]
      var <- neonstore::neon_read(
        table = tbl,
        product = prod,
        site = sit, ext = "csv", start_date = "2018-01-01"
      )
      # Zooplankton
    } else if( prod == "DP1.20219.001"){
      # Extract Zooplankton counts
      var1 <- neonstore::neon_read(
        table = "zoo_taxonomyProcessed",
        product = prod,
        site = sit, ext = "csv"
      )
      sub1 <- var1[, c("collectDate", "sampleID", "phylum", "adjCountPerBottle")]
      # Extract details of volume of tow
      var2 <- neonstore::neon_read(
        table = "zoo_fieldData-basic",
        product = prod,
        site = sit, ext = "csv"
      )
      sub2 <- var2[, c("collectDate", "sampleID", "towsTrapsVolume")]
      
      sc <- merge(sub1, sub2, by = c(1, 2), all.x = T)
      
      sc2 <- plyr::ddply(sc, c("collectDate", "sampleID"), function(x) {
        abund <- sum(x[,4]) / mean(x[, 5])
      })
      
    } else {
      var <- neonstore::neon_read(
        table = tbl,
        product = prod,
        site = sit
      )
    }
    
    
    
    # Formatting data for Shiny app
    
    # TN
    if( prod == "DP1.20093.001"){
      sc <- select(var, collectDate, siteID, namedLocation, analyte, analyteConcentration, analyteUnits) %>%
        filter(grepl('buoy', namedLocation))%>%
        arrange(siteID,analyte,collectDate)%>%
        rename(units = analyteUnits)%>%
        rename(value = analyteConcentration)%>%
        select(-namedLocation)%>%
        mutate(collectDate = format(as.Date(collectDate, "%Y-%m-%d HH:MM:SS"), "%Y-%m-%d"))
      tn <- sc[sc$analyte == "TN",]
      tn <- tn[, c("collectDate", "value")]
      write.csv(tn, file.path("app", "data", paste0(sit, "_TN_", vars$units[idx[1]], ".csv")),
                row.names = FALSE, quote = FALSE)
      
      # DIN
      amm <- sc[sc$analyte == "NH4 - N",]
      amm$value[amm$value < 0] <- 0
      nit <- sc[sc$analyte == "NO3+NO2 - N",]
      nit$value[nit$value < 0] <- 0
      
      din <- nit
      din$value <- amm$value + nit$value
      din <- din[, c("collectDate", "value")]
      write.csv(din, file.path("app", "data", paste0(sit, "_DIN_", nit$units[1], ".csv")),
                row.names = FALSE, quote = FALSE)
    }
    
    # WAQ
    if( prod == "DP1.20288.001"){
      sc <- var[, c("endDateTime", "chlorophyll")]
      sc$Date <- as.Date(sc$endDateTime)
      sc2 <- plyr::ddply(sc, "Date", function(x) mean(x[, 2], na.rm = TRUE))
      write.csv(sc2, file.path("app", "data", paste0(sit, "_chla_", vars$units[idx[1]], ".csv")),
                row.names = FALSE, quote = FALSE)
    }
    
    # Air temperature
    if( prod == "DP1.00002.001"){
      sc <- var[, c("endDateTime", "tempSingleMean")]
      sc$Date <- as.Date(sc$endDateTime)
      sc2 <- plyr::ddply(sc, "Date", function(x) mean(x[, 2], na.rm = TRUE))
      write.csv(sc2, file.path("app", "data", paste0(sit, "_airt_", vars$units[idx[1]], ".csv")),
                row.names = FALSE, quote = FALSE)
    }
    
    # Nitrate in surface water
    if( prod == "DP1.20033.001"){
      sc <- var[, c("endDateTime", "surfWaterNitrateMean")]
      sc$Date <- as.Date(sc$endDateTime)
      sc2 <- plyr::ddply(sc, "Date", function(x) mean(x[, 2], na.rm = TRUE))
      write.csv(sc2, file.path("app", "data", paste0(sit, "_surfN_", vars$units[idx[1]], ".csv")),
                row.names = FALSE, quote = FALSE)
    }
    
    # Water temperature thermistor
    if( prod == "DP1.20264.001"){
      sc <- var[, c("endDateTime", "thermistorDepth", "tsdWaterTempMean")]
      sc$Date <- as.Date(sc$endDateTime)
      sc2 <- plyr::ddply(sc, c("Date", "thermistorDepth"), function(x) mean(x[, 3], na.rm = TRUE))
      write.csv(sc2, file.path("app", "data", paste0(sit, "_wtemp_", vars$units[idx[1]], ".csv")),
                row.names = FALSE, quote = FALSE)
    }
    
    # Secchi depth
    if( prod == "DP1.20252.001"){
      sc <- var[, c("date", "secchiMeanDepth")]
      write.csv(sc, file.path("app", "data", paste0(sit, "_secchiDepth_", vars$units[idx[1]], ".csv")),
                row.names = FALSE, quote = FALSE)
    }
    
    # Secchi depth
    if( prod == "DP1.20097.001"){
      sc <- var[, c("collectDate", "dissolvedOxygen")]
      write.csv(sc, file.path("app", "data", paste0(sit, "_dissolvedOxygen_", vars$units[idx[1]], ".csv")),
                row.names = FALSE, quote = FALSE)
    }
    
    # Underwater PAR
    if( prod == "DP1.20261.001"){
      sc <- var[, c("endDateTime", "uPARMean")]
      sc$Date <- as.Date(sc$endDateTime)
      sc2 <- plyr::ddply(sc, "Date", function(x) mean(x[, 2], na.rm = TRUE))
      write.csv(sc2, file.path("app", "data", paste0(sit, "_uPAR_", vars$units[idx[1]], ".csv")),
                row.names = FALSE, quote = FALSE)
      sc2[, 2] <- LakeMetabolizer::sw.to.par(sc2[, 2])
      write.csv(sc2, file.path("app", "data", paste0(sit, "_swr_wattsPerSquareMeter.csv")),
                row.names = FALSE, quote = FALSE)
    }
    
    # PAR
    if( prod == "DP1.20261.001"){
      sc <- var[, c("endDateTime", "PARMean")]
      sc$Date <- as.Date(sc$endDateTime)
      sc$PARMean[sc$PARMean < 0] <- 0
      sc$swr <- LakeMetabolizer::sw.to.par.base(sc$PARMean)
      sc2 <- plyr::ddply(sc, "Date", function(x) mean(x[, "swr"], na.rm = TRUE))
      write.csv(sc2, file.path("app", "data", paste0(sit, "_swr_wattsPerSquareMeter.csv")),
                row.names = FALSE, quote = FALSE)
    }
  }
}

# end
