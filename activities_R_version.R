#' Improving Ecological Forecasts with Data
#' Macrosystems EDDIE Module 7
#' http://module7.macrosystemseddie.org

# Load in required datasets ----

#* Load libraries ----
library(ggplot2)
library(plotly)

#* Load in NEON sites in sp format with coordinates for map ----
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites <- neon_sites[which(neon_sites$siteID), ]
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))

#* Load in NEON sites dataframe ----
neon_sites_df <- read.csv("data/neon_sites.csv")
neon_sites_df$long <- round(neon_sites_df$long, 3)
neon_sites_df$lat <- round(neon_sites_df$lat, 3)
neon_sites_df <- neon_sites_df[which(neon_sites_df$siteID), ]
neon_sites_df$uid <- paste0("M", seq_len(nrow(neon_sites_df))) # For leaflet map

#* Reference for downloading variables ----
neon_vars <- read.csv("data/neon_variables.csv")

#* Load in R functions ----
sapply(list.files("R", full.names = TRUE), source)

# Objective 1 - Site Selection ----
siteID <- "BARC"

#* Website to find more information:
print(paste0("https://www.neonscience.org/field-sites/", siteID))

# Objective 2 - Explore data ----
#* Load in data ----
airt <- read_neon_data(siteID, "Air temperature")
wtemp <- read_neon_data(siteID, "Surface water temperature")
chla <- read_neon_data(siteID, "Chlorophyll-a")
secchi <- read_neon_data(siteID, "Secchi depth")

p <- ggplot(data = airt) +
  geom_point(aes(Date, value)) +
  ylab("Temperature (\u00B0C)") +
  xlab("Time") +
  theme_classic()
ggplotly(p)

# Parameters for NPZ model
parms <- c(
  maxUptake = 1.0, #day-1
  kspar=120, #uEinst m-2 s-1
  ksdin=0.5, #mmol m-3
  maxGrazing=1.0, # day-1
  ksphyto=1, #mmol N m-3
  pFaeces=0.3, #unitless
  mortalityRate=0.4, #(mmmolN m-3)-1 day-1
  excretionRate=0.1, #day-1
  mineralizationRate=0.1, #day-1
  Chl_Nratio = 1, #mg chl (mmolN)-1
  Q10 = 2,  #unitless
  addTEMP = 0, # added to temperature
  scaleNLOAD = 1, # multiplier for N loading
  refTEMP = 20
)  

# Initial conditions for NPZ
yini <- c(
  PHYTO = 2, #mmolN m-3
  # ZOO = 0.4, #mmolN m-3
  # DETRITUS = 1, #mmolN m-3
  DIN = 9) #mmolN m-3
