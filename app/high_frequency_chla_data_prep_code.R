# Prep code for Rmd version of Module 7
# Author: Mary Lofton
# Date: 05Jan24

# Purpose: prepare data files and other ancillary code for RMarkdown version of Mod 7

# Load packages
pacman::p_load(tidyverse, lubridate, data.table, zoo, sparklyr, neonUtilities)

#define NEON token
source("./app/R/neon_token_source.R")

# Get high-frequency data to inform IC distribution
#read in site info
sites <- readr::read_csv("./app/data/NEON_Field_Site_Metadata_20220412.csv") |> 
  dplyr::filter(field_site_id %in% c("BARC","SUGG","LIRO","PRPO","PRLA","CRAM"))
lake_sites <- sites$field_site_id[(which(sites$field_site_subtype == "Lake"))]

#read in wq data

neon <- arrow::s3_bucket("neon4cast-targets/neon",
                         endpoint_override = "data.ecoforecast.org",
                         anonymous = TRUE)

####Chl-a ===============================================#######

wq_portal <- purrr::map_dfr(sites$field_site_id, function(site){
  message(site)
  arrow::open_dataset(neon$path("waq_instantaneous-basic-DP1.20288.001"), partitioning = "siteID") %>%   # waq_instantaneous
    #wq_portal <- neonstore::neon_table("waq_instantaneous", site = sites$field_site_id, lazy = TRUE) %>%   # waq_instantaneous
    dplyr::filter(siteID %in% site) %>%
    dplyr::select(siteID, startDateTime, sensorDepth,
                  chlorophyll, chlorophyllExpUncert,chlorophyllFinalQF,
                  chlaRelativeFluorescence, chlaRelFluoroFinalQF) %>%
    dplyr::mutate(sensorDepth = as.numeric(sensorDepth),
                  chla = as.numeric(chlorophyll),
                  RFU = as.numeric(chlaRelativeFluorescence),
                  chlorophyllExpUncert = as.numeric(chlorophyllExpUncert)) %>%
    dplyr::rename(site_id = siteID) %>% 
    dplyr::filter(((sensorDepth > 0 & sensorDepth < 1)| is.na(sensorDepth))) |> 
    collect() %>% # sensor depth of NA == surface?
    dplyr::mutate(startDateTime = as_datetime(startDateTime)) %>%
    # QF (quality flag) == 0, is a pass (1 == fail), 
    # make NA so these values are not used in the mean summary
    dplyr::mutate(chla = ifelse(chlorophyllFinalQF == 0, chla, NA),
                  RFU = ifelse(chlaRelFluoroFinalQF == 0, RFU, NA)) %>% 
    dplyr::select(startDateTime, site_id, chla, RFU) %>%
    dplyr::filter(month(startDateTime) %in% c(9:10) & year(startDateTime) == 2019)
}
)

#QC water quality data

wq_full <- wq_portal

# chlorophyll ranges
chla_max <- 200
chla_min <- 0

# GR flag will be true if either the DO concentration or the chlorophyll are 
# outside the ranges specified about

wq_cleaned <- wq_full %>%
  dplyr::mutate(chla = ifelse(is.na(chla),chla, ifelse(chla >= chla_min & chla <= chla_max, chla, NA)),
                RFU = ifelse(is.na(RFU),RFU, ifelse(RFU >= chla_min & RFU <= chla_max, RFU, NA))) 
# manual cleaning based on visual inspection

##############################################################################
ggplot(data = wq_cleaned, aes(x = startDateTime, y = chla))+
  geom_point()+
  facet_wrap(vars(site_id), scales = "free", nrow = 2)+
  theme_bw()
##############################################################################

chla_final <- wq_cleaned %>%
  rename(datetime = startDateTime) %>%
  select(datetime, chla, site_id) %>%
  filter(complete.cases(.))

write.csv(chla_final, "./app/data/chla_microgramsPerLiter_highFrequency.csv",row.names = FALSE)

dat <- read_csv("./app/data/chla_microgramsPerLiter_highFrequency.csv") %>%
  filter(site_id == "CRAM" & date(datetime) == "2020-09-25")
