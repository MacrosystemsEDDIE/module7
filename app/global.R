# Load required libraries
suppressPackageStartupMessages(library(shinyBS, quietly = TRUE))
suppressPackageStartupMessages(library(shinydashboard, quietly = TRUE))
suppressPackageStartupMessages(library(rintrojs, quietly = TRUE))
suppressPackageStartupMessages(library(slickR, quietly = TRUE))
suppressPackageStartupMessages(library(sortable, quietly = TRUE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE))
suppressPackageStartupMessages(library(stringr, quietly = TRUE))
suppressPackageStartupMessages(library(hover, quietly = TRUE))
suppressPackageStartupMessages(library(shiny, quietly = TRUE))
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE))
suppressPackageStartupMessages(library(DT, quietly = TRUE))
suppressPackageStartupMessages(library(sf, quietly = TRUE))
suppressPackageStartupMessages(library(leaflet, quietly = TRUE))
suppressPackageStartupMessages(library(plotly, quietly = TRUE))
suppressPackageStartupMessages(library(kableExtra, quietly = TRUE))
suppressPackageStartupMessages(library(magrittr, quietly = TRUE))
suppressPackageStartupMessages(library(mvtnorm, quietly = TRUE))
suppressPackageStartupMessages(library(ggpubr, quietly = TRUE))
suppressPackageStartupMessages(library(ncdf4, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(lubridate, quietly = TRUE))
library(shinyalert, quietly = TRUE, warn.conflicts = FALSE)

# Icons
neonIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28),
  Terrestrial = makeIcon("icons/mountain-icon.png", iconWidth =  28, iconHeight = 28)
)


# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")
met_pars <- read.csv("data/met_params.csv", fileEncoding = "UTF-8-BOM")

# Colors for plots
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind
cols <- RColorBrewer::brewer.pal(8, "Dark2")
cols2 <- ggthemes::ggthemes_data$colorblind$value
l.cols <- RColorBrewer::brewer.pal(8, "Set2")
p.cols <- RColorBrewer::brewer.pal(12, "Paired")

# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)

# colors for theme
obj_bg <- "#D4ECE1"
ques_bg <- "#B8E0CD"
nav_bg <- "#DDE4E1"
nav_butt <- "#31ED92"
nav_txt <- "#000000" # white = #fff; black = #000000
slider_col <- "#2CB572"

# Create scale for plots
da_method_fill_scale <- scale_fill_manual(values = c("No DA" = l.cols[1], "Chl-a DA" = l.cols[2]), name = "")
#da_method_fill_scale <- scale_fill_manual(values = c("No DA" = l.cols[1], "Chl-a" = l.cols[2], "Nitrate" = l.cols[3], "Both" = l.cols[4]))

# Plot saving
png_dpi <- 120
p_wid <- 213
p_hei <- 120
p_units <- "mm"
l_siz <- 0.6
p_siz <- 1.6

# Functions required
invisible(sapply(list.files("R", full.names = TRUE), source))

# Load and format questions
quest <- read.csv("data/student_questions.csv", row.names = 1)
qid <- row.names(quest)

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Slides
recap_slides <- list.files("www/Mod7_key_slides", full.names = TRUE)
model_slides <- list.files("www/model_slides", full.names = TRUE)
fc_uc_slides <- list.files("www/fc_uc_slides", full.names = TRUE)
ic_uc_slides <- list.files("www/ic_uc_slides", full.names = TRUE)
chla_slides <- list.files("www/chla_data_collection", full.names = TRUE)
nitrate_slides <- list.files("www/nitrate_data_collection/", full.names = TRUE)

# Selected neon sites 
sel_sites <- c("CRAM", "BARC", "SUGG", "PRPO", "PRLA", "LIRO")
reservoir_sites <- c("Green Reservoir", "Brown Reservoir")

# Load in sp format with coordinates
neon_sites <- readRDS("data/neon_sites.rds")
neon_sites <- neon_sites[which(neon_sites$siteID %in% sel_sites), ]
neon_sites$uid <- paste0("M", seq_len(nrow(neon_sites)))

# data collection methods
data_collection_methods <- read.csv("data/data_collection_method.csv", fileEncoding = "UTF-8-BOM")
data_frequency_latency <- read.csv("data/data_frequency_latency.csv", fileEncoding = "UTF-8-BOM")
rownames(data_frequency_latency) <- data_frequency_latency[, 1]

# RMSE dataframe for da_freq
da_freq_df <- data.frame(
  "chla" = rep(NA, 5),
  #"nitrate" = rep(NA, 5),
  "rmse" = rep(NA, 5)
)

# RMSE dataframe for obs_uc
obs_uc_df <- data.frame(
  "chla" = rep(NA, 5),
  #"nitrate" = rep(NA, 5),
  "rmse" = rep(NA, 5)
)

#Load in the dataframe
neon_sites_df <- read.csv("data/neon_sites.csv")
neon_sites_df$long <- round(neon_sites_df$long, 3)
neon_sites_df$lat <- round(neon_sites_df$lat, 3)
neon_sites_df <- neon_sites_df[which(neon_sites_df$siteID %in% sel_sites), ]
neon_sites_df$uid <- paste0("M", seq_len(nrow(neon_sites_df))) # For leaflet map

# Add type labels
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Aquatic"]))] <- "Aquatic"
neon_sites$type[which(neon_sites$siteID %in% (neon_sites_df$siteID[neon_sites_df$type == "Terrestrial"]))] <- "Terrestrial"

# Subset to aquatic
neon_sites <- neon_sites[neon_sites$type == "Aquatic", ]
neon_sites_df <- neon_sites_df[neon_sites_df$type == "Aquatic", ]

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")

# Statistics
stats <- list("Minimum" = "Min.", "1st Quartile" = "1st Qu.", "Median" = "Median", "Mean" = "Mean", "3rd Quartile" = "3rd Qu.", "Maximum" = "Max.", "Standard Deviation" = "sd")
mod_choices <- c("Decrease (negative relationship)", "Stay the same (no effect)", "Increase (positive relationship)")
assim_choices <- c("increase","decrease","stay the same")
# Sorting variables
state_vars <- c("Phytoplankton", "Nitrogen")
process_vars <- c("Mortality", "Uptake")

# Budget options
budget_options <- read.csv("data/wq_monitoring_budget2.csv", fileEncoding = "UTF-8-BOM")
budget_options$label <- paste0(budget_options$item, " ($", formatC(budget_options$cost, big.mark = ",", format = "d"), ")")
budget <- data.frame(variable = c("Budget"), value = c(10000), fill = c("Budget"))


# ggplot theme
mytheme <- theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
                 axis.text.x=element_text(size=18, colour='black'), axis.text.y=element_text(size=18, colour='black'),
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 strip.text.x = element_text(size=14), strip.text.y = element_text(size=14),
                 panel.background = element_rect(fill = NA, color = "black"),
                 panel.grid.major = element_line(colour = "gray"),
                 legend.text=element_text(size=16),
                 legend.title = element_text(size = 20))

# Theme for saving plots
png_theme <- theme(legend.position = "bottom",
                   legend.text = element_text(size = 14),
                   legend.title = element_text(size = 14))
# Linear regression variables ----
# lin_reg_vars <- read.csv("data/multiple_linear_regression_variables.csv",
#                          fileEncoding = "UTF-8-BOM")

# Forecast Date
fc_date <- "2020-09-25"

# Assimilation methods
assim_methods <- c("increase", "decrease", "stay the same")
view_vars <- data.frame(lname = c("Chlorophyll-a", "Nitrate", "Max uptake"),
                        sname = c("chla", "nitrate", "maxUptake"))

# Sampling frequency
samp_freq <- c("Monthly", "Fortnightly", "Weekly", "Daily")
samp_freq2 <- c("Month", "Fortnight", "Week", "Day")

# Uncertainty sources to include
uc_sources <- c("Process", "Parameter", "Initial Conditions", "Driver", "Total")

# Add last update time
app_time <- format(file.info("ui.R")$mtime, "%Y-%m-%d")
app_update_txt <- paste0("This app was last updated on: ", app_time)

# Tab names for updating buttons
tab_names <- read.csv("data/tab_names.csv", fileEncoding = "UTF-8-BOM")
rownames(tab_names) <- tab_names[, 1]


# Scenario Forecast #1
set.seed(123)
scen_fc1 <- data.frame(Date = seq.Date(as.Date("2021-08-16"), as.Date("2021-08-22"), by = 1),
                       surftemp = c(9.6, 9.8, 10.1, 11.1, 11.5, 11.4, 11.8),
                       bottemp = c(7.3, 7.6, 7.9, 8.2, 8.6, 9.2, 9.4))
scen_fc1$bottemp <- scen_fc1$bottemp + 1.9

bsd <- rnorm(nrow(scen_fc1), 1.2, 0.1) - 0.9
ssd <- rnorm(nrow(scen_fc1), 1.25, 0.18) - 0.9

scen_fc1$surf_uci <- scen_fc1$surftemp + ssd[order(ssd)]
scen_fc1$surf_lci <- scen_fc1$surftemp - ssd[order(ssd)]

scen_fc1$bot_uci <- scen_fc1$bottemp + bsd[order(bsd)]
scen_fc1$bot_lci <- scen_fc1$bottemp - bsd[order(bsd)]

# Scenario Forecast #2
scen_fc2 <- data.frame(Date = seq.Date(as.Date("2021-08-16"), as.Date("2021-08-22"), by = 1),
                       surftemp = c(9.6, 9.8, 10.1, 11.1, 11.5, 11.4, 11.8),
                       bottemp = c(7.3, 7.6, 7.9, 8.2, 8.6, 9.2, 9.4))
scen_fc2$bottemp <- scen_fc2$bottemp + 1.9
set.seed(123)
bsd <- rnorm(nrow(scen_fc2), 1.75, 0.6) - 0.9
ssd <- rnorm(nrow(scen_fc2), 1.2, 0.25) - 0.9

scen_fc2$surf_uci <- scen_fc2$surftemp + ssd[order(ssd)]
scen_fc2$surf_uci[5] <- scen_fc2$surftemp[5] + max(ssd)
scen_fc2$surf_lci <- scen_fc2$surftemp - ssd[order(ssd)]
scen_fc2$surf_lci[5] <- scen_fc2$surftemp[5] - max(ssd)

scen_fc2$bot_uci <- scen_fc2$bottemp + bsd[order(bsd)]
scen_fc2$bot_lci <- scen_fc2$bottemp - bsd[order(bsd)]

data_collect_options <- data.frame(text = c("Sensor A", #cheap, unreliable sensor
                                            "Sensor B"), #expensive, reliable sensor
                                   chla_freq = c(1, 1), obs_cv = c(runif(1,0.2,0.4), 0.1))


# Prep for Acti C
#set start date of forecast
actc_start_date <- "2020-10-02" 

#load NEON data and format for input into EnKF
actc_lake_data <- format_enkf_inputs(siteID = "BARC", neon_vars = neon_vars)

#get initial conditions for forecast
actc_yini <- get_yini(lake_data = actc_lake_data,
                 start_date = actc_start_date)

#load NOAA GEFS forecast
actc_noaa_fc <- list(list =load_noaa_forecast(siteID = "BARC", start_date = actc_start_date))

#get regression for water temp
lm_wt <- get_NEON_lm(siteID = "BARC", x = "Air temperature",
                     y = "Surface water temperature", start_date = actc_start_date)

#get regression for upar
lm_upar <- get_NEON_lm(siteID = "BARC", x = "Shortwave radiation",
                       y = "Underwater PAR", start_date = actc_start_date)

#convert NOAA GEFS forecast to water temp and upar forecast
actc_driver_file <- convert_forecast(lm_wt = lm_wt, lm_upar = lm_upar,
                                noaa_fc = actc_noaa_fc, start_date = actc_start_date)

barc_df <- format_enkf_inputs(siteID = "BARC", neon_vars = neon_vars)

idx <- which(barc_df$Date == actc_start_date)

# Historical data
df <- barc_df[(idx-7):(idx+1), ]
df$chla[nrow(df)] <- NA
df$Date <- as.Date(df$datetime)
df$maxUptake <- as.numeric(NA)
obs_plot_c <- list(hist = df)

# Future data
df <- barc_df[(idx+1):(idx+35), ]
df$Date <- as.Date(df$datetime)
df$maxUptake <- as.numeric(NA)
obs_plot_c$future <- df

gap_df <- obs_plot_c


# end
