shinyServer(function(input, output, session) {
  
  #### Presentation ----

  #** Recap Presentation slides ----
  output$slides <- renderSlickR({
    slickR(recap_slides) + settings(dots = TRUE)
  })
  
  #### Objective 1 ----

  # NEON Sites datatable ----
  output$table01 <- DT::renderDT(
    neon_sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE, dom = 't'), server = FALSE
  )

  # to keep track of previously selected row
  prev_row <- reactiveVal()
  siteID <- reactiveValues(lab = NULL)

  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')


  # Select NEON DT rows ----
  start_date <- "2020-09-25"
  noaa_fc <- reactiveValues(list = NULL, conv = NULL)
  lake_data <- reactiveValues(df = NULL)
  autocorrelation_data <- reactiveValues(df = NULL)
  obs_plot <- reactiveValues(hist = NULL, future = NULL)
  
  observeEvent(input$table01_rows_selected, {
    row_selected = neon_sites[input$table01_rows_selected, ]
    siteID$lab <- neon_sites$siteID[input$table01_rows_selected]
    coords <- st_coordinates(row_selected)
    colnames(coords) <- c("long", "lat")
    row_selected = cbind(row_selected, coords)
    proxy <- leafletProxy('neonmap')
    proxy %>%
      addAwesomeMarkers(layerId = as.character(row_selected$uid),
                        lng=row_selected$long,
                        lat=row_selected$lat,
                        icon = my_icon)

    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy %>%
        addMarkers(data = prev_row(),
                   layerId = as.character(prev_row()$uid))
    }
    # set new value to reactiveVal
    prev_row(row_selected)
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Loading NEON data",
                 detail = "This may take a while. This window will disappear
                     when it is loaded.", value = 0.33)
    #load NEON data and format for input into EnKF
    lake_data_file = paste0("./data/neon/",siteID$lab,"_chla_microgramsPerLiter.csv")
    lake_data$df <- read_csv(lake_data_file, show_col_types = FALSE) %>%
      rename(datetime = Date, chla = V1) %>%
      filter(cumsum(!is.na(chla)) > 0) %>%
      mutate(chla = ifelse(chla < 0, 0, chla))
   
    #create autocorrelation dataset
    autocorrelation_data$df <- lake_data$df %>%
      filter(datetime < start_date) %>%
      mutate(chla = na.approx(chla, na.rm = F)) %>% 
      mutate(chla_lag = lag(chla)) %>%
      filter(complete.cases(.))

    idx <- which(lake_data$df$Date == start_date)
    
  })

  # Neon map ----
  output$neonmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = neon_sites,
                 layerId = ~uid, clusterOptions = markerClusterOptions(),
                 label = ~locationDescription, icon = ~neonIcons[type])

  })

  # Download phenocam ----
  pheno_file <- reactiveValues(img = NULL)
  observeEvent(input$view_webcam, {

    req(!is.null(siteID$lab))
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Accessing and downloading phenocam image",
                 detail = "This may take a while. This window will disappear
                     when it is downloaded.", value = 0.5)

    p <- input$neonmap_marker_click
    idx <- which(neon_sites_df$siteID == siteID$lab)
    url <- neon_sites_df$pheno_url[idx]
    pheno_file$img <<- download_phenocam(url)
    progress$set(value = 1)
  })

  # Show phenocam image ----
  output$pheno <- renderImage({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in the table.")
    )
    validate(
      need(!is.null(pheno_file$img), "Click 'View latest photo' to download the image.")
    )
    list(src = pheno_file$img,
         alt = "Image failed to render. Please click 'Save plot' again.",
         height = 320,
         width = 350)
  }, deleteFile = FALSE)

  observeEvent(input$view_webcam, {
    output$prompt1 <- renderText({
      "Hover your cursor above the image to enlarge."
    })
  })

  # Download html ----
  observeEvent(input$table01_rows_selected, {
    p <- input$neonmap_marker_click  # typo was on this line
    sid <- neon_sites$siteID[input$table01_rows_selected]
    idx <- which(neon_sites_df$siteID == sid)
    # output$site_name <- neon_sites$description[idx]
    output$site_html <- renderUI({
      return(get_html(site_id = neon_sites_df$siteID[idx]))
    })
  })
  #** Create hyperlink ----
  observeEvent(input$table01_rows_selected, {
    sid <- neon_sites$siteID[input$table01_rows_selected]
    url <- paste0("https://www.neonscience.org/field-sites/field-sites-map/", sid)

    output$site_link <- renderUI({
      tags$a(href = url, "Click here for more site info", target = "_blank")
    })
  })
  #** Create prompt ----
  observeEvent(input$table01_rows_selected, {
    output$prompt2 <- renderText({
      "Click on the link below to find out more information about your site."
    })
  })

  #### Objective 2 ----
  
  # Plot chlorophyll-a
  plot.chla <- reactiveValues(main=NULL)
  
  output$chla_plot <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_chla > 0,
           message = "Click 'Plot chlorophyll-a'")
    )
    
    df <- lake_data$df
    
    p <- ggplot(data = df, aes(x = datetime, y = chla))+
      geom_line(aes(color = "Chl-a"))+
      xlab("")+
      ylab("Chlorophyll-a (ug/L)")+
      scale_color_manual(values = c("Chl-a" = "chartreuse4"), name = "")+
      theme_bw()
    
    plot.chla$main <- p
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Download plot of air and water temperature
  output$save_chla_plot <- downloadHandler(
    filename = function() {
      paste("Q5-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.chla$main, device = device)
    }
  )
  
  #### Objective 3 ----
  
  # Plot lagged chlorophyll-a time series
  plot.lag1 <- reactiveValues(main=NULL)
  
  output$lag_plot1 <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_lag1 > 0,
           message = "Click 'Plot lagged timeseries'")
    )
    
    df <- autocorrelation_data$df
    
    plot_data <- df %>%
      filter(datetime > "2019-06-01" & datetime < "2019-10-01")
    
    p <- plot_chla_lag(plot_data)
    
    plot.lag1$main <- p
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Download timeseries of lagged chl-a
  output$save_lag_plot1 <- downloadHandler(
    filename = function() {
      paste("Q7-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.lag1$main, device = device)
    }
  )
  
  # Plot lagged chlorophyll-a scatterplot
  plot.lag2 <- reactiveValues(main=NULL)
  
  output$lag_plot2 <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_lag2 > 0,
           message = "Click 'Plot lag scatterplot'")
    )
    
    df <- autocorrelation_data$df
    
    p <- ggplot(data = df, aes(x = chla_lag, y = chla))+
      geom_point()+
      xlab("1 day lag of chlorophyll-a (ug/L)")+
      ylab("chlorophyll-a (ug/L)")+
      geom_abline(slope = 1, intercept = 0, linetype = 2)+
      theme_bw()
    
    plot.lag2$main <- p
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Download scatterplot of lagged chl-a
  output$save_lag_plot2 <- downloadHandler(
    filename = function() {
      paste("Q8-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.lag2$main, device = device)
    }
  )
  
  # Autocorrelation calculation ----
  output$out_ac <- renderUI({
    
    validate(
      need(!is.null(autocorrelation_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$calc_ac > 0,
           message = "Click 'Calculate autocorrelation'")
    )
    
    df <- autocorrelation_data$df
    
    autocorrelation_lag1 = round(sum((df$chla[-1] - mean(df$chla[-1]))*(df$chla_lag[-1] - mean(df$chla[-1])))/sum((df$chla - mean(df$chla))^2),3)
    
    autocorrelation_lag1
    
    out_ac <- paste("<b>","Autocorrelation: ",autocorrelation_lag1,"</b>", sep = "")
    
    HTML(paste(out_ac))
  })
  
  # Autocorrelation plot many lags ----
  plot.ac <- reactiveValues(main=NULL)
  
  output$ac_plot <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_ac > 0,
           message = "Click 'Plot autocorrelation for many lags'")
    )
    
    df <- autocorrelation_data$df
    
    acf_list <- acf(df$chla, plot = FALSE)
    
    acf_plot_data <- tibble(Lag = acf_list$lag,
                            ACF = round(acf_list$acf, 2))
    
    p <- ggplot(data = acf_plot_data, aes(x = Lag, y = ACF))+
      geom_bar(stat = "identity", color = "#2CB572", fill = "#D4ECE1")+
      xlab("Lag in days")+
      ylab("Chl-a autocorrelation")+
      theme_bw()
    
    plot.ac$main <- p
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Download ac plot
  output$save_ac_plot <- downloadHandler(
    filename = function() {
      paste("Q10-Q11-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.ac$main, device = device)
    }
  )
  
  # PACF plot ----
  plot.pacf <- reactiveValues(main=NULL)
  
  output$pacf_plot <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$plot_pacf > 0,
           message = "Click 'Plot PACF'")
    )
    
    df <- autocorrelation_data$df
    
    pacf_list <- acf(df$chla, type = c("partial"), plot = FALSE)
    
    pacf_plot_data <- tibble(Lag = pacf_list$lag,
                             Partial_ACF = round(pacf_list$acf, 2))
    
    p <- ggplot(data = pacf_plot_data, aes(x = Lag, y = Partial_ACF))+
      geom_bar(stat = "identity", color = "deepskyblue4", fill = "darkslategray3")+
      xlab("Lag in days")+
      ylab("Partial autocorrelation of chl-a data")+
      theme_bw()
    
    plot.pacf$main <- p
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Download scatterplot of pacf
  output$save_pacf_plot <- downloadHandler(
    filename = function() {
      paste("Q12-Q14-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.pacf$main, device = device)
    }
  )
  
  # Text output of fitted AR model ----
  
  #create reactive model object
  ar.model <- reactiveValues(fit = NULL, intercept = NULL,
                             ar1 = NULL, chla_mean = NULL, eqn = NULL)
  
  output$ar_model <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$fit_model > 0,
           message = "Click 'Fit model'")
    )
    
    #assign dataframe
    df <- autocorrelation_data$df
    
    #fit model
    ar.model$fit <- ar.ols(df$chla, order.max = 1, aic = FALSE,
                       intercept = TRUE, demean = TRUE)
    
    #extract parameters
    ar.model$intercept = round(c(ar.model$fit$x.intercept),2) #beta_0
    ar.model$ar1 = round(c(ar.model$fit$ar),2) #beta_1
    ar.model$chla_mean = round(c(ar.model$fit$x.mean),2) #mean chla
    
    #make equation
    ar.model$eqn <- paste0("$$Chla_{t} = ",ar.model$intercept," + ",ar.model$ar1," * (Chla_{t-1} - ",ar.model$chla_mean,") + ",ar.model$chla_mean,"$$")
    
    return(withMathJax(
      tags$p(ar.model$eqn)
    ))
  })
  
  # Model fit plot ----
  model_fit_data <- reactiveValues(df = NULL)
  plot.arfit <- reactiveValues(main=NULL)
  
  output$arfit_plot <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$fit_model > 0,
           message = "Click 'Fit model'")
    )
    
    df <- autocorrelation_data$df
    
    mod <- ar.model$intercept + ar.model$ar1 * (df$chla - ar.model$chla_mean) + ar.model$chla_mean
    
    model_fit_data$df <- tibble(date = df$datetime,
                                chla = df$chla,
                                model = mod,
                                residuals = (mod - df$chla))
    
    p <- plot_mod_predictions(model_fit_plot_data = model_fit_data$df, variable_name = "Chlorophyll-a (ug/L)")
    
    plot.arfit$main <- p
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Download scatterplot of pacf
  output$save_arfit_plot <- downloadHandler(
    filename = function() {
      paste("Q15-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.arfit$main, device = device)
    }
  )
  
  # Text output for bias ----
  output$out_bias <- renderUI({
    
    validate(
      need(!is.null(autocorrelation_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model above.")
    )
    validate(
      need(input$calc_bias > 0,
           message = "Click 'Calculate bias'")
    )
    
    df <- model_fit_data$df
    
    bias <- round(mean(model_fit_data$df$model - model_fit_data$df$chla, na.rm = TRUE),4)
    
    out_bias <- paste("<b>","Bias: ",bias,"</b>", sep = "")
    
    HTML(paste(out_bias))
  })
  
  # Text output for RMSE ----
  output$out_rmse <- renderUI({
    
    validate(
      need(!is.null(autocorrelation_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model above.")
    )
    validate(
      need(input$calc_rmse > 0,
           message = "Click 'Calculate RMSE'")
    )
    
    df <- model_fit_data$df
    
    rmse <- round(sqrt(mean((model_fit_data$df$model - model_fit_data$df$chla)^2, na.rm = TRUE)), 2)
    
    out_rmse <- paste("<b>","RMSE: ",rmse,"</b>", sep = "")
    
    HTML(paste(out_rmse))
  })
  
  ## Objective 4 ----
  
  # Forecast uncertainty slides ----
  
  # Slickr model output
  output$fc_uc_slides <- renderSlickR({
    slickR(fc_uc_slides)
  })
  
  # Process uncertainty slides ----
  
  # Slickr model output
  output$proc_uc_slides <- renderSlickR({
    slickR(proc_uc_slides)
  })
  
  # Create reactive object for forecasts
  first_forecast <- reactiveValues(ic_distribution = NULL,
                                  process_distribution = NULL,
                                  curr_chla = NULL,
                                  n_members = NULL,
                                  forecast_chla = NULL,
                                  sigma = NULL,
                                  ic_sd = NULL)
  
  # Text output for proc uc sd ----
  output$proc_uc_sd <- renderUI({
    
    validate(
      need(!is.null(autocorrelation_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(input$calc_proc_distrib > 0,
           message = "Click 'Calculate process uncertainty distribution'")
    )
    
    df <- model_fit_data$df
    
    first_forecast$sigma <- sd(df$residuals, na.rm = TRUE)
    
    proc_uc_sd <- paste("<b>","Process uncertainty standard deviation: ",round(first_forecast$sigma,2),"</b>", sep = "")
    
    HTML(paste(proc_uc_sd))
  })
  
  # Process uncertainty distribution plot ----
  plot.proc.uc.distrib <- reactiveValues(main=NULL)
  
  output$proc_uc_distrib <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(input$calc_proc_distrib > 0,
           message = "Click 'Calculate process uncertainty distribution'")
    )
    
    first_forecast$n_members <- 500
    
    first_forecast$process_distribution <- rnorm(n = first_forecast$n_members, mean = 0, sd = first_forecast$sigma)
    
    p <- plot_process_dist(proc_uc = first_forecast$process_distribution)
    
    plot.proc.uc.distrib$main <- p
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # Download scatterplot of pacf
  output$save_proc_uc_distrib_plot <- downloadHandler(
    filename = function() {
      paste("Q19-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.proc.uc.distrib$main, device = device)
    }
  )
  
  #** Slickr Initial conditions UC slides ----
  output$ic_uc_slides <- renderSlickR({
    slickR(ic_uc_slides) + settings(dots = TRUE)
  })
  
  # High frequency data plot ----
  high_frequency_data <- reactiveValues(df=NULL)
  plot.high.freq <- reactiveValues(main=NULL)
  
  output$high_freq_plot <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(input$plot_high_freq > 0,
           message = "Click 'Plot high-frequency data'")
    )
    
    if(siteID$lab == "LIRO"){
      high_frequency_data$df <- read_csv("data/chla_microgramsPerLiter_highFrequency.csv", show_col_types = FALSE) %>%
        mutate(date = date(datetime),
               time = hms::as_hms(datetime)) %>%
        filter(site_id == siteID$lab & date >= "2019-09-15" & date <= "2019-09-18")
    } else if(siteID$lab == "PRPO") {
      high_frequency_data$df <- read_csv("data/chla_microgramsPerLiter_highFrequency.csv", show_col_types = FALSE) %>%
        mutate(date = date(datetime),
               time = hms::as_hms(datetime)) %>%
        filter(site_id == siteID$lab & date >= "2019-10-08" & date <= "2019-10-11")
    } else {
    high_frequency_data$df <- read_csv("data/chla_microgramsPerLiter_highFrequency.csv", show_col_types = FALSE) %>%
      mutate(date = date(datetime),
             time = hms::as_hms(datetime)) %>%
      filter(site_id == siteID$lab & date >= "2019-10-09" & date <= "2019-10-12")
    }
    
    p <- ggplot(data = high_frequency_data$df)+
      geom_line(aes(x = time, y = chla, group = date, color = as.factor(date)))+
      theme_bw()+
      labs(color = "Date")+
      xlab("Hour of day")+
      ylab("Chlorophyll-a (ug/L)")
    
    plot.high.freq$main <- p
    
    return(ggplotly(p, dynamicTicks = FALSE))
    
  })
  
  # Download scatterplot of pacf
  output$save_high_freq_plot <- downloadHandler(
    filename = function() {
      paste("Q21-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.high.freq$main, device = device)
    }
  )
  
  # Text output for ic uc sd ----
  output$ic_uc_sd <- renderUI({
    
    validate(
      need(!is.null(autocorrelation_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(high_frequency_data$df),
           message = "Please load and plot the high-frequency data above.")
    )
    validate(
      need(input$calc_ic_uc > 0,
           message = "Click 'Calculate initial conditions uncertainty'")
    )
    
    df <- high_frequency_data$df
    
    ic_sd_dataframe <- df %>%
      group_by(date) %>%
      summarize(daily_sd_chla = sd(chla, na.rm = TRUE))
    
    first_forecast$ic_sd <- mean(ic_sd_dataframe$daily_sd_chla, na.rm = TRUE)
    
    ic_uc_sd <- paste("<b>","Initial condition uncertainty standard deviation: ",round(first_forecast$ic_sd,2),"</b>", sep = "")
    
    HTML(paste(ic_uc_sd))
  })
  
  #IC distribution plot
  first_forecast_dates <- reactiveValues(start_date = NULL,
                                         forecast_date = NULL)
  plot.ic.uc.distrib <- reactiveValues(main=NULL)
  
  output$ic_distrib_plot <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(high_frequency_data$df),
           message = "Please load and plot the high-frequency data above.")
    )
    validate(
      need(input$calc_ic_uc > 0,
           message = "Click 'Calculate initial conditions uncertainty'")
    )
    
    df <- lake_data$df
    
    start_date <- "2020-09-25"
    first_forecast_dates$start_date <- "2020-09-25"
    
    curr_chla <- df %>%
      filter(abs(difftime(datetime,start_date)) == min(abs(difftime(datetime,start_date)))) %>%
      pull(chla)
    first_forecast$curr_chla <- curr_chla
    
    n_members <- as.numeric(first_forecast$n_members)
    ic_sd <- as.numeric(first_forecast$ic_sd)
    
    ic_distribution <- rnorm(n = n_members, mean = curr_chla, sd = ic_sd)
    first_forecast$ic_distribution <- ic_distribution
    
    p <- plot_ic_dist(curr_chla = curr_chla, ic_uc = ic_distribution)
    
    plot.ic.uc.distrib$main <- p
    
    return(ggplotly(p, dynamicTicks = FALSE))
    
  })
  
  # Download scatterplot of pacf
  output$save_ic_distrib_plot <- downloadHandler(
    filename = function() {
      paste("Q22-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.ic.uc.distrib$main, device = device)
    }
  )
  
  #First forecast plot
  plot.fc1 <- reactiveValues(main=NULL)
  
  output$fc1_plot <- renderPlotly({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(first_forecast$process_distribution),
           message = "Please generate a process uncertainty distribution above.")
    )
    validate(
      need(!is.null(first_forecast$ic_distribution),
           message = "Please generate an initial conditions distribution above.")
    )
    validate(
      need(input$fc1 > 0,
           message = "Click 'Generate forecast'")
    )
    
    intercept = as.numeric(ar.model$intercept)
    ar1 = as.numeric(ar.model$ar1)
    chla_mean = as.numeric(ar.model$chla_mean)
    ic_distribution = as.numeric(first_forecast$ic_distribution)
    process_distribution = as.numeric(first_forecast$process_distribution)
    forecast_date = "2020-09-26"
    first_forecast_dates$forecast_date <- forecast_date
    
    forecast_chla = intercept + ar1 * (ic_distribution - chla_mean) + chla_mean + process_distribution
    first_forecast$forecast_chla <- forecast_chla
    
    p <- plot_fc_dist(forecast_dist = forecast_chla)
    
    plot.fc1$main <- p
    
    return(ggplotly(p, dynamicTicks = FALSE))
    
  })
  
  
  #First forecast visualization over time
  plot.fc1.viz <- reactiveValues(main=NULL)
  
  output$fc1_viz_plot <- renderPlot({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(first_forecast$process_distribution),
           message = "Please generate a process uncertainty distribution above.")
    )
    validate(
      need(!is.null(first_forecast$ic_distribution),
           message = "Please generate an initial conditions distribution above.")
    )
    validate(
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate a forecast above.")
    )
    validate(
      need(input$fc1_viz > 0,
           message = "Click 'Visualize forecast'")
    )
    
    curr_chla = as.numeric(first_forecast$curr_chla)
    start_date = first_forecast_dates$start_date
    forecast_date = first_forecast_dates$forecast_date
    ic_distribution = as.numeric(first_forecast$ic_distribution)
    forecast_chla = as.numeric(first_forecast$forecast_chla)
    n_members = as.numeric(first_forecast$n_members)
    
    p <- plot_fc_1day(curr_chla, start_date, forecast_date, ic_distribution, forecast_chla, n_members)
    
    plot.fc1.viz$main <- p
    
    return(p)
    
  })
  
  # Download plot
  output$save_fc1_viz_plot <- downloadHandler(
    filename = function() {
      paste("Q23-Q25-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.fc1.viz$main, device = device)
    }
  )
  
  ## Objective 5 ----
  
  # create reactive for EnKF inputs
  EnKF_inputs_outputs <- reactiveValues(new_obs = NULL,
                                        updated_ic = NULL,
                                        second_forecast_date = NULL,
                                        second_forecast_da = NULL,
                                        updated_ic_no_da = NULL,
                                        second_forecast_no_da = NULL)
  
  # Text output for new observation ----
  output$new_obs <- renderUI({
    
    validate(
      need(!is.null(autocorrelation_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(plot.fc1.viz$main),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$view_new_obs > 0,
           message = "Click 'View new observation'")
    )
    
    df <- lake_data$df
    
    new_obs <- df %>%
      filter(datetime == first_forecast_dates$forecast_date) %>%
      pull(chla)
    
    EnKF_inputs_outputs$new_obs <- new_obs
    
    new_obs_text <- paste("<b>","New observation: ",round(new_obs,2),"</b>", sep = "")
    
    HTML(paste(new_obs_text))
  })
  
  #Updated initial condition figure
  plot.updated.ic <- reactiveValues(main=NULL)
  
  output$updated_ic_plot <- renderPlot({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(plot.fc1.viz$main),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    
    forecast_chla <- first_forecast$forecast_chla
    ic_sd <- first_forecast$ic_sd
    curr_chla = as.numeric(first_forecast$curr_chla)
    start_date = first_forecast_dates$start_date
    forecast_date = first_forecast_dates$forecast_date
    ic_distribution = as.numeric(first_forecast$ic_distribution)
    forecast_chla = as.numeric(first_forecast$forecast_chla)
    n_members = as.numeric(first_forecast$n_members)
    
    if(input$update_ic > 0 & input$view_new_obs > 0){
      previous_plot <- plot.fc1.viz$main
      new_obs <- EnKF_inputs_outputs$new_obs
      chla_obs <- c(curr_chla, new_obs) #vector of observations to use for plotting
      ic_update <- EnKF(forecast = forecast_chla, new_observation = new_obs, ic_sd = ic_sd)
      EnKF_inputs_outputs$updated_ic <- ic_update
      p1 <- plot_fc_update(chla_obs, start_date, forecast_date, ic_distribution, ic_update, forecast_chla, n_members)
      plot.updated.ic$main <- p1
      return(p1)
    } else if(input$view_new_obs > 0) {
      previous_plot <- plot.fc1.viz$main
      new_obs <- EnKF_inputs_outputs$new_obs
      p <- plot_fc_new_obs(previous_plot = previous_plot, new_obs = new_obs, forecast_date = forecast_date)
      plot.updated.ic$main <- p
      return(p)
    } else {
      previous_plot <- plot.fc1.viz$main
      plot.updated.ic$main <- previous_plot
      return(previous_plot)
    }
    
  })
  
  # Download plot
  output$save_updated_ic_plot <- downloadHandler(
    filename = function() {
      paste("Q27-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.updated.ic$main, device = device)
    }
  )
  
  # Second forecast figure
  plot.second.fc.da <- reactiveValues(main=NULL)
  
  output$second_fc_da_plot <- renderPlot({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(plot.fc1.viz$main),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$update_ic > 0,
           message = "Please update the initial condition above.")
    )
    validate(
      need(input$second_forecast_da > 0,
           message = "Please click 'Generate forecast'.")
    )
    
    #unpacking values we will need
    intercept = as.numeric(ar.model$intercept)
    ar1 = as.numeric(ar.model$ar1)
    chla_mean = as.numeric(ar.model$chla_mean)
    ic_update = EnKF_inputs_outputs$updated_ic
    process_distribution = as.numeric(first_forecast$process_distribution)
    
    #generate second forecast
    second_forecast = intercept + ar1 * (ic_update - chla_mean) + chla_mean + process_distribution
    EnKF_inputs_outputs$second_forecast_da = second_forecast
    
    #add in new dates
    second_forecast_date = "2020-09-27"
    EnKF_inputs_outputs$second_forecast_date <- second_forecast_date
    forecast_date = first_forecast_dates$forecast_date
    forecast_dates = c(forecast_date, second_forecast_date)
    EnKF_inputs_outputs$forecast_dates = forecast_dates
    
    curr_chla = as.numeric(first_forecast$curr_chla)
    new_obs = EnKF_inputs_outputs$new_obs
    chla_obs = c(curr_chla, new_obs) #vector of observations to use for plotting
    start_date = first_forecast_dates$start_date
    ic_distribution = as.numeric(first_forecast$ic_distribution)
    forecast_chla = as.numeric(first_forecast$forecast_chla)
    n_members = as.numeric(first_forecast$n_members)
    
    p <- plot_second_forecast(chla_obs, start_date, forecast_dates, ic_distribution, 
                         ic_update, forecast_chla, second_forecast, n_members)
    
    plot.second.fc.da$main <- p
    
    return(p)
    
  })
  
  # Download plot
  output$save_second_fc_da_plot <- downloadHandler(
    filename = function() {
      paste("Q28-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.second.fc.da$main, device = device)
    }
  )
  
  #Initial condition no DA figure
  plot.updated.ic.no.da <- reactiveValues(main=NULL)
  
  output$updated_ic_no_da_plot <- renderPlot({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(plot.fc1.viz$main),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    
    forecast_chla <- first_forecast$forecast_chla
    ic_sd <- first_forecast$ic_sd
    curr_chla = as.numeric(first_forecast$curr_chla)
    start_date = first_forecast_dates$start_date
    forecast_date = first_forecast_dates$forecast_date
    ic_distribution = as.numeric(first_forecast$ic_distribution)
    forecast_chla = as.numeric(first_forecast$forecast_chla)
    n_members = as.numeric(first_forecast$n_members)
    
    if(input$view_ic_no_da > 0 & input$show_ic == TRUE){
      previous_plot <- plot.fc1.viz$main
      new_obs <- NA
      chla_obs <- c(curr_chla, new_obs) #vector of observations to use for plotting
      ic_update_no_da <- EnKF(forecast = forecast_chla, new_observation = new_obs, ic_sd = ic_sd)
      EnKF_inputs_outputs$updated_ic_no_da <- ic_update_no_da
      p1 <- plot_fc_update(chla_obs, start_date, forecast_date, ic_distribution, ic_update_no_da, forecast_chla, n_members)
      plot.updated.ic.no.da$main <- p1
      return(p1)
    } else {
      previous_plot <- plot.fc1.viz$main
      plot.updated.ic.no.da$main <- previous_plot
      return(previous_plot)
    }
    
  })
  
  # Download plot
  output$save_updated_ic_no_da_plot <- downloadHandler(
    filename = function() {
      paste("Q29-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.updated.ic.no.da$main, device = device)
    }
  )
  
  # Second forecast figure
  plot.second.fc.no.da <- reactiveValues(main=NULL)
  
  output$second_fc_no_da_plot <- renderPlot({ 
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(plot.fc1.viz$main),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$view_ic_no_da > 0,
           message = "Please click 'Run ensemble Kalman filter with missing observation' above.")
    )
    validate(
      need(input$second_forecast_no_da > 0,
           message = "Please click 'Generate forecast'.")
    )
    
    #unpacking values we will need
    intercept = as.numeric(ar.model$intercept)
    ar1 = as.numeric(ar.model$ar1)
    chla_mean = as.numeric(ar.model$chla_mean)
    ic_update_no_da = EnKF_inputs_outputs$updated_ic_no_da
    process_distribution = as.numeric(first_forecast$process_distribution)
    
    #generate second forecast
    second_forecast_no_da = intercept + ar1 * (ic_update_no_da - chla_mean) + chla_mean + process_distribution
    EnKF_inputs_outputs$second_forecast_no_da = second_forecast_no_da
    
    #assign dates
    forecast_dates = EnKF_inputs_outputs$forecast_dates

    curr_chla = as.numeric(first_forecast$curr_chla)
    new_obs = NA
    chla_obs = c(curr_chla, new_obs) #vector of observations to use for plotting
    start_date = first_forecast_dates$start_date
    ic_distribution = as.numeric(first_forecast$ic_distribution)
    forecast_chla = as.numeric(first_forecast$forecast_chla)
    n_members = as.numeric(first_forecast$n_members)
    
    p <- plot_second_forecast(chla_obs, start_date, forecast_dates, ic_distribution, 
                              ic_update_no_da, forecast_chla, second_forecast_no_da, n_members)
    
    plot.second.fc.no.da$main <- p
    
    return(p)
    
  })
  
  # Download plot
  output$save_second_fc_no_da_plot <- downloadHandler(
    filename = function() {
      paste("Q30-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.second.fc.no.da$main, device = device)
    }
  )
  
  
  
  
  
  ##########OLD

  # NEON variable description table ----
  output$var_desc <- renderDT({
    var_desc <- neon_vars[!duplicated(neon_vars$Short_name), c("Short_name", "description")]
    colnames(var_desc) <- c("Name", "Description")
    datatable(var_desc, rownames = FALSE, options = list(pageLength = 4))
  })

  # Site data datatable ----
  output$neon_datatable <- DT::renderDT({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    df <- neon_DT()$data
    df[, -1] <- signif(df[, -1], 4)
    df[, 1] <- format(df[, 1], format = "%Y-%m-%d")
    names(df)[ncol(df)] <- read_var
    return(df)
  })

  # Variable description ----
  output$txt_out <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    out_txt <- neon_vars$description[which(neon_vars$Short_name == input$view_var)][1]
    return(out_txt)
  })

  # Site data plot ----
  output$var_plot <- renderPlotly({

    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    read_var <- neon_vars$id[which(neon_vars$Short_name == input$view_var)][1]
    units <- neon_vars$units[which(neon_vars$Short_name == input$view_var)][1]
    file <- file.path("data", "neon", paste0(siteID$lab, "_", read_var, "_", units, ".csv"))
    validate(
      need(file.exists(file), message = "This variable is not available at this site. Please select a different variable or site.")
    )
    obj <- neon_DT()$sel

    p <- ggplot() +
      geom_point(data = neon_DT()$data, aes_string(names(neon_DT()$data)[1], names(neon_DT()$data)[2]), color = "black") +
      ylab(paste0(input$view_var, " (", units, ")")) +
      xlab("Time") +
      theme_bw(base_size = 12)

    if(!is.null(obj)) {
      p <- p +
        geom_point(data = obj, aes_string(names(obj)[1], names(obj)[2]), color = cols[2])

    }
    return(ggplotly(p, dynamicTicks = TRUE, source = "A"))
  })
  
  # Slickr model output
  output$model_slides <- renderSlickR({
    slickR(model_slides)
  })


  # Variable relationships plot ----
  output$xy_plot <- renderPlotly({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    validate(
      need(input$x_var != "",
           message = "Please select a X variable.")
    )
    validate(
      need(input$y_var != "",
           message = "Please select a Y variable.")
    )
    
    ref <- input$x_var

    x_var <- neon_vars$id[which(neon_vars$Short_name == ref)][1]
    x_units <- neon_vars$units[which(neon_vars$Short_name == ref)][1]
    x_file <- file.path("data", "neon", paste0(siteID$lab, "_", x_var, "_", x_units, ".csv"))
    validate(
      need(file.exists(x_file), message = paste0(ref, " is not available at this site. Please select a different X variable."))
    )
    xvar <- read.csv(x_file)
    xvar[, 1] <- as.POSIXct(xvar[, 1], tz = "UTC")
    xvar$Date <- as.Date(xvar[, 1])
    if(ref == "Surface water temperature") {
      xvar <- xvar[xvar[, 2] == min(xvar[, 2], na.rm = TRUE), c(1, 3)] # subset to Surface water temperature
    }
    xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
    ref2 <- input$y_var
    y_var <- neon_vars$id[which(neon_vars$Short_name == ref2)][1]
    y_units <- neon_vars$units[which(neon_vars$Short_name == ref2)][1]
    y_file <- file.path("data", "neon", paste0(siteID$lab, "_", y_var, "_", y_units, ".csv"))
    validate(
      need(file.exists(y_file), message = paste0(ref2, " is not available at this site. Please select a different Y variable."))
    )
    yvar <- read.csv(y_file)
    yvar[, 1] <- as.POSIXct(yvar[, 1], tz = "UTC")
    yvar$Date <- as.Date(yvar[, 1])
    if(ref2 == "Surface water temperature") {
      yvar <- yvar[yvar[, 2] == min(yvar[, 2], na.rm = TRUE), c(1, 3)] # subset to Surface water temperature
    }
    yvar <- plyr::ddply(yvar, c("Date"), function(y) mean(y[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp
    
    df <- merge(xvar, yvar, by = "Date")
    
    validate(
      need(nrow(df) > 0, message = "No variables at matching timesteps. Please select different  X-Y variables.")
    )
    colnames(df)[-1] <- c("X", "Y")
    p <- ggplot(df, aes_string(names(df)[2], names(df)[3])) +
      geom_point() +
      xlab(paste0(input$x_var, " (", x_units, ")")) +
      ylab(paste0(input$y_var, " (", y_units, ")")) +
      theme_minimal(base_size = 12)
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  #Check answers to Q7
  
  observeEvent(input$ans_btn2, {
    if(input$q7a == "Increase (positive relationship)") {
      res <- "Q.7a is correct!"
    } else {
      res <- "Incorrect answer for Q.7a"
    }
    
    if(input$q7b == "Increase (positive relationship)") {
      res2 <- "Q.7b is correct!"
    } else {
      res2 <- "Incorrect answer for Q.7b"
    }
    
    if(input$q7c == "Increase (positive relationship)") {
      res3 <- "Q.7c is correct!"
    } else {
      res3 <- "Incorrect answer for Q.7c"
    }
    
    output$q7a_ans <- renderText({
      res
    })
    output$q7b_ans <- renderText({
      res2
    })
    output$q7c_ans <- renderText({
      res3
    })
  })
  
  #* Toggle for dataframe answers for matching states and params
  observeEvent(input$ans_btn, {
    # if(input$ans_btn %% 2 != 1 |){
    #   hide(id = "ans_vars")
    # }else{
    show(id = "ans_vars")
    # }
    # toggle("ans_vars")
  })
  
  observeEvent(input$ans_btn, {
    if(length(input$rank_list_2) == 0) {
      res <- "Drag answers into State box!"
    } else if(all(input$rank_list_2 %in% state_vars)) {
      res <- "State variables are correct!"
    } else {
      res <- "Incorrect answer in State box"
    }
    
    if(length(input$rank_list_3) == 0) {
      res2 <- "Drag answers into Parameter box!"
    } else if(all(input$rank_list_3 %in% process_vars)) {
      res2 <- "Parameter variables are correct!"
    } else {
      res2 <- "Incorrect answer in Parameter box"
    }
    
    output$state_ans <- renderText({
      res
    })
    output$proc_ans <- renderText({
      res2
    })
  })
  
  # Q9 table
  q9_ans <- reactiveValues() # %>% formatStyle(c(1:3), border = '1px solid #ddd'))
  q9_ans$df <- data.frame(`Variable relationship` = c("Primary productivity vs. water temperature", "Primary productivity vs. light", "Primary productivity vs. nutrients"),
                          `Q.6 Answers` = rep(NA, 3), 
                          `Q.7 Answers` = rep(NA, 3) 
  )
  
  observeEvent(input$submitButtonQ6, {
    Q6_ans <- c(input$q6b, input$q6c, input$q6d)
    q9_ans$df[,2] <- Q6_ans
  })
  
  observeEvent(input$submitButtonQ7, {
    Q7_ans <- c(input$q7a, input$q7b, input$q7c)
    q9_ans$df[,3] <- Q7_ans
  })
  
  output$q9_tab <- DT::renderDT(
    q9_ans$df, #%>% formatStyle(c(1:dim(q6_ans$dt)[2]), border = '1px solid #ddd'),
    selection = "none", class = "cell-border stripe",
    options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t"),
    server = FALSE, escape = FALSE, rownames = FALSE, colnames=c("Variable relationship","Q.6 Answers","Q.7 Answers"), editable = FALSE
  )
  # q9_proxy <- dataTableProxy("q9_tab")
  # observeEvent(input$q9_tab_cell_edit, {
  #   info = input$q9_tab_cell_edit
  #   i = info$row
  #   j = info$col
  #   v = info$value
  #   q9_ans$dt[i, j] <<- DT::coerceValue(v, q9_ans$dt[i, j])
  #   # replaceData(q6_proxy, q6_ans$dt, resetPaging = FALSE)  # important
  # })
  
  
  #** Save air and water temp ----
  selected2 <- reactiveValues(sel = NULL)
  observeEvent(input$clear_sel2, {
    selected2$sel <- NULL
    lm_wt$sub <- NULL
    lm_wt$m <- NULL
    lm_wt$b <- NULL
    lm_wt$r2 <- NULL
    lm_wt$sigma <- NULL
  })
  
  #selected
  observe({
    # suppress warnings
    storeWarn<- getOption("warn")
    options(warn = -1)
    selected2$sel <- event_data(event = "plotly_selected", source = "B")
    
    #restore warnings, delayed so plot is completed
    shinyjs::delay(expr =({
      options(warn = storeWarn)
    }) ,ms = 100)
  })
  
  lm_wt <- reactiveValues(sub = NULL, m = NULL, b = NULL, r2 = NULL, sigma = NULL)
  
  observeEvent(input$add_lm2, {
    # if(is.null(selected2$sel)) {
    #   df <- wtemp_airtemp()$data
    # } else {
    #   df <- selected2$sel[, 2:4]
    # }
    df <- wtemp_airtemp()$qaqc
    fit <- lm(df[, 3] ~ df[, 2])
    coeffs <- fit$coefficients
    lm_wt$sub <- df
    lm_wt$m <- round(coeffs[2], 2)
    lm_wt$b <- round(coeffs[1], 2)
    lm_wt$r2 <- round(summary(fit)$r.squared, 2)
    lm_wt$sigma <- sigma(fit)
  })
  
  output$lm2_r2 <- renderText({
    validate(
      need(!is.null(lm_wt$r2),
           message = "Please click 'Add linear regression'.")
    )
    if(!is.null(lm_wt$r2)) {
      paste0("R2 = ", lm_wt$r2)
    } else {
      "R2 = NULL"
    }
  })
  
  output$lm2_eqn <- renderUI({
    validate(
      need(!is.null(lm_wt$m),
           message = "Please click 'Add linear regression'.")
    )
    formula <- "$$ wtemp = %s \u00D7 airtemp + %s   ;   r^2 = %s $$"
    text <- sprintf(formula, lm_wt$m, lm_wt$b, lm_wt$r2)
    withMathJax(
      tags$p(text)
    )
  })
  
  wtemp_airtemp <- reactive({ # view_var
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    df <- na.exclude(lake_data$df[, c("datetime", "airt", "wtemp")])
    
    validate(
      need(nrow(df) > 0, message = "No variables at matching timesteps.")
    )
    colnames(df)[-1] <- c("X", "Y")
    sel <- tryCatch(df[(selected2$sel$pointNumber+1),,drop=FALSE] , error=function(e){NULL})
    qaqc <- df[df$Y != 5.2300000,]
    return(list(data = df, qaqc = qaqc, sel = sel))
  })
  
  # Air temp vs Water temp plot ----
  
  output$at_wt <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    obj <- wtemp_airtemp()$sel
    
    p <- ggplot() +
      geom_point(data = wtemp_airtemp()$data, aes_string(names(wtemp_airtemp()$data)[2], names(wtemp_airtemp()$data)[3]), color = "black") +
      ylab("Surface water temperature (\u00B0C)") +
      xlab("Air temperature (\u00B0C)") +
      theme_minimal(base_size = 12)
    
    if(nrow(obj) != 0) {
      p <- p +
        geom_point(data = obj, aes_string(names(obj)[2], names(obj)[3]), color = cols[2])
    }
    if(!is.null(lm_wt$m)) {
      p <- p +
        geom_abline(slope = lm_wt$m, intercept = lm_wt$b, color = cols[2], linetype = "dashed")
    }
    return(ggplotly(p, dynamicTicks = TRUE, source = "B"))
  })
  
  observeEvent(input$run_qaqc1, {
    
               output$at_wt <- renderPlotly({
                 validate(
                   need(input$table01_rows_selected != "",
                        message = "Please select a site in Objective 1.")
                 )
                 obj <- wtemp_airtemp()$sel
                 
                 p <- ggplot() +
                   geom_point(data = wtemp_airtemp()$data, aes_string(names(wtemp_airtemp()$data)[2], names(wtemp_airtemp()$data)[3]), color = "gray") +
                   geom_point(data = wtemp_airtemp()$qaqc, aes_string(names(wtemp_airtemp()$data)[2], names(wtemp_airtemp()$data)[3]), color = "black") +
                   ylab("Surface water temperature (\u00B0C)") +
                   xlab("Air temperature (\u00B0C)") +
                   theme_minimal(base_size = 12)
                 
                 if(nrow(obj) != 0) {
                   p <- p +
                     geom_point(data = obj, aes_string(names(obj)[2], names(obj)[3]), color = cols[2])
                 }
                 if(!is.null(lm_wt$m)) {
                   p <- p +
                     geom_abline(slope = lm_wt$m, intercept = lm_wt$b, color = cols[2], linetype = "dashed")
                 }
                 return(ggplotly(p, dynamicTicks = TRUE, source = "B"))
               })
               })
  
  
  
  #** Save SWR and uPAR ----
  selected3 <- reactiveValues(sel = NULL)
  observeEvent(input$clear_sel3, {
    selected3$sel <- NULL
    lm_upar$sub <- NULL
    lm_upar$m <- NULL
    lm_upar$b <- NULL
    lm_upar$r2 <- NULL
    lm_upar$sigma <- NULL
  })
  
  #selected
  observe({
    # suppress warnings
    storeWarn<- getOption("warn")
    options(warn = -1)
    selected3$sel <- event_data(event = "plotly_selected", source = "C")
    
    #restore warnings, delayed so plot is completed
    shinyjs::delay(expr =({
      options(warn = storeWarn)
    }) ,ms = 100)
  })
  
  swr_upar <- reactive({ # view_var
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    df <- na.exclude(lake_data$df[, c("datetime", "swr", "par")])
    validate(
      need(nrow(df) > 0, message = "No variables at matching timesteps.")
    )
    colnames(df)[-1] <- c("X", "Y")
    sel <- tryCatch(df[(selected3$sel$pointNumber+1),,drop=FALSE] , error=function(e){NULL})
    qaqc <- df[df$Y >= 10,]
    return(list(data = df, qaqc = qaqc, sel = sel))
  })
  
  lm_upar <- reactiveValues(sub = NULL, m = NULL, b = NULL, r2 = NULL, sigma = NULL)
  
  observeEvent(input$add_lm3, {
    # if(is.null(selected3$sel)) {
    #   df <- swr_upar()$data
    # } else {
    #   df <- selected3$sel[, 2:4]
    # }
    df <- swr_upar()$qaqc
    fit <- lm(df[, 3] ~ df[, 2])
    coeffs <- fit$coefficients
    lm_upar$sub <- df
    lm_upar$m <- round(coeffs[2], 2)
    lm_upar$b <- round(coeffs[1], 2)
    lm_upar$r2 <- round(summary(fit)$r.squared, 2)
    lm_upar$sigma <- sigma(fit)
  })
  
  output$lm3_r2 <- renderText({
    validate(
      need(!is.null(lm_upar$r2),
           message = "Please click 'Add linear regression'.")
    )
    if(!is.null(lm_upar$m)) {
      r2 <- round(lm_upar$r2, 2)
      paste0("R2 = ", r2)
    } else {
      "R2 = NULL"
    }
  })
  
  output$lm3_eqn <- renderUI({
    validate(
      need(!is.null(lm_upar$m),
           message = "Please click 'Add linear regression'.")
    )
    if(lm_upar$b < 0) {
      formula <- "$$ uPAR = %s \u00D7 SWR %s   ;   r^2 = %s $$"
    } else {
      formula <- "$$ uPAR = %s \u00D7 SWR + %s   ;   r^2 = %s $$"
    }
    text <- sprintf(formula, lm_upar$m, lm_upar$b, lm_upar$r2)
    withMathJax(
      tags$p(text)
    )
  })
  
  # SWR vs uPAR plot ----
  output$sw_upar <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    
    obj <- swr_upar()$sel
    
    p <- ggplot() +
      geom_point(data = swr_upar()$data, aes_string(names(swr_upar()$data)[2], names(swr_upar()$data)[3]), color = "black") +
      ylab("Underwater PAR (micromolesPerSquareMeterPerSecond)") +
      xlab("Shortwave radiation (wattsPerSquareMeter)") +
      theme_minimal(base_size = 12)
    
    if(nrow(obj) != 0) {
      p <- p +
        geom_point(data = obj, aes_string(names(obj)[2], names(obj)[3]), color = cols[2])
    }
    if(!is.null(lm_upar$m)) {
      p <- p +
        geom_abline(slope = lm_upar$m, intercept = lm_upar$b, color = cols[2], linetype = "dashed")
    }
    
    return(ggplotly(p, dynamicTicks = TRUE, source = "C"))
    
  })
  
  observeEvent(input$run_qaqc2, {
    output$sw_upar <- renderPlotly({
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      
      obj <- swr_upar()$sel
      
      p <- ggplot() +
        geom_point(data = swr_upar()$data, aes_string(names(swr_upar()$data)[2], names(swr_upar()$data)[3]), color = "gray") +
        geom_point(data = swr_upar()$qaqc, aes_string(names(swr_upar()$data)[2], names(swr_upar()$data)[3]), color = "black") +
        ylab("Underwater PAR (micromolesPerSquareMeterPerSecond)") +
        xlab("Shortwave radiation (wattsPerSquareMeter)") +
        theme_minimal(base_size = 12)
      
      if(nrow(obj) != 0) {
        p <- p +
          geom_point(data = obj, aes_string(names(obj)[2], names(obj)[3]), color = cols[2])
      }
      if(!is.null(lm_upar$m)) {
        p <- p +
          geom_abline(slope = lm_upar$m, intercept = lm_upar$b, color = cols[2], linetype = "dashed")
      }
      
      return(ggplotly(p, dynamicTicks = TRUE, source = "C"))
      
    })
  })
  
  #** Convert NOAA forecast data ----
  observeEvent(input$conv_fc, {
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$m),
           message = "Please add a regression line for the air vs. water temperature.")
    )
    validate(
      need(!is.null(lm_upar$m),
           message = "Please add a regression line for the SWR vs. uPAR.")
    )
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Converting NOAA data."),
                 detail = "This window will disappear when it is finished converting.", value = 0.01)
    
    mlt1 <- convert_forecast(lm_wt = lm_wt, lm_upar = lm_upar, noaa_fc = noaa_fc, start_date = start_date)
    progress$set(value = 1)
    noaa_fc$conv <- mlt1
    if(min(mlt1$upar, na.rm = TRUE) <= 0) {
      showModal(modalDialog(
        title = "Uh oh!",
        "Inspect your Underwater PAR plot. It looks like you have negative values which isn't possible!
        Adjust your linear regression and convert the forecast again."
      ))
    }
  })
  
  #** Plot of converted data
  output$conv_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$m),
           message = "Please add a regression line for the air vs. water temperature.")
    )
    validate(
      need(!is.null(lm_upar$m),
           message = "Please add a regression line for the SWR vs. uPAR.")
    )
    validate(
      need(!is.null(noaa_fc$conv),
           message = "Click 'Convert forecast'.")
    )
    validate(
      need(input$conv_fc > 0, "Click 'Convert forecast'.")
    )
    
    mlt1 <- noaa_fc$conv
    mlt2 <- reshape2::melt(mlt1, id.vars = c("date", "fc_date", "L1"))
    
    p <- ggplot()
    p <- p +
      geom_line(data = mlt2, aes(date, value, group = L1, color = fc_date)) +
      scale_color_manual(values = p.cols[2]) +
      facet_wrap(~variable, scales = "free_y", nrow = 2,
                 strip.position = "left",
                 labeller = as_labeller(c(wtemp = "Water temperature (\u00B0C)", upar = "Underwater PAR (µmol m-2 s-1)") )) +
      labs(color = "Forecast date") +
      xlab("Time") +
      theme_minimal(base_size = 12) +
      ylab(NULL) +
      theme(strip.background = element_blank(),
            strip.placement = "outside")
    
    gp <- ggplotly(p, dynamicTicks = TRUE)
    return(gp)
  })
  
  
  
  #** Initial Condition Uncertainty ----
  ic_dist <- reactiveValues(df = NULL)
  
  #** Generate IC distribution w/ changing initial condition values ----
  ic_plot <- reactiveValues(plot = NULL)
  observeEvent(input$gen_ic1, {
    req(input$table01_rows_selected != "")
    req(!is.null(lake_data$df))
    mn_chla <- lake_data$df$chla[lake_data$df$Date == start_date]
    dat <- data.frame(value = rnorm(1000, input$ic_val, input$ic_uc))
    dat$value[dat$value < 0.1] <- 0.1 # If below the min, set to a non-zero value
    ic_dist$df <- dat
    
    df <- data.frame(x = lake_data$df$chla[lake_data$df$Date == start_date],
                     label = "Observed")
    
    dens <- density(ic_dist$df$value)
    df <- data.frame(x = dens$x, y = dens$y)
    probs <- c(0, 0.025, 0.125, 0.875, 0.975, 1)
    quantiles <- quantile(ic_dist$df$value, prob = probs)
    df$quant <- findInterval(df$x, quantiles)
    df$quant[df$quant == 4] <- 2
    df$quant[df$quant == 5] <- 1
    df$quant[df$quant == 6] <- 0
    df$quant <- factor(df$quant)
    
    xlims <- c(input$ic_val - (3 * input$ic_uc), input$ic_val + (3 * input$ic_uc))
    ylims <- c(0, max(df$y) + 1)
    
    ic_plot$plot1 <- ggplot(df, aes(x,y)) + 
      geom_ribbon(aes(ymin = 0, ymax = y, fill = quant)) + 
      geom_vline(xintercept = input$ic_val, linetype = "dashed") + 
      scale_fill_brewer(guide = "none", palette = "OrRd") +
      xlab("Chlorophyll-a (μg/L)") +
      ylab("Density") +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      theme_bw(base_size = 18)
  })
  
  
  
  #** Plot - IC distribution ----
  output$ic_uc_plot1 <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(ic_plot$plot1), "Click 'Generate distribution'")
    )
    ic_plot$plot1
  })
  
  # Reset plot
  observeEvent(input$ic_val, {
    ic_plot$plot1 <- NULL
  })
  
  #** Generate IC distribution w/ changing initial condition uncertainty ----
  observeEvent(input$gen_ic2, {
    req(input$table01_rows_selected != "")
    req(!is.null(lake_data$df))
    mn_chla <- lake_data$df$chla[lake_data$df$Date == start_date]
    dat <- data.frame(value = rnorm(1000, input$ic_val, input$ic_uc))
    dat$value[dat$value < 0.1] <- 0.1 # If below the min, set to a non-zero value
    ic_dist$df <- dat
    
    df <- data.frame(x = lake_data$df$chla[lake_data$df$Date == start_date],
                     label = "Observed")
    
    dens <- density(ic_dist$df$value)
    df <- data.frame(x = dens$x, y = dens$y)
    probs <- c(0, 0.025, 0.125, 0.875, 0.975, 1)
    quantiles <- quantile(ic_dist$df$value, prob = probs)
    df$quant <- findInterval(df$x, quantiles)
    df$quant[df$quant == 4] <- 2
    df$quant[df$quant == 5] <- 1
    df$quant[df$quant == 6] <- 0
    df$quant <- factor(df$quant)
    
    xlims <- c(input$ic_val - (3 * input$ic_uc), input$ic_val + (3 * input$ic_uc))
    ylims <- c(0, max(df$y) + 1)
    
    ic_plot$plot2 <- ggplot(df, aes(x,y)) + 
      geom_ribbon(aes(ymin = 0, ymax = y, fill = quant)) + 
      geom_vline(xintercept = input$ic_val, linetype = "dashed") + 
      scale_fill_brewer(guide = "none", palette = "OrRd") +
      xlab("Chlorophyll-a (μg/L)") +
      ylab("Density") +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      theme_bw(base_size = 18)
  })
  
  
  
  #** Plot - IC distribution ----
  output$ic_uc_plot2 <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(ic_plot$plot2), "Click 'Generate distribution'")
    )
    ic_plot$plot2
  })
  
  # Reset plot
  observeEvent(input$ic_uc, {
    ic_plot$plot2 <- NULL
  })
  
  
  #** Recent obs timeseries ----
  # output$ic_obs_plot <- renderPlotly({
  #   validate(
  #     need(input$table01_rows_selected != "",
  #          message = "Please select a site in Objective 1.")
  #   )
  #   validate(
  #     need(!is.null(lm_wt$m) & !is.null(lm_upar$m),
  #          message = "Please prepare inputs in Objective 5.")
  #   )
  # 
  # 
  #   p <- ggplot()
  # 
  #   if(!is.null(ic_dist$df)) {
  #     quants <- quantile(ic_dist$df$value, c(0.25, 0.75))
  # 
  #     err_bar <- data.frame(x = as.Date(start_date), ymin = quants[1], ymax = quants[2])
  #     p <- p +
  #       geom_errorbar(data = err_bar, aes(x, ymin = ymin, ymax = ymax, width = 0.5))
  #   }
  # 
  #   p <- p +
  #     geom_point(data = obs_plot$hist, aes(Date, chla, color = "Chlorophyll-a")) +
  #     geom_vline(xintercept = as.Date(start_date), linetype = "dashed") +
  #     ylab("Chlorophyll-a (μg/L)") +
  #     xlab("Date") +
  #     scale_color_manual(values = c("Chlorophyll-a" = cols[2])) +
  #     theme_bw(base_size = 12) +
  #     theme(legend.position = "none")
  #   return(ggplotly(p, dynamicTicks = TRUE))
  # })
  
  #** Run IC forecast ----
  est_out1 <- reactiveValues(out = NULL)
  observeEvent(input$n_mem1, {
    est_out1$out <- NULL
  })
  observeEvent(input$run_fc1, {
    
    req(input$table01_rows_selected != "")
    req(!is.null(lm_wt$m) & !is.null(lm_upar$m))
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with ", input$n_mem1, " members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 36,
                                         freq_din = 36,
                                         lake_data = lake_data$df,
                                         start_date = start_date)
    driver_file <- convert_forecast(lm_wt = lm_wt, lm_upar = lm_upar, noaa_fc = noaa_fc, start_date = start_date)
    
    #get initial conditions for forecast
    yini <- get_yini(lake_data = lake_data$df,
                     start_date = start_date)
    yini[1] <- input$ic_val
    
    chla_cv <- ((input$ic_uc / input$ic_val))

    progress$set(value = 0.3)
    est_out <- EnKF(n_en = input$n_mem1, 
                   start = '2020-09-25', # start date 
                   stop = '2020-10-29', # stop date
                   time_step = 'days',
                   obs_file = obs_file,
                   driver_file = driver_file,
                   n_states_est = 2, 
                   n_params_est = 1,
                   n_params_obs = 0,
                   maxUptake_init = 0.12, 
                   obs_cv = c(0.01,0.05),# cv for chl-a and DIN, respectively
                   param_cv = 0.1, # for maxUptake
                   init_cond_cv = c(chla_cv, 0.1),#cv for chl-a and DIN, respectively
                   state_names = c("chla", "nitrate"),
                   yini = yini)
    
    progress$set(value = 0.9)
    out <- format_enkf_output(est_out = est_out, lake_data = lake_data$df)
    out$chla$obs$obs[1] <- NA
    
    est_out1$out <- out
    progress$set(value = 1)
  })
  
  #** Plot - FC1 - chla UC ----
  output$chla_fc1 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$r2) & !is.null(lm_upar$r2),
           message = "Please complete Objective 5.")
    )
    validate(
      need(!is.null(est_out1$out),
           message = "Click 'Run forecast'.")
    )
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type1, est_out = est_out1$out, var = "chla",
                  n_days = input$run_fc1_nday)
  })
  
  #** Plot - FC1 - nitrate UC ----
  output$nitrate_fc1 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(any(!is.null(est_out1$out)),
           message = "Click 'Run forecast'.")
    )
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type1, est_out = est_out1$out, var = "nitrate",
                  n_days = input$run_fc1_nday)
  })
  
  #** Catch for distribution plot ----
  observeEvent(input$plot_type1, {
    if(input$plot_type1 == "Distribution") {
      updateSliderInput(session, inputId = "run_fc1_nday", value = 35)
      shinyjs::disable("run_fc1_nday")
    } else {
      shinyjs::enable("nday_no_da")
    }
  })

  # Objective 7 ----
  #** Submit Hypothesis Rank ----
  observeEvent(input$submit_hyp, {

    shinyalert::shinyalert(title = "Hypothesis Submitted!",
                           "Now continue below and generate the forecasts and we will see how it matches with your hypothesis.")
  })

  #* Run Forecast with NO DA ----
  est_out_no_da <- reactiveValues(out = NULL)
  observeEvent(input$run_fc_no_da, {
    
    req(input$table01_rows_selected != "")
    req(!is.null(lm_wt$m) & !is.null(lm_upar$m))
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with ", input$n_mem_no_da, " members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 36,
                                         freq_din = 36,
                                         lake_data = lake_data$df,
                                         start_date = start_date)
    driver_file <- convert_forecast(lm_wt = lm_wt, lm_upar = lm_upar, noaa_fc = noaa_fc, start_date = start_date)
    
    #get initial conditions for forecast
    yini <- get_yini(lake_data = lake_data$df,
                     start_date = start_date)
    
    progress$set(value = 0.3)
    est_out <- EnKF(n_en = input$n_mem_no_da, 
                    start = '2020-09-25', # start date 
                    stop = '2020-10-29', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(0.01,0.05), #cv for chl-a and DIN, respectively
                    param_cv = 0.1, #for maxUptake
                    init_cond_cv = c(0.05, 0.1), #cv for chl-a and DIN, respectively
                    state_names = c("chla", "nitrate"),
                    yini = yini)
    
    progress$set(value = 0.9)
    est_out_no_da$out <- format_enkf_output(est_out = est_out, lake_data = lake_data$df)
    progress$set(value = 1)
  })
  
  #** Reset forecast when slider is moved
  observeEvent(input$n_mem_no_da, {
    est_out_no_da$out <- NULL
  })
  
  #** Assess forecast with NO DA ----
  fc_no_da <- reactiveValues(plot = NULL, rmse = NA)
  observeEvent(input$assess_fc_no_da, {
    req(input$table01_rows_selected != "")
    req(!is.null(est_out_no_da$out))
    
    fc_no_da$rmse <- rmse(est_out = est_out_no_da$out, lake_data = lake_data$df, var = "chla")
    fc_no_da$plot <- pred_v_obs(obs_plot = obs_plot, est_out = est_out_no_da$out, var = "chla")
  })
  
  #** Plot Forecast with NO DA ----
  output$chla_fc_no_da <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$r2) & !is.null(lm_upar$r2),
           message = "Please complete Objective 5.")
    )
    validate(
      need(!is.null(est_out_no_da$out),
           message = "Click 'Run forecast'.")
    )
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type_no_da,
                  est_out = est_out_no_da$out, var = "chla", add_obs = input$add_obs_no_da, n_days = input$nday_no_da)
  })
  
  #** Catch for distribution plot ----
  observeEvent(input$plot_type_no_da, {
    if(input$plot_type_no_da == "Distribution") {
      updateSliderInput(session, inputId = "nday_no_da", value = 35)
      shinyjs::disable("nday_no_da")
    } else {
      shinyjs::enable("nday_no_da")
    }
  })
  
  #** Calculate RMSE with NO DA ----
  output$rmse_no_da <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(est_out_no_da$out),
           message = "Click 'Run forecast'.")
    )
    validate(
      need(!is.na(fc_no_da$rmse),
           message = "Click 'Assess forecast'.")
    )
    paste0("Chlorophyll-a RMSE = ", fc_no_da$rmse, " μg/L") 
  })
  
  #** Plot Pred vs. Obs with NO DA ----
  output$chla_fc_assess_no_da <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(est_out_no_da$out),
           message = "Click 'Run forecast'.")
    )
    validate(
      need(!is.null(fc_no_da$plot),
           message = "Click 'Assess forecast'.")
    )
    fc_no_da$plot
  })
  
  #* Run Forecast with chl-a DA ----
  est_out_chla_assim <- reactiveValues(out = NULL)
  observeEvent(input$run_fc_chla_assim, {
    
    req(input$table01_rows_selected != "")
    req(!is.null(lm_wt$m) & !is.null(lm_upar$m))

    # Reset assessment data
    fc_chla_assim$plot <- NULL
    fc_chla_assim$rmse <- NULL
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with ", input$n_mem_chla_assim, " members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 7,
                                         freq_din = 36,
                                         lake_data = lake_data$df,
                                         start_date = start_date)
    driver_file <- convert_forecast(lm_wt = lm_wt, lm_upar = lm_upar, noaa_fc = noaa_fc, start_date = start_date)
    
    #get initial conditions for forecast
    yini <- get_yini(lake_data = lake_data$df,
                     start_date = start_date)
    
    progress$set(value = 0.3)
    est_out <- EnKF(n_en = input$n_mem_chla_assim, 
                    start = '2020-09-25', # start date 
                    stop = '2020-10-29', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(0.01,0.05), #cv for chl-a and DIN, respectively
                    param_cv = 0.1, #for maxUptake
                    init_cond_cv = c(0.05, 0.1), #cv for chl-a and DIN, respectively
                    state_names = c("chla", "nitrate"),
                    yini = yini)
    
    progress$set(value = 0.9)
    est_out_chla_assim$out <- format_enkf_output(est_out = est_out, lake_data = lake_data$df)
    progress$set(value = 1)
  })
  
  #** Reset forecast when slider is moved
  observeEvent(input$ndays_chla_assim, {
    est_out_chla_assim$out <- NULL
    fc_chla_assim$plot <- NULL
    fc_chla_assim$rmse <- NULL
  })
  
  
  #** Assess forecast with chl-a DA ----
  fc_chla_assim <- reactiveValues(plot = NULL, rmse = NA)
  observeEvent(input$assess_fc_chla_assim, {
    req(input$table01_rows_selected != "")
    req(!is.null(est_out_chla_assim$out))
    
    fc_chla_assim$rmse <- rmse(est_out = est_out_chla_assim$out, lake_data = lake_data$df, var = "chla")
    fc_chla_assim$plot <- pred_v_obs(obs_plot = obs_plot, est_out = est_out_chla_assim$out, var = "chla")
  })
  
  #** Plot Forecast with chl-a DA ----
  output$chla_fc_chla_assim <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$r2) & !is.null(lm_upar$r2),
           message = "Please complete Objective 5.")
    )
    validate(
      need(!is.null(est_out_chla_assim$out),
           message = "Click 'Run forecast'.")
    )
    var <- view_vars$sname[view_vars$lname == input$view_var_chla_assim]
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type_chla_assim,
                  est_out = est_out_chla_assim$out, var = var, add_obs = input$add_obs_chla_assim, n_days = input$nday_chla_assim)
  })
  
  #** Catch for distribution plot ----
  observeEvent(input$plot_type_chla_assim, {
    if(input$plot_type_chla_assim == "Distribution") {
      updateSliderInput(session, inputId = "nday_chla_assim", value = 35)
      shinyjs::disable("nday_chla_assim")
    } else {
      shinyjs::enable("nday_chla_assim")
    }
  })
  
  #** Print RMSE with chl-a DA ----
  output$rmse_chla_assim <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(est_out_chla_assim$out),
           message = "Click 'Run forecast'.")
    )
    validate(
      need(!is.na(fc_chla_assim$rmse),
           message = "Click 'Assess forecast'.")
    )
    paste0("Chlorophyll-a RMSE = ", fc_chla_assim$rmse, " μg/L") 
  })
  
  #** Plot Pred vs. Obs with chl-a DA ----
  output$chla_fc_assess_chla_assim <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(est_out_chla_assim$out),
           message = "Click 'Run forecast'.")
    )
    validate(
      need(!is.null(fc_chla_assim$plot),
           message = "Click 'Assess forecast'.")
    )
    fc_chla_assim$plot
  })
  
  #* Run Forecast with nitrate DA ----
  est_out_nitrate_assim <- reactiveValues(out = NULL)
  observeEvent(input$run_fc_nitrate_assim, {
    
    req(input$table01_rows_selected != "")
    req(!is.null(lm_wt$m) & !is.null(lm_upar$m))
    
    # Reset assessment data
    fc_nitrate_assim$plot <- NULL
    fc_nitrate_assim$rmse <- NULL
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with ", input$n_mem_nitrate_assim, " members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 36,
                                         freq_din = 7,
                                         lake_data = lake_data$df,
                                         start_date = start_date)
    driver_file <- convert_forecast(lm_wt = lm_wt, lm_upar = lm_upar, noaa_fc = noaa_fc, start_date = start_date)
    
    #get initial conditions for forecast
    yini <- get_yini(lake_data = lake_data$df,
                     start_date = start_date)
    
    progress$set(value = 0.3)
    est_out <- EnKF(n_en = input$n_mem_nitrate_assim, 
                    start = '2020-09-25', # start date 
                    stop = '2020-10-29', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(0.01,0.05), #cv for chl-a and DIN, respectively
                    param_cv = 0.1, #for maxUptake
                    init_cond_cv = c(0.05, 0.1), #cv for chl-a and DIN, respectively
                    state_names = c("chla", "nitrate"),
                    yini = yini)
    
    progress$set(value = 0.9)
    est_out_nitrate_assim$out <- format_enkf_output(est_out = est_out, lake_data = lake_data$df)
    progress$set(value = 1)
  })
  
  #** Reset forecast when slider is moved
  observeEvent(input$ndays_nitrate_assim, {
    est_out_nitrate_assim$out <- NULL
    fc_nitrate_assim$plot <- NULL
    fc_nitrate_assim$rmse <- NULL
  })
  
  
  #** Assess forecast with nitrate DA ----
  fc_nitrate_assim <- reactiveValues(plot = NULL, rmse = NA)
  observeEvent(input$assess_fc_nitrate_assim, {
    req(input$table01_rows_selected != "")
    req(!is.null(est_out_nitrate_assim$out))
    
    fc_nitrate_assim$rmse <- rmse(est_out = est_out_nitrate_assim$out, lake_data = lake_data$df, var = "chla")
    fc_nitrate_assim$plot <- pred_v_obs(obs_plot = obs_plot, est_out = est_out_nitrate_assim$out, var = "chla")
  })
  
  #** Plot Forecast with nitrate DA ----
  output$chla_fc_nitrate_assim <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$r2) & !is.null(lm_upar$r2),
           message = "Please complete Objective 5.")
    )
    validate(
      need(!is.null(est_out_nitrate_assim$out),
           message = "Click 'Run forecast'.")
    )
    var <- view_vars$sname[view_vars$lname == input$view_var_nitrate_assim]
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type_nitrate_assim,
                  est_out = est_out_nitrate_assim$out, var = var, add_obs = input$add_obs_nitrate_assim, n_days = input$nday_nitrate_assim)
  })
  
  #** Catch for distribution plot ----
  observeEvent(input$plot_type_nitrate_assim, {
    if(input$plot_type_nitrate_assim == "Distribution") {
      updateSliderInput(session, inputId = "nday_nitrate_assim", value = 35)
      shinyjs::disable("nday_nitrate_assim")
    } else {
      shinyjs::enable("nday_nitrate_assim")
    }
  })
  
  #** Calculate RMSE with nitrate DA ----
  output$rmse_nitrate_assim <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(est_out_nitrate_assim$out),
           message = "Click 'Run forecast'.")
    )
    validate(
      need(!is.na(fc_nitrate_assim$rmse),
           message = "Click 'Assess forecast'.")
    )
    paste0("Chlorophyll-a RMSE = ", fc_nitrate_assim$rmse, " μg/L") 
  })
  
  #** Plot Pred vs. Obs with nitrate DA ----
  output$chla_fc_assess_nitrate_assim <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(est_out_nitrate_assim$out),
           message = "Click 'Run forecast'.")
    )
    validate(
      need(!is.null(fc_nitrate_assim$plot),
           message = "Click 'Assess forecast'.")
    )
    fc_nitrate_assim$plot
  })
  
  #* Run Forecast with BOTH DA ----
  est_out_both_assim <- reactiveValues(out = NULL)
  observeEvent(input$run_fc_both_assim, {
    
    req(input$table01_rows_selected != "")
    req(!is.null(lm_wt$m) & !is.null(lm_upar$m))
    
    # Reset assessment data
    fc_both_assim$plot <- NULL
    fc_both_assim$rmse <- NULL
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with ", input$n_mem_both_assim, " members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 7,
                                         freq_din = 7,
                                         lake_data = lake_data$df,
                                         start_date = start_date)
    driver_file <- convert_forecast(lm_wt = lm_wt, lm_upar = lm_upar, noaa_fc = noaa_fc, start_date = start_date)
    
    #get initial conditions for forecast
    yini <- get_yini(lake_data = lake_data$df,
                     start_date = start_date)
    
    progress$set(value = 0.3)
    est_out <- EnKF(n_en = input$n_mem_both_assim, 
                    start = '2020-09-25', # start date 
                    stop = '2020-10-29', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(0.01, 0.05), #cv for chl-a and DIN, respectively
                    param_cv = 0.1, #for maxUptake
                    init_cond_cv = c(0.05, 0.1), #cv for chl-a and DIN, respectively
                    state_names = c("chla", "nitrate"),
                    yini = yini)
    
    progress$set(value = 0.9)
    est_out_both_assim$out <- format_enkf_output(est_out = est_out, lake_data = lake_data$df)
    progress$set(value = 1)
  })
  
  #** Reset forecast when slider is moved
  observeEvent(input$ndays_both_assim, {
    est_out_both_assim$out <- NULL
    fc_both_assim$plot <- NULL
    fc_both_assim$rmse <- NULL
  })
  
  
  #** Assess forecast with both DA ----
  fc_both_assim <- reactiveValues(plot = NULL, rmse = NA)
  observeEvent(input$assess_fc_both_assim, {
    req(input$table01_rows_selected != "")
    req(!is.null(est_out_both_assim$out))
    
    fc_both_assim$rmse <- rmse(est_out = est_out_both_assim$out, lake_data = lake_data$df, var = "chla")
    fc_both_assim$plot <- pred_v_obs(obs_plot = obs_plot, est_out = est_out_both_assim$out, var = "chla")
  })
  
  #** Plot Forecast with both DA ----
  output$chla_fc_both_assim <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$r2) & !is.null(lm_upar$r2),
           message = "Please complete Objective 5.")
    )
    validate(
      need(!is.null(est_out_both_assim$out),
           message = "Click 'Run forecast'.")
    )
    var <- view_vars$sname[view_vars$lname == input$view_var_both_assim]
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type_both_assim,
                  est_out = est_out_both_assim$out, var = var, add_obs = input$add_obs_both_assim, n_days = input$nday_both_assim)
  })
  
  #** Catch for distribution plot ----
  observeEvent(input$plot_type_both_assim, {
    if(input$plot_type_both_assim == "Distribution") {
      updateSliderInput(session, inputId = "nday_both_assim", value = 35)
      shinyjs::disable("nday_both_assim")
    } else {
      shinyjs::enable("nday_both_assim")
    }
  })
  
  #** Calculate RMSE with both DA ----
  output$rmse_both_assim <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.na(est_out_both_assim$out),
           message = "Click 'Run forecast'.")
    )
    validate(
      need(!is.null(fc_both_assim$rmse),
           message = "Click 'Assess forecast'.")
    )
    paste0("Chlorophyll-a RMSE = ", fc_both_assim$rmse, " μg/L") 

  })
  
  #** Plot Pred vs. Obs with both DA ----
  output$chla_fc_assess_both_assim <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(est_out_both_assim$out),
           message = "Click 'Run forecast'.")
    )
    validate(
      need(!is.null(fc_both_assim$plot),
           message = "Click 'Assess forecast'.")
    )
    fc_both_assim$plot
  })
  
  #** Compare all forecasts ----
  da_method <- reactiveValues(dt = data.frame(RMSE = rep(NA, 2)), plot = NULL)
  
  observeEvent(input$compare_da, {
    
    req(input$table01_rows_selected != "")
    
    if(input$run_fc_no_da == 0){
      validate(
             shinyalert::shinyalert(title = "Click 'Run forecast' for the forecast with no data assimilation.")
      )
    }
    if(input$assess_fc_no_da == 0){
      validate(
        shinyalert::shinyalert(title = "Click 'Assess forecast' for the forecast with no data assimilation.")
      )
    }
    if(input$run_fc_chla_assim == 0){
      validate(
        shinyalert::shinyalert(title = "Click 'Run forecast' for the forecast with data assimilation.")
      )
    }
    if(input$assess_fc_chla_assim == 0){
      validate(
        shinyalert::shinyalert(title = "Click 'Assess forecast' for the forecast with data assimilation.")
      )
    }
  
    var <- "chla"
    da_method$plot <- plot_four_forecasts(no_da = est_out_no_da$out, chla = est_out_chla_assim$out,
                                          #nitrate = est_out_nitrate_assim$out, both = est_out_both_assim$out, 
                                          var = var, obs_plot = obs_plot, add_obs = TRUE)
    
    da_method$dt$RMSE[1] <- fc_no_da$rmse
    da_method$dt$RMSE[2] <- fc_chla_assim$rmse
    # da_method$dt$RMSE[3] <- fc_nitrate_assim$rmse
    # da_method$dt$RMSE[4] <- fc_both_assim$rmse
  })
  
  #** Table of RMSE ----
  output$da_method_tab <- renderDT(da_method$dt, selection = "none",
                           options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                          columnDefs = list(list(width = '100%', targets = "_all")), scrollX = TRUE
                           ), colnames = c("RMSE"),
                           rownames = c("No DA", "Chl-a"),
                           server = FALSE, escape = FALSE)
  
  #** Plot of all DA methods ----
  output$da_method_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$r2) & !is.null(lm_upar$r2),
           message = "Please complete Objective 5.")
    )
    validate(
      need(!is.null(da_method$plot),
           message = "Click 'Compare DA methods'.")
    )
    da_method$plot
  })
  
  #** Plot of all assessment plots ----
  output$all_assess_plot <- renderPlot({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(est_out_both_assim$out),
           message = "Click 'Run forecast'.")
    )
    validate(
      need(!is.null(fc_both_assim$plot),
           message = "Click 'Assess forecast'.")
    )
    fc_both_assim$plot
    ggpubr::ggarrange(plotlist = list(fc_no_da$plot, fc_chla_assim$plot, fc_nitrate_assim$plot, fc_both_assim$plot), nrow = 2)
  })
  
  hyp1 <- reactiveValues(dt = NULL)
  observe({
    hyp1$dt <- data.frame(Hypothesis = input$rank_hyp_2)

  })

  output$hyp1 <- renderDT({
    validate(
      need(length(input$rank_hyp_4) == 4,
           "Add rankings across!")
    )
    hyp1$dt
    }, selection = "none",
                          options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                         columnDefs = list(list(width = '20%', targets = "_all")), scrollX = TRUE
                          ), colnames = c("Hypothesis"),
                          server = FALSE, escape = FALSE)

  # Objective 8 - Explore observation uncertainty ----
  #** Slickr Chla slides ----
  output$chla_slides <- renderSlickR({
    slickR(chla_slides) + settings(dots = TRUE)
  })
  
  #** Slickr Nitrate slides ----
  output$nitrate_slides <- renderSlickR({
    slickR(nitrate_slides) + settings(dots = TRUE)
  })
  
  #* Run forecast w/ Observation Uncertainty ----
  obs_uc <- reactiveValues(out = NULL, rmse = NA)
  observeEvent(input$run_fc_obs_uc, {
    
    req(input$table01_rows_selected != "")
    req(!is.null(lm_wt$m) & !is.null(lm_upar$m))

    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with 100 members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    freq_chla <- 7#ifelse("Chlorophyll-a" %in% input$obs_uc_da, 7, 36) 
    freq_din <- 36#ifelse("Nitrate" %in% input$obs_uc_da, 7, 36) 
    
    obs_file <- create_data_assim_inputs(freq_chla = freq_chla,
                                         freq_din = freq_din,
                                         lake_data = lake_data$df,
                                         start_date = start_date)
    driver_file <- convert_forecast(lm_wt = lm_wt, lm_upar = lm_upar, noaa_fc = noaa_fc, start_date = start_date)
    
    #get initial conditions for forecast
    yini <- get_yini(lake_data = lake_data$df,
                     start_date = start_date)
    
    chla_cv <-  ((input$obs_uc_chla / mean(obs_plot$hist$chla, na.rm = TRUE)))
    nitrate_cv <- 0.25#((input$obs_uc_nitrate / 2)) # mean(obs_plot$hist$nitrate, na.rm = TRUE)))
    
    progress$set(value = 0.3)
    est_out <- EnKF(n_en = 100, 
                    start = '2020-09-25', # start date 
                    stop = '2020-10-29', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(chla_cv, nitrate_cv), #cv for chl-a and DIN, respectively
                    param_cv = 0.1, #for maxUptake
                    init_cond_cv = c(0.05, 0.1), #cv for chl-a and DIN, respectively
                    state_names = c("chla", "nitrate"),
                    yini = yini)
    
    progress$set(value = 0.9)
    obs_uc$out <- format_enkf_output(est_out = est_out, lake_data = lake_data$df)
    obs_uc$rmse <- rmse(est_out = obs_uc$out, lake_data = lake_data$df, var = "chla")
    progress$set(value = 1)
  })
  
  #** Plot Forecast with Observational Uncertainty ----
  output$fc_obs_uc_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$r2) & !is.null(lm_upar$r2),
           message = "Please complete Objective 5.")
    )
    validate(
      need(!is.null(obs_uc$out),
           message = "Click 'Run forecast'.")
    )
    var <- "chla" #view_vars$sname[view_vars$lname == input$view_var_both_assim]
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type_obs_uc,
                  est_out = obs_uc$out, var = var, add_obs = input$add_obs_obs_uc, n_days = input$nday_obs_uc)
  })
  
  #** Print RMSE with Observational Uncertainty ----
  output$rmse_obs_uc <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(obs_uc$out),
           message = "Click 'Run forecast'.")
    )
    paste0("Chlorophyll-a RMSE = ", obs_uc$rmse, " μg/L") 
  })
  
  #** Table with RMSE & DA freq ---
  obs_uc_rmse <- reactiveValues(dt = obs_uc_df)
  output$obs_uc_rmse <- renderDT(obs_uc_rmse$dt, selection = "single",
                                  options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                                 columnDefs = list(list(width = '10%', targets = "_all"))
                                  ), colnames = c("Chl-a obs uncertainty", "RMSE"),
                                  server = FALSE, escape = FALSE)
  
  #** Save RMSE & Frequencies in a table ----
  observeEvent(input$save_rmse, {
    
    req(!is.na(obs_uc$rmse))
    
    obs_uc_chla <- input$obs_uc_chla #ifelse("Chlorophyll-a" %in% input$da_freq_da, input$da_freq_chla, 0) 
    #freq_din <- 0 #ifelse("Nitrate" %in% input$da_freq_da, input$da_freq_nitrate, 0) 
    
    idx <- input$obs_uc_rmse_rows_selected
    if(is.null(idx)) {
      idx <- which(is.na(obs_uc_rmse$dt[, 1]))[1]
    }
    obs_uc_rmse$dt$chla[idx] <- obs_uc_chla
    #da_freq_rmse$dt$nitrate[idx] <- freq_din
    obs_uc_rmse$dt$rmse[idx] <- obs_uc$rmse
  })
  
  #** Reset forecast when slider is moved
  observeEvent(input$obs_uc_chla, {
    obs_uc$out <- NULL
  })
  
  #** Disable Save button when no RMSE available ----
  observe({
    if(is.na(obs_uc$rmse)) {
      shinyjs::disable("save_rmse")
    } else {
      shinyjs::enable("save_rmse")
      
    }
  })
  
  
  
  #** Catch for distribution plot ----
  observeEvent(input$plot_type_obs_uc, {
    if(input$plot_type_obs_uc == "Distribution") {
      updateSliderInput(session, inputId = "nday_obs_uc", value = 35)
      shinyjs::disable("nday_obs_uc")
    } else {
      shinyjs::enable("nday_obs_uc")
    }
  })
  
  # Objective 9 ----
  #** Slickr Chla slides ----
  output$chla_slides2 <- renderSlickR({
    slickR(chla_slides) + settings(dots = TRUE)
  })
  
  #** Slickr Nitrate slides ----
  output$nitrate_slides2 <- renderSlickR({
    slickR(nitrate_slides) + settings(dots = TRUE)
  })
  
  #* Run forecast w/ different DA Frequencies ----
  da_freq <- reactiveValues(out = NULL, rmse = NA)
  observeEvent(input$run_fc_da_freq, {
    
    req(input$table01_rows_selected != "")
    req(!is.null(lm_wt$m) & !is.null(lm_upar$m))
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with 100 members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    freq_chla <- input$da_freq_chla #ifelse("Chlorophyll-a" %in% input$da_freq_da, input$da_freq_chla, 36) 
    freq_din <- 36 #ifelse("Nitrate" %in% input$da_freq_da, input$da_freq_nitrate, 36) 
    
    obs_file <- create_data_assim_inputs(freq_chla = freq_chla,
                                         freq_din = freq_din,
                                         lake_data = lake_data$df,
                                         start_date = start_date)
    driver_file <- convert_forecast(lm_wt = lm_wt, lm_upar = lm_upar, noaa_fc = noaa_fc, start_date = start_date)
    
    #get initial conditions for forecast
    yini <- get_yini(lake_data = lake_data$df,
                     start_date = start_date)
    
    # Carried over from the previous exercise?
    # chla_cv <- ((input$obs_uc_chla / mean(obs_plot$hist$chla, na.rm = TRUE)))
    chla_cv <- 0.05
    
    progress$set(value = 0.3)
    est_out <- EnKF(n_en = 100, 
                    start = '2020-09-25', # start date 
                    stop = '2020-10-29', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(chla_cv, 0.05), #cv for chl-a and DIN, respectively
                    param_cv = 0.1, #for maxUptake
                    init_cond_cv = c(0.05, 0.1), #cv for chl-a and DIN, respectively
                    state_names = c("chla", "nitrate"),
                    yini = yini)
    
    progress$set(value = 0.9)
    da_freq$out <- format_enkf_output(est_out = est_out, lake_data = lake_data$df)
    da_freq$rmse <- rmse(est_out = da_freq$out, lake_data = lake_data$df, var = "chla")
    progress$set(value = 1)
  })
  
  #** Plot Forecast with DA Frequencies ----
  output$fc_da_freq_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$r2) & !is.null(lm_upar$r2),
           message = "Please complete Objective 5.")
    )
    validate(
      need(!is.null(da_freq$out),
           message = "Click 'Run forecast'.")
    )
    var <- "chla" #view_vars$sname[view_vars$lname == input$view_var_both_assim]
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type_da_freq,
                  est_out = da_freq$out, var = var, add_obs = input$add_obs_da_freq, n_days = input$nday_da_freq)
  })
  
  #** Print RMSE with DA frequencies ----
  output$rmse_da_freq <- renderText({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(da_freq$out),
           message = "Click 'Run forecast'.")
    )
    paste0("Chlorophyll-a RMSE = ", da_freq$rmse, " μg/L") 
  })
  
  #** Table with RMSE & DA freq ---
  da_freq_rmse <- reactiveValues(dt = da_freq_df)
  output$da_freq_rmse <- renderDT(da_freq_rmse$dt, selection = "single",
                              options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                                             columnDefs = list(list(width = '10%', targets = "_all"))
                              ), colnames = c("Chl-a freq", "RMSE"),
                              server = FALSE, escape = FALSE)
  
  #** Save RMSE & Frequencies in a table ----
  observeEvent(input$save_rmse2, {
    
    req(!is.na(da_freq$rmse))
    
    freq_chla <- input$da_freq_chla #ifelse("Chlorophyll-a" %in% input$da_freq_da, input$da_freq_chla, 0) 
    freq_din <- 0 #ifelse("Nitrate" %in% input$da_freq_da, input$da_freq_nitrate, 0) 
    
    idx <- input$da_freq_rmse_rows_selected
    if(is.null(idx)) {
      idx <- which(is.na(da_freq_rmse$dt[, 1]))[1]
    }
    da_freq_rmse$dt$chla[idx] <- freq_chla
    #da_freq_rmse$dt$nitrate[idx] <- freq_din
    da_freq_rmse$dt$rmse[idx] <- da_freq$rmse
  })
  
  #** Reset forecast when slider is moved
  observeEvent(input$da_freq_chla, {
    da_freq$out <- NULL
  })
  
  #** Disable Save button when no RMSE available ----
  observe({
    if(is.na(da_freq$rmse)) {
      shinyjs::disable("save_rmse2")
    } else {
      shinyjs::enable("save_rmse2")
      
    }
  })
  
  #** Catch for distribution plot ----
  observeEvent(input$plot_type_da_freq, {
    if(input$plot_type_da_freq == "Distribution") {
      updateSliderInput(session, inputId = "nday_da_freq", value = 35)
      shinyjs::disable("nday_da_freq")
    } else {
      shinyjs::enable("nday_da_freq")
    }
  })
  
  #* Activity C ----
  #** Objective 10 -Management Scenario ----
  
  #** Slickr Chla slides ----
  output$chla_slides3 <- renderSlickR({
    slickR(chla_slides) + settings(dots = TRUE)
  })
  
  #** Slickr Nitrate slides ----
  output$nitrate_slides3<- renderSlickR({
    slickR(nitrate_slides) + settings(dots = TRUE)
  })
  
  #*** Budget plot
  budget_exp <- reactiveValues(df = budget_options)

  #** Sensors ----
  observeEvent(input$chla_sens, {
    if(input$chla_sens) {
      budget_exp$df$value[1] <- budget_options$cost[1]
    } else {
      budget_exp$df$value[1] <- 0
    }
  })
  observeEvent(input$nitrate_sens, {
    if(input$nitrate_sens) {
      budget_exp$df$value[2] <- budget_options$cost[2]
    } else {
      budget_exp$df$value[2] <- 0
    }
  })
  
  observeEvent(input$data_stream, {
    if(input$data_stream) {
      budget_exp$df$value[3] <- budget_options$cost[3]
    } else {
      budget_exp$df$value[3] <- 0
    }
  })
  
  #** Field Equipment ----
  observeEvent(input$buoy, {
    if(input$buoy) {
      budget_exp$df$value[4] <- budget_options$cost[4]
    } else {
      budget_exp$df$value[4] <- 0
    }
  })
  observeEvent(input$res_access, {
    if(input$res_access) {
      budget_exp$df$value[5] <- budget_options$cost[5]
    } else {
      budget_exp$df$value[5] <- 0
    }
  })
  observeEvent(input$man_samp, {
    if(input$man_samp) {
      budget_exp$df$value[6] <- budget_options$cost[6]
    } else {
      budget_exp$df$value[6] <- 0
    }
  })
  observeEvent(input$sens_main, {
    if(input$sens_main) {
      budget_exp$df$value[7] <- budget_options$cost[7]
    } else {
      budget_exp$df$value[7] <- 0
    }
  })
  
  #** Field Personnel ----
  observeEvent(input$deploy_sens, {
    if(input$deploy_sens) {
      budget_exp$df$value[8] <- budget_options$cost[8] * as.numeric(input$ndeploy_sens)
    } else {
      budget_exp$df$value[8] <- 0
    }
  })
  observeEvent(input$ndeploy_sens, {
    if(input$deploy_sens) {
      budget_exp$df$value[8] <- budget_options$cost[8] * as.numeric(input$ndeploy_sens)
    } else {
      budget_exp$df$value[8] <- 0
    }
  })
  
  observeEvent(input$chla_samp, {
    if(input$chla_samp) {
      budget_exp$df$value[9] <- budget_options$cost[9] * as.numeric(input$nchla_samp) * 52
    } else {
      budget_exp$df$value[9] <- 0
    }
  })
  observeEvent(input$nchla_samp, {
    if(input$chla_samp) {
      budget_exp$df$value[9] <- budget_options$cost[9] * as.numeric(input$nchla_samp) * 52
    } else {
      budget_exp$df$value[9] <- 0
    }
  })
  
  observeEvent(input$nitrate_samp, {
    if(input$nitrate_samp) {
      budget_exp$df$value[10] <- budget_options$cost[10] * as.numeric(input$nnitrate_samp) * 52
    } else {
      budget_exp$df$value[10] <- 0
    }
  })
  observeEvent(input$nnitrate_samp, {
    if(input$nitrate_samp) {
      budget_exp$df$value[10] <- budget_options$cost[10] * as.numeric(input$nnitrate_samp) * 52
    } else {
      budget_exp$df$value[10] <- 0
    }
  })

  #** Laboratory Analysis ----
  observeEvent(input$analyze_chla, {
    if(input$analyze_chla) {
      budget_exp$df$value[11] <- budget_options$cost[11] * as.numeric(input$nanalyze_chla) * 52
    } else {
      budget_exp$df$value[11] <- 0
    }
  })
  observeEvent(input$nanalyze_chla, {
    if(input$analyze_chla) {
      budget_exp$df$value[11] <- budget_options$cost[11] * as.numeric(input$nanalyze_chla) * 52
    } else {
      budget_exp$df$value[11] <- 0
    }
  })
  
  observeEvent(input$analyze_nitrate, {
    if(input$analyze_nitrate) {
      budget_exp$df$value[12] <- budget_options$cost[12] * as.numeric(input$nanalyze_nitrate) * 52
    } else {
      budget_exp$df$value[12] <- 0
    }
  })
  observeEvent(input$nanalyze_nitrate, {
    if(input$analyze_nitrate) {
      budget_exp$df$value[12] <- budget_options$cost[12] * as.numeric(input$nanalyze_nitrate) * 52
    } else {
      budget_exp$df$value[12] <- 0
    }
  })
  
  #* Budget Plot ----
  output$budget_plot <- renderPlot({
    
    budget_exp$df$value[nrow(budget_exp$df)] <- budget_exp$df$cost[nrow(budget_exp$df)] - sum(budget_exp$df$value[1:(nrow(budget_exp$df) - 1)])


    validate(
      need(budget_exp$df$value[nrow(budget_exp$df)] >= 0, "It looks like you overspent your budget. You will need to re-draft your budget to make sure you do not overspend.")
    )
    
    
    p <- ggplot() +
      geom_bar(data = budget_exp$df, aes(x, value, fill = category), stat = "identity", position = "stack") +
      ylab("Money ($)") +
      xlab("") +
      coord_cartesian(ylim= c(0, max(budget_options$cost))) +
      theme_bw(base_size = 20)
    return(p)
    # return(ggplotly(p))
  })
  
  
  output$wq_monitoring_tab <- renderDT({
    budget_exp$df[1:(nrow(budget_exp$df) - 1), c(1:3)]
  }, selection = "none",
  options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                 columnDefs = list(list(width = '20%', targets = "_all")), scrollX = FALSE
  ), colnames = c("Category", "Item", "Description"), rownames = NULL,
  server = FALSE, escape = FALSE)
  
  output$budget_table <- renderDT({
    budget_exp$df[1:(nrow(budget_exp$df) - 1), c(1, 2, 7)]
  }, selection = "none",
  options = list(searching = FALSE, paging = FALSE, ordering= FALSE, dom = "t", autoWidth = TRUE,
                 columnDefs = list(list(width = '20%', targets = "_all")), scrollX = FALSE
  ), colnames = c("Item", "Cost", "Sub-total"), rownames = NULL,
  server = FALSE, escape = FALSE)
  
  output$total_exp <- renderText({
    paste0("Total expenditure = $", formatC(sum(budget_exp$df$value[1:(nrow(budget_exp$df) - 1)]), big.mark = ",", format = "s")) 
  })
  
  #** Submit Budget ----
  observeEvent(input$submit_budget, {
    shinyjs::disable("chla_sens")
    shinyjs::disable("nitrate_sens")
    shinyjs::disable("data_stream")
    shinyjs::disable("buoy")
    shinyjs::disable("res_access")
    shinyjs::disable("man_samp")
    shinyjs::disable("sens_main")
    shinyjs::disable("deploy_sens")
    shinyjs::disable("ndeploy_sens")
    shinyjs::disable("chla_samp")
    shinyjs::disable("nchla_samp")
    shinyjs::disable("nitrate_samp")
    shinyjs::disable("nnitrate_samp")
    shinyjs::disable("analyze_chla")
    shinyjs::disable("nanalyze_chla")
    shinyjs::disable("analyze_nitrate")
    shinyjs::disable("nanalyze_nitrate")
    shinyjs::disable("submit_budget")
    shinyalert::shinyalert(title = "Budget Submitted!",
                           "Great job! Now answer the questions below with regards to why you made the decisions you did.")
  })
  
  # Activity C : New version ----
  #* 1 ---- Expensive sensor
  actc1 <- reactiveValues(out = NULL, rmse = NA)
  actc2 <- reactiveValues(out = NULL, rmse = NA)
  actc3 <- reactiveValues(out = NULL, rmse = NA)
  #gap_df <- reactiveValues(dat = NULL)
  
  # Reset values if data collection method is changed
  observeEvent(input$data_collec1, {
    actc1$out <- NULL
    actc1$rmse <- NA
    actc2$out <- NULL
    actc2$rmse <- NA
    actc3$out <- NULL
    actc3$rmse <- NA
    #gap_df$dat <- obs_plot_c
  })
  
  observeEvent(input$run_fc_dec1a, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with 100 members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 10,
                                         freq_din = 36,
                                         lake_data = actc_lake_data,
                                         start_date = actc_start_date)
    
    # #manually introduce semi-random gap representing sensor malfunction
    # start_gap <- sample(5:25,1)
    # gap_length <- sample(1:5,1)
    # gap <- seq(start_gap, start_gap+gap_length, 1)
    # gap_dates <- obs_file$datetime[gap]
    # obs_file[obs_file$datetime %in% gap_dates, "chla"] <- NA
    # gap_df$dat$future$chla[as.Date(gap_df$dat$future$datetime) %in% gap_dates] <- NA

    n_en = 100 # how many ensemble members 
    
    #run the forecast!
    
    est_out <- EnKF(n_en = n_en, 
                   start = '2020-10-02', # start date 
                   stop = '2020-10-31', # stop date
                   time_step = 'days',
                   obs_file = obs_file,
                   driver_file = actc_driver_file,
                   n_states_est = 2, 
                   n_params_est = 1,
                   n_params_obs = 0,
                   maxUptake_init = 0.12, 
                   obs_cv = c(0.05,0.05),#cv for chl-a and DIN, respectively
                   param_cv = 0.1,#for maxUptake
                   init_cond_cv = c(0.05,0.1),#cv for chl-a and DIN, respectively
                   state_names = c("chla","nitrate"),
                   yini = actc_yini)

    progress$set(value = 0.9)
    actc1$out <- format_enkf_output(est_out = est_out, lake_data = actc_lake_data)
    actc1$rmse <- rmse(est_out = est_out, lake_data = actc_lake_data, var = "chla")
    progress$set(value = 1)
    
  })
  observeEvent(input$run_fc_dec1b, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with 100 members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 10,
                                         freq_din = 36,
                                         lake_data = actc_lake_data,
                                         start_date = actc_start_date)
    
    n_en = 100 # how many ensemble members 
    
    #run the forecast!
    
    est_out <- EnKF(n_en = n_en, 
                    start = '2020-10-02', # start date 
                    stop = '2020-10-31', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = actc_driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(0.05,0.05),#cv for chl-a and DIN, respectively
                    param_cv = 0.1,#for maxUptake
                    init_cond_cv = c(0.05,0.1),#cv for chl-a and DIN, respectively
                    state_names = c("chla","nitrate"),
                    yini = actc_yini)
    
    progress$set(value = 0.9)
    actc1$out <- format_enkf_output(est_out = est_out, lake_data = actc_lake_data)
    actc1$rmse <- rmse(est_out = est_out, lake_data = actc_lake_data, var = "chla")
    progress$set(value = 1)
    
  })
  
  
  output$fc_dec1a <- renderPlotly({
    
    validate(
      need(!is.null(actc1$out),
           message = "Click 'Run forecast'.")
    )

    var <- "chla" #view_vars$sname[view_vars$lname == input$view_var_both_assim]
    plot_enkf_out_actc(obs_plot = obs_plot_c, start_date = "2020-10-22", 
                  est_out = actc1$out, var = var, 
                  add_obs = input$add_obs_actc1a, 
                  n_days = 36, h_line = 25, show_assim = FALSE)
    
  })
  output$fc_dec1b <- renderPlotly({
    
    validate(
      need(!is.null(actc1$out),
           message = "Click 'Run forecast'.")
    )
    var <- "chla" #view_vars$sname[view_vars$lname == input$view_var_both_assim]
    plot_enkf_out_actc(obs_plot = obs_plot_c, start_date = "2020-10-22", 
                  est_out = actc1$out, var = var, 
                  add_obs = input$add_obs_actc1b, 
                  n_days = 36, h_line = 25, show_assim = FALSE)
    
  })
  
  #* 2 - Cheap sensor ----
  observeEvent(input$run_fc_dec2a, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with 100 members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 10,
                                         freq_din = 36,
                                         lake_data = actc_lake_data,
                                         start_date = actc_start_date)
    
    n_en = 100 # how many ensemble members 
    
    #randomly draw for observation uncertainty within limits
    obs_cv_chla <- runif(1,0.2,0.4)
    
    #run the forecast!
    
    est_out <- EnKF(n_en = n_en, 
                    start = '2020-10-02', # start date 
                    stop = '2020-10-31', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = actc_driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(0.4, 0.05),#cv for chl-a and DIN, respectively
                    param_cv = 0.1,#for maxUptake
                    init_cond_cv = c(0.4,0.1),#cv for chl-a and DIN, respectively
                    state_names = c("chla","nitrate"),
                    yini = actc_yini)
    
    progress$set(value = 0.9)
    actc2$out <- format_enkf_output(est_out = est_out, lake_data = actc_lake_data)
    actc2$rmse <- rmse(est_out = est_out, lake_data = actc_lake_data, var = "chla")
    progress$set(value = 1)
    
  })
  observeEvent(input$run_fc_dec2b, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with 100 members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 10,
                                         freq_din = 36,
                                         lake_data = actc_lake_data,
                                         start_date = actc_start_date)
    
    # #manually introduce semi-random gap representing sensor malfunction
    # start_gap <- sample(5:25,1)
    # gap_length <- sample(1:5,1)
    # gap <- seq(start_gap, start_gap+gap_length, 1)
    # gap_dates <- obs_file$datetime[gap]
    # obs_file[obs_file$datetime %in% gap_dates, "chla"] <- NA
    # gap_df$dat$future$chla[as.Date(gap_df$dat$future$datetime) %in% gap_dates] <- NA
    # 
    n_en = 100 # how many ensemble members 
    
    #randomly draw for observation uncertainty within limits
    obs_cv_chla <- runif(1,0.2,0.4)
    
    #run the forecast!
    
    est_out <- EnKF(n_en = n_en, 
                    start = '2020-10-02', # start date 
                    stop = '2020-10-31', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = actc_driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(0.4, 0.05),#cv for chl-a and DIN, respectively
                    param_cv = 0.1,#for maxUptake
                    init_cond_cv = c(0.4,0.1),#cv for chl-a and DIN, respectively
                    state_names = c("chla","nitrate"),
                    yini = actc_yini)
    
    progress$set(value = 0.9)
    actc2$out <- format_enkf_output(est_out = est_out, lake_data = actc_lake_data)
    actc2$rmse <- rmse(est_out = est_out, lake_data = actc_lake_data, var = "chla")
    progress$set(value = 1)
    
  })
  
  output$fc_dec2a <- renderPlotly({
    
    validate(
      need(!is.null(actc2$out),
           message = "Click 'Run forecast'.")
    )
    var <- "chla" #view_vars$sname[view_vars$lname == input$view_var_both_assim]
    plot_enkf_out_actc(obs_plot = obs_plot_c, start_date = "2020-10-22", 
                  est_out = actc2$out, var = var, 
                  add_obs = input$add_obs_actc2a, 
                  n_days = 36, h_line = 25, show_assim = FALSE)
    
  })
  output$fc_dec2b <- renderPlotly({
    
    validate(
      need(!is.null(actc2$out),
           message = "Click 'Run forecast'.")
    )
    var <- "chla" #view_vars$sname[view_vars$lname == input$view_var_both_assim]
    plot_enkf_out_actc(obs_plot = obs_plot_c, start_date = "2020-10-22", 
                  est_out = actc2$out, var = var, 
                  add_obs = input$add_obs_actc2b, 
                  n_days = 36, h_line = 25, show_assim = FALSE)
    
  })
  
  #* 3 - No sensor ----
  observeEvent(input$run_fc_dec3a, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with 100 members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 36,
                                         freq_din = 36,
                                         lake_data = actc_lake_data,
                                         start_date = actc_start_date)
    
    n_en = 100 # how many ensemble members 
    
    #randomly draw for observation uncertainty within limits
    obs_cv_chla <- runif(1,0.2,0.4)
    
    #run the forecast!
    
    est_out <- EnKF(n_en = n_en, 
                    start = '2020-10-02', # start date 
                    stop = '2020-10-31', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = actc_driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(0.4, 0.05),#cv for chl-a and DIN, respectively
                    param_cv = 0.1,#for maxUptake
                    init_cond_cv = c(0.4,0.1),#cv for chl-a and DIN, respectively
                    state_names = c("chla","nitrate"),
                    yini = actc_yini)
    
    progress$set(value = 0.9)
    actc3$out <- format_enkf_output(est_out = est_out, lake_data = actc_lake_data)
    actc3$rmse <- rmse(est_out = est_out, lake_data = actc_lake_data, var = "chla")
    progress$set(value = 1)
    
  })
  observeEvent(input$run_fc_dec3b, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = paste0("Running forecast of chlorophyll-a with 100 members."),
                 detail = "This may take a while. This window will disappear
                     when it is finished running the forecasts.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 36,
                                         freq_din = 36,
                                         lake_data = actc_lake_data,
                                         start_date = actc_start_date)
    
    # #manually introduce semi-random gap representing sensor malfunction
    # start_gap <- sample(5:25,1)
    # gap_length <- sample(1:5,1)
    # gap <- seq(start_gap, start_gap+gap_length, 1)
    # gap_dates <- obs_file$datetime[gap]
    # obs_file[obs_file$datetime %in% gap_dates, "chla"] <- NA
    # gap_df$dat$future$chla[as.Date(gap_df$dat$future$datetime) %in% gap_dates] <- NA
    # 
    n_en = 100 # how many ensemble members 
    
    #randomly draw for observation uncertainty within limits
    obs_cv_chla <- runif(1,0.2,0.4)
    
    #run the forecast!
    
    est_out <- EnKF(n_en = n_en, 
                    start = '2020-10-02', # start date 
                    stop = '2020-10-31', # stop date
                    time_step = 'days',
                    obs_file = obs_file,
                    driver_file = actc_driver_file,
                    n_states_est = 2, 
                    n_params_est = 1,
                    n_params_obs = 0,
                    maxUptake_init = 0.12, 
                    obs_cv = c(0.4, 0.05),#cv for chl-a and DIN, respectively
                    param_cv = 0.1,#for maxUptake
                    init_cond_cv = c(0.4,0.1),#cv for chl-a and DIN, respectively
                    state_names = c("chla","nitrate"),
                    yini = actc_yini)
    
    progress$set(value = 0.9)
    actc3$out <- format_enkf_output(est_out = est_out, lake_data = actc_lake_data)
    actc3$rmse <- rmse(est_out = est_out, lake_data = actc_lake_data, var = "chla")
    progress$set(value = 1)
    
  })
  
  output$fc_dec3a <- renderPlotly({
    
    validate(
      need(!is.null(actc3$out),
           message = "Click 'Run forecast'.")
    )
    var <- "chla" #view_vars$sname[view_vars$lname == input$view_var_both_assim]
    plot_enkf_out_actc(obs_plot = obs_plot_c, start_date = "2020-10-22", 
                  est_out = actc3$out, var = var, 
                  add_obs = input$add_obs_actc3a, 
                  n_days = 36, h_line = 25, show_assim = FALSE)
    
  })
  output$fc_dec3b <- renderPlotly({
    
    validate(
      need(!is.null(actc3$out),
           message = "Click 'Run forecast'.")
    )
    var <- "chla" #view_vars$sname[view_vars$lname == input$view_var_both_assim]
    plot_enkf_out_actc(obs_plot = obs_plot_c, start_date = "2020-10-22", 
                  est_out = actc3$out, var = var, 
                  add_obs = input$add_obs_actc3b, 
                  n_days = 36, h_line = 25, show_assim = FALSE)
    
  })
  
  # observeEvent(input$add_obs_actc2b, {
  #   if(input$add_obs_actc2b & input$run_fc_dec2b == 1) {
  #     showModal(
  #       modalDialog(
  #         title = "Oh dear!",
  #         "There is a data gap during the forecast period due to sensor malfunction.")
  #     )
  #   }
  # })
  # 
  # observeEvent(input$add_obs_actc1a, {
  #   if(input$add_obs_actc1a & input$run_fc_dec1a == 1) {
  #     showModal(
  #       modalDialog(
  #         title = "Oh dear!",
  #         "There is a data gap during the forecast period due to sensor malfunction.")
  #     )
  #   }
  # })
  
  
  # Navigating Tabs ----
  #* Main Tab ====
  rv1 <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$maintab, {
    curr_tab1 <- input$maintab
    rv1$prev <- readr::parse_number(curr_tab1) - 1
    rv1$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  #* Tab 1a ----
  rv1a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries1, {
    curr_tab1 <- input$tabseries1
    rv1a$prev <- readr::parse_number(curr_tab1) - 1
    rv1a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  #* Tab 2a ----
  rv2a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries2, {
    curr_tab1 <- input$tabseries2
    rv2a$prev <- readr::parse_number(curr_tab1) - 1
    rv2a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  #* Tab 3a ----
  rv3a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries3, {
    curr_tab1 <- input$tabseries3
    rv3a$prev <- readr::parse_number(curr_tab1) - 1
    rv3a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  #* Tab 4a ----
  rv4a <- reactiveValues(prev = 0, nxt = 2)
  observeEvent(input$tabseries4, {
    curr_tab1 <- input$tabseries4
    rv4a$prev <- readr::parse_number(curr_tab1) - 1
    rv4a$nxt <- readr::parse_number(curr_tab1) + 1
  })
  
  observe({
    toggleState(id = "prevBtn1", condition = rv1$prev > 0)
    if(rv1$nxt > 7 & rv4a$nxt > 12) {
      shinyjs::disable("nextBtn1")
    } else {
      shinyjs::enable("nextBtn1")
    }
    hide(selector = ".page")
  })
  
  
  #** Next button labels ----
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx + 1]
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if(curr_tab1 == "mtab7") {
      curr_obj <- input$tabseries4
      idx2 <- which(tab_names$tab_id == curr_obj)
      new_nam <- tab_names$name[idx2 + 1]
    }
    if(curr_tab1 == "mtab7" & rv4a$nxt > 12) {
      updateActionButton(session, inputId = "nextBtn1", label = paste("Next >"))
    } else {
      updateActionButton(session, inputId = "nextBtn1", label = paste(new_nam, ">"))
    }
  })
  
  #** Previous button labels ----
  observe({
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    new_nam <- tab_names$name[idx - 1]
    
    if (curr_tab1 == "mtab4") {
      curr_obj <- input$tabseries1
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "stab1") idx2 <- idx2 - 1 # Move off Site selection label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab5") {
      curr_obj <- input$tabseries2
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "stab4") idx2 <- idx2 - 1 # Move off Activity B label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if (curr_tab1 == "mtab6") {
      curr_obj <- input$tabseries3
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "stab7") idx2 <- idx2 - 1 # Move off Activity B label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if(curr_tab1 == "mtab7") {
      curr_obj <- input$tabseries4
      idx2 <- which(tab_names$tab_id == curr_obj)
      if(curr_obj == "stab11") idx2 <- idx2 - 1 # Move off Activity C label
      new_nam <- tab_names$name[idx2 - 1]
    }
    if(curr_tab1 == "mtab1") {
      updateActionButton(session, inputId = "prevBtn1", label = paste("< Previous"))
    } else {
      # shinyjs::show(id = "prevBtn1")
      updateActionButton(session, inputId = "prevBtn1", label = paste("<", new_nam))
    }
  })
  
  #** Advancing Tabs ----
  observeEvent(input$nextBtn1, {
    
    if(input$nextBtn1 %in% c(5, 10, 15)) {
      showModal(
        modalDialog(
          title = "Save Progress",
          "Don't forget to save your progress as you go just in case you lose connection with the server. Click 'Download user input' at the bottom of the page to save a snapshot of your answers so far.")
      )
    } else {
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
      if (curr_tab1 == "mtab4" & rv1a$nxt < 4) {
        curr_obj <- input$tabseries1
        updateTabsetPanel(session, "tabseries1",
                          selected = paste0("stab", rv1a$nxt))
        
      } else if (curr_tab1 == "mtab5" & rv2a$nxt < 7) {
        curr_obj <- input$tabseries2
        
        updateTabsetPanel(session, "tabseries2",
                          selected = paste0("stab", rv2a$nxt))
        
      } else if (curr_tab1 == "mtab6" & rv3a$nxt < 11) {
        curr_obj <- input$tabseries3
        updateTabsetPanel(session, "tabseries3",
                          selected = paste0("stab", rv3a$nxt))
      } else if (curr_tab1 == "mtab7" & rv4a$nxt < 14) {
        curr_obj <- input$tabseries4
        updateTabsetPanel(session, "tabseries4",
                          selected = paste0("stab", rv4a$nxt))
      } else {
        updateTabsetPanel(session, "tabseries1",
                          selected = "stab1")
        updateTabsetPanel(session, "tabseries2",
                          selected = "stab4")
        updateTabsetPanel(session, "tabseries3",
                          selected = "stab7")
        updateTabsetPanel(session, "tabseries4",
                          selected = "stab11")
        updateTabsetPanel(session, "maintab",
                          selected = paste0("mtab", rv1$nxt))
      }
      shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
    }
  })
  
  #** Moving back through tabs ----
  observeEvent(input$prevBtn1, {
    curr_tab1 <- input$maintab
    idx <- which(tab_names$tab_id == curr_tab1)
    if (curr_tab1 == "mtab4" & rv1a$prev > 0) {
      curr_obj <- input$tabseries1
      
      updateTabsetPanel(session, "tabseries1",
                        selected = paste0("stab", rv1a$prev))
      
    } else if (curr_tab1 == "mtab5" & rv2a$prev > 3) {
      curr_obj <- input$tabseries2
      
      updateTabsetPanel(session, "tabseries2",
                        selected = paste0("stab", rv2a$prev))
      
    } else if (curr_tab1 == "mtab6" & rv3a$prev > 6) {
      curr_obj <- input$tabseries3
      updateTabsetPanel(session, "tabseries3",
                        selected = paste0("stab", rv3a$prev))
    } else if (curr_tab1 == "mtab7" & rv4a$prev > 10) {
      curr_obj <- input$tabseries4
      updateTabsetPanel(session, "tabseries4",
                        selected = paste0("stab", rv4a$prev))
    } else {
      updateTabsetPanel(session, "tabseries1",
                        selected = "stab3")
      updateTabsetPanel(session, "tabseries2",
                        selected = "stab6")
      updateTabsetPanel(session, "tabseries3",
                        selected = "stab10")
      updateTabsetPanel(session, "maintab",
                        selected = paste0("mtab", rv1$prev))
    }
    shinyjs::runjs("window.scrollTo(0, 0)")
  })
  
  # Help buttons ----
  observeEvent(input$help, {
    introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
  })
  observeEvent(input$help2, {
    shinyalert(title = "Resume Progress", text = "Use this field to upload your '.eddie' file to resume your progress.", type = "info")
  })
  
  
    
    
  # Select site when uploading answers
  
  observe({
    if(input$row_num != "") {
      dt_proxy <- dataTableProxy("table01")
      selectRows(dt_proxy, input$row_num)
    }
  })
  
  # Repopulate data collection frequency table
  observe({
    req(input$maintab == "mtab4" & exists("up_answers") & input$tabseries1 == "stab2")
    updateNumericInput(session, "q4_at_freq", value = up_answers$a4_at_freq)
    updateNumericInput(session, "q4_wt_freq", value = up_answers$a4_wt_freq)
    updateNumericInput(session, "q4_swr_freq", value = up_answers$a4_swr_freq)
    updateNumericInput(session, "q4_par_freq", value = up_answers$a4_par_freq)
    updateNumericInput(session, "q4_n_freq", value = up_answers$a4_n_freq)
    updateNumericInput(session, "q4_chl_freq", value = up_answers$a4_chl_freq)
  })
  
  #** Render Report ----
  report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  report2 <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  
  observeEvent(input$generate, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Gathering data and building report.",
                 detail = "This may take a while. This window will disappear
                     when the report is ready.", value = 0)
    
    
    # Set up parameters to pass to Rmd document
    params <- list(name = input$name,
                   id_number = input$id_number,
                   a1 = input$q1,
                   a2 = input$q2,
                   a3a = input$q3a,
                   a3b = input$q3b,
                   a3c = input$q3c,
                   a3d = input$q3d,
                   a3e = input$q3e,
                   a3f = input$q3f,
                   a4_at_freq = q4_ans$dt[1,1],
                   a4_wt_freq = q4_ans$dt[2,1],
                   a4_swr_freq = q4_ans$dt[3,1],
                   a4_par_freq = q4_ans$dt[4,1],
                   a4_n_freq = q4_ans$dt[5,1],
                   a4_chl_freq = q4_ans$dt[6,1],
                   a5 = input$q5,
                   a6a = input$q6a,
                   a6b = input$q6b,
                   a6c = input$q6c,
                   a6d = input$q6d,
                   a7a = input$q7a,
                   a7b = input$q7b,
                   a7c = input$q7c,
                   a8_states = input$rank_list_2,
                   a8_pars = input$rank_list_3,
                   a9 = input$q9,
                   a10 = input$q10,
                   a11 = input$q11,
                   a12 = input$q12,
                   a13 = input$q13,
                   a14 = input$q14,
                   a15 = input$q15,
                   a16 = input$q16,
                   a17 = input$q17,
                   a18 = input$q18,
                   a19 = input$q19,
                   a20 = input$q20,
                   a21 = input$q21,
                   a22 = input$q22,
                   a23 = input$q23,
                   a24 = input$q24,
                   a25 = input$q25,
                   a26 = input$q26,
                   a27 = input$q27,
                   a28 = input$q28,
                   a29 = input$q29,
                   a30 = input$q30,
                   a31 = input$q31,
                   a32 = input$q32,
                   a33 = input$q33,
                   a34 = input$q34,
                   a35 = input$q35,
                   a36 = input$q36,
                   a37 = input$q37,
                   da_method_plot = "www/compare_da.png",
                   no_da_rmse = da_method$dt$RMSE[1],
                   chla_assim_rmse = da_method$dt$RMSE[2],
                   obs_uc_rmse = obs_uc_rmse$dt,
                   da_freq_rmse = da_freq_rmse$dt
                   # save_pars = par_file,
                   # pheno_file = pheno_file$img,
                   # site_html = "data/site.html",
                   # mod_2019_png = "www/mod_run_2019.png",
                   # noaa_plot = "www/noaa_fc.png",
                   # comm_plot = "www/comm_fc_plot.png",
                   # assess_plot = "www/assess_fc.png",
                   # update_plot = "www/fc_update.png",
                   # next_fc_plot = "www/new_fc.png",
                   # wt_m = lmfit2$m,
                   # wt_b = lmfit2$b,
                   # wt_r2 = lmfit2$r2,
                   # upar_m = lmfit3$m,
                   # upar_b = lmfit3$b,
                   # upar_r2 = lmfit3$r2,
                   # mod_summ = summ_file
    )
    print(params)
    
    
    tmp_file <- paste0(tempfile(), ".docx") #Creating the temp where the .pdf is going to be stored
    
    rmarkdown::render("report.Rmd",
                      output_format = "all",
                      output_file = tmp_file,
                      params = params,
                      envir = new.env(parent = globalenv()))
    progress$set(value = 1)
    report$filepath <- tmp_file #Assigning in the temp file where the .docx is located to the reactive file created above
  })
  
  observeEvent(input$generate2, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Gathering data and building report.",
                 detail = "This may take a while. This window will disappear
                     when the report is ready.", value = 0)
    
    
    # Set up parameters to pass to Rmd document
    params <- list(name = input$name,
                   id_number = input$id_number,
                   a1 = input$q1,
                   a2 = input$q2,
                   a3a = input$q3a,
                   a3b = input$q3b,
                   a3c = input$q3c,
                   a3d = input$q3d,
                   a3e = input$q3e,
                   a3f = input$q3f,
                   a4_at_freq = q4_ans$dt[1,1],
                   a4_wt_freq = q4_ans$dt[2,1],
                   a4_swr_freq = q4_ans$dt[3,1],
                   a4_par_freq = q4_ans$dt[4,1],
                   a4_n_freq = q4_ans$dt[5,1],
                   a4_chl_freq = q4_ans$dt[6,1],
                   a5 = input$q5,
                   a6a = input$q6a,
                   a6b = input$q6b,
                   a6c = input$q6c,
                   a6d = input$q6d,
                   a7a = input$q7a,
                   a7b = input$q7b,
                   a7c = input$q7c,
                   a8_states = input$rank_list_2,
                   a8_pars = input$rank_list_3,
                   a9 = input$q9,
                   a10 = input$q10,
                   a11 = input$q11,
                   a12 = input$q12,
                   a13 = input$q13,
                   a14 = input$q14,
                   a15 = input$q15,
                   a16 = input$q16,
                   a17 = input$q17,
                   a18 = input$q18,
                   a19 = input$q19,
                   a20 = input$q20,
                   a21 = input$q21,
                   a22 = input$q22,
                   a23 = input$q23,
                   a24 = input$q24,
                   a25 = input$q25,
                   a26 = input$q26,
                   a27 = input$q27,
                   a28 = input$q28,
                   a29 = input$q29,
                   a30 = input$q30,
                   a31 = input$q31,
                   a32 = input$q32,
                   a33 = input$q33,
                   a34 = input$q34,
                   a35 = input$q35,
                   a36 = input$q36,
                   a37 = input$q37,
                   da_method_plot = "www/compare_da.png",
                   no_da_rmse = da_method$dt$RMSE[1],
                   chla_assim_rmse = da_method$dt$RMSE[2],
                   obs_uc_rmse = obs_uc_rmse$dt,
                   da_freq_rmse = da_freq_rmse$dt
                   # save_pars = par_file,
                   # pheno_file = pheno_file$img,
                   # site_html = "data/site.html",
                   # mod_2019_png = "www/mod_run_2019.png",
                   # noaa_plot = "www/noaa_fc.png",
                   # comm_plot = "www/comm_fc_plot.png",
                   # assess_plot = "www/assess_fc.png",
                   # update_plot = "www/fc_update.png",
                   # next_fc_plot = "www/new_fc.png",
                   # wt_m = lmfit2$m,
                   # wt_b = lmfit2$b,
                   # wt_r2 = lmfit2$r2,
                   # upar_m = lmfit3$m,
                   # upar_b = lmfit3$b,
                   # upar_r2 = lmfit3$r2,
                   # mod_summ = summ_file
    )
    print(params)
    
    
    tmp_file <- paste0(tempfile(), ".docx") #Creating the temp where the .pdf is going to be stored
    
    rmarkdown::render("report.Rmd",
                      output_format = "all",
                      output_file = tmp_file,
                      params = params,
                      envir = new.env(parent = globalenv()))
    progress$set(value = 1)
    report2$filepath <- tmp_file #Assigning in the temp file where the .docx is located to the reactive file created above
  })
  
  # Hide download button until report is generated
  output$reportbuilt <- reactive({
    return(!is.null(report$filepath))
  })
  outputOptions(output, 'reportbuilt', suspendWhenHidden = FALSE)
  
  # Hide download button until report is generated
  output$reportbuilt2 <- reactive({
    return(!is.null(report2$filepath))
  })
  outputOptions(output, 'reportbuilt2', suspendWhenHidden = FALSE)
  
  #** Download Report ----
  
  #Download report
  output$download <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("module7_report_", input$id_number, ".docx") %>%
        gsub(" ", "_", .)
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      file.copy(report$filepath, file)
    }
  )
  
  #Download report
  output$download2 <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("module7_report_", input$id_number, ".docx") %>%
        gsub(" ", "_", .)
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      file.copy(report2$filepath, file)
    }
  )
  
})

# end
