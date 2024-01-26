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
  
  observe({
    if(input$row_num != "") {
      dt_proxy <- dataTableProxy("table01")
      selectRows(dt_proxy, input$row_num)
    }
  })

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
  
  observe({
  
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
  
  # Slickr model output
  output$model_slides <- renderSlickR({
    slickR(model_slides)
  })
  
  # Plot lagged chlorophyll-a time series
  plot.lag1 <- reactiveValues(main=NULL)
  
  observe({
    
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
  
  observe({
  
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
  
  observe({
    
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
  
  })
  
  # Autocorrelation plot many lags ----
  plot.ac <- reactiveValues(main=NULL)
  
  observe({
  
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
  
  observe({
  
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
  
  observe({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    # validate(
    #   need(input$fit_model > 0,
    #        message = "Click 'Fit model'")
    # )
    
    #assign dataframe
    df <- autocorrelation_data$df
    
    #fit model
    ar.model$fit <- ar.ols(df$chla, order.max = 1, aic = FALSE,
                           intercept = TRUE, demean = TRUE)
    
    #extract parameters
    ar.model$intercept = round(c(ar.model$fit$x.intercept),2) #beta_0
    ar.model$ar1 = round(c(ar.model$fit$ar),2) #beta_1
    ar.model$chla_mean = round(c(ar.model$fit$x.mean),2) #mean chla
    
    #get predictions
    mod <- ar.model$intercept + ar.model$ar1 * (df$chla - ar.model$chla_mean) + ar.model$chla_mean
    
    model_fit_data$df <- tibble(date = df$datetime,
                                chla = df$chla,
                                model = mod,
                                residuals = (mod - df$chla))
    
  })
  
  observe({
  output$ar_model <- renderUI({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(input$fit_model > 0,
           message = "Click 'Fit model'")
    )
    
    #make equation
    ar.model$eqn <- paste0("$$Chla_{t} = ",ar.model$intercept," + ",ar.model$ar1," * (Chla_{t-1} - ",ar.model$chla_mean,") + ",ar.model$chla_mean,"$$")
    
    return(withMathJax(
      tags$p(ar.model$eqn)
    ))
  })
  })
  
  # Model fit plot ----
  model_fit_data <- reactiveValues(df = NULL)
  plot.arfit <- reactiveValues(main=NULL)
  
  observe({
  
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
    
    p <- plot_mod_predictions(model_fit_plot_data = model_fit_data$df, variable_name = "Chlorophyll-a (ug/L)")
    
    plot.arfit$main <- p
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
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
  
  observe({
    
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
  
  })
  
  # Text output for RMSE ----
  observe({
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
                                  ic_sd = NULL,
                                  ic_distrib_x_lim = NULL,
                                  ic_distrib_y_lim = NULL)
  
  # create reactive value to hold things for objective 6 later
  obs_uc <- reactiveValues(ic_sd_low = NULL,
                           ic_distribution_low = NULL,
                           ic_sd_high = NULL,
                           ic_distribution_high = NULL,
                           ic_distrib_low_y_lim=NULL,
                           ic_distrib_high_x_lim=NULL)
  
  # process distribution
  observe({
    
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
    
    first_forecast$n_members <- 500
    
    first_forecast$process_distribution <- rnorm(n = first_forecast$n_members, mean = 0, sd = first_forecast$sigma)
    
  })
  
  
  # Text output for proc uc sd ----
  observe({
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
    
    proc_uc_sd <- paste("<b>","Process uncertainty standard deviation: ",round(first_forecast$sigma,2),"</b>", sep = "")
    
    HTML(paste(proc_uc_sd))
  })
  })
  
  # Process uncertainty distribution plot ----
  plot.proc.uc.distrib <- reactiveValues(main=NULL)
  
  observe({
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
    
    p <- plot_process_dist(proc_uc = first_forecast$process_distribution)
    
    plot.proc.uc.distrib$main <- p
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
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
  
  # wrangle high frequency data
  observe({
    
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
  
  # calculate ic distribution
  observe({
    
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
    
    df <- lake_data$df
    
    start_date <- "2020-09-25"
    first_forecast_dates$start_date <- "2020-09-25"
    
    curr_chla <- df %>%
      filter(abs(difftime(datetime,start_date)) == min(abs(difftime(datetime,start_date)))) %>%
      pull(chla)
    first_forecast$curr_chla <- curr_chla
    
    n_members <- as.numeric(first_forecast$n_members)
    
    ic_sd <- as.numeric(first_forecast$ic_sd)
    ic_sd_low = ic_sd/2
    obs_uc$ic_sd_low <- ic_sd_low
    ic_sd_high = ic_sd*2
    obs_uc$ic_sd_high <- ic_sd_high
    
    ic_distribution <- rnorm(n = n_members, mean = curr_chla, sd = ic_sd)
    first_forecast$ic_distribution <- ic_distribution
    ic_distribution_low <- rnorm(n = n_members, mean = curr_chla, sd = ic_sd_low)
    obs_uc$ic_distribution_low <- ic_distribution_low
    ic_distribution_high <- rnorm(n = n_members, mean = curr_chla, sd = ic_sd_high)
    obs_uc$ic_distribution_high <- ic_distribution_high
    
    p <- plot_ic_dist(curr_chla = first_forecast$curr_chla, ic_uc = first_forecast$ic_distribution)
    first_forecast$ic_distrib_x_lim = range(c(layer_scales(p)$x$range$range))
    first_forecast$ic_distrib_y_lim = range(c(layer_scales(p)$y$range$range))
    
    p1 <- plot_ic_dist(curr_chla = first_forecast$curr_chla, ic_uc = obs_uc$ic_distribution_low)
    obs_uc$ic_distrib_low_y_lim = range(c(layer_scales(p1)$y$range$range))
    
    p2 <- plot_ic_dist(curr_chla = first_forecast$curr_chla, ic_uc = obs_uc$ic_distribution_high)
    obs_uc$ic_distrib_high_x_lim = range(c(layer_scales(p2)$x$range$range))
    
    plot.ic.uc.distrib$main <- p
    plot.ic.uc.distrib$low <- p1
    plot.ic.uc.distrib$high <- p2
    
  })
  
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
    
    ic_uc_sd <- paste("<b>","Initial condition uncertainty standard deviation: ",round(first_forecast$ic_sd,2),"</b>", sep = "")
    
    HTML(paste(ic_uc_sd))
  })
  
  #IC distribution plot
  first_forecast_dates <- reactiveValues(start_date = NULL,
                                         forecast_date = NULL)
  plot.ic.uc.distrib <- reactiveValues(main=NULL,
                                       low=NULL,
                                       high=NULL)
  
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

    p = plot.ic.uc.distrib$main
    
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
  
  #generate first forecast
  observe({
    
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
    forecast_chla = ifelse(forecast_chla < 0, 0, forecast_chla)
    first_forecast$forecast_chla <- forecast_chla
    
  })
  
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
    
    p <- plot_fc_dist(forecast_dist = first_forecast$forecast_chla)
    
    plot.fc1$main <- p
    
    return(ggplotly(p, dynamicTicks = FALSE))
    
  })
  
  
  #First forecast visualization over time
  plot.fc1.viz <- reactiveValues(main=NULL)
  
  observe({
  
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
  
  # update ic
  observe({
    
    validate(
      need(!is.null(autocorrelation_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(model_fit_data$df),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$view_new_obs > 0,
           message = "Click 'View new observation'")
    )
    
    #unpacking values we will need
    intercept = as.numeric(ar.model$intercept)
    ar1 = as.numeric(ar.model$ar1)
    chla_mean = as.numeric(ar.model$chla_mean)
    process_distribution = as.numeric(first_forecast$process_distribution)
    
    df <- lake_data$df
    
    new_obs <- df %>%
      filter(datetime == first_forecast_dates$forecast_date) %>%
      pull(chla)
    
    EnKF_inputs_outputs$new_obs <- new_obs
    
    forecast_chla <- first_forecast$forecast_chla
    ic_sd <- first_forecast$ic_sd
    
    ic_update <- EnKF(forecast = forecast_chla, new_observation = new_obs, ic_sd = ic_sd)
    EnKF_inputs_outputs$updated_ic <- ic_update
    
    #generate second forecast
    second_forecast_da = intercept + ar1 * (ic_update - chla_mean) + chla_mean + process_distribution
    second_forecast_da = ifelse(second_forecast_da < 0, 0, second_forecast_da)
    EnKF_inputs_outputs$second_forecast_da = second_forecast_da
    
    #add in new dates
    second_forecast_date = "2020-09-27"
    EnKF_inputs_outputs$second_forecast_date <- second_forecast_date
    forecast_date = first_forecast_dates$forecast_date
    forecast_dates = c(forecast_date, second_forecast_date)
    EnKF_inputs_outputs$forecast_dates = forecast_dates
    
    #updated ic no da
    ic_update_no_da <- EnKF(forecast = forecast_chla, new_observation = NA, ic_sd = ic_sd)
    EnKF_inputs_outputs$updated_ic_no_da <- ic_update_no_da
    
    #generate second forecast with no da
    second_forecast_no_da = intercept + ar1 * (ic_update_no_da - chla_mean) + chla_mean + process_distribution
    second_forecast_no_da = ifelse(second_forecast_no_da < 0, 0, second_forecast_no_da)
    EnKF_inputs_outputs$second_forecast_no_da = second_forecast_no_da
    
    
  })
  
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$view_new_obs > 0,
           message = "Click 'View new observation'")
    )
    
    new_obs_text <- paste("<b>","New observation: ",round(EnKF_inputs_outputs$new_obs,2),"</b>", sep = "")
    
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
      need(!is.null(first_forecast$forecast_chla),
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
    
    previous_plot <- plot_fc_1day(curr_chla, start_date, forecast_date, ic_distribution, forecast_chla, n_members)
    
    if(input$update_ic > 0 & input$view_new_obs > 0){
      new_obs <- EnKF_inputs_outputs$new_obs
      chla_obs <- c(curr_chla, new_obs) #vector of observations to use for plotting
      ic_update = EnKF_inputs_outputs$updated_ic 
      p1 <- plot_fc_update(chla_obs, start_date, forecast_date, ic_distribution, ic_update, forecast_chla, n_members)
      plot.updated.ic$main <- p1
      return(p1)
    } else if(input$view_new_obs > 0) {
      new_obs <- EnKF_inputs_outputs$new_obs
      p <- plot_fc_new_obs(previous_plot = previous_plot, new_obs = new_obs, forecast_date = forecast_date)
      plot.updated.ic$main <- p
      return(p)
    } else {
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
  plot.second.fc.da <- reactiveValues(main=NULL,
                                      ylim=NULL)
  
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$view_new_obs > 0,
           message = "Please view the new observation above.")
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
    second_forecast = EnKF_inputs_outputs$second_forecast_da
    forecast_dates = EnKF_inputs_outputs$forecast_dates
    curr_chla = as.numeric(first_forecast$curr_chla)
    new_obs = EnKF_inputs_outputs$new_obs
    chla_obs = c(curr_chla, new_obs) #vector of observations to use for plotting
    start_date = first_forecast_dates$start_date
    ic_distribution = as.numeric(first_forecast$ic_distribution)
    forecast_chla = as.numeric(first_forecast$forecast_chla)
    n_members = as.numeric(first_forecast$n_members)
    
    p <- plot_second_forecast(chla_obs, start_date, forecast_dates, ic_distribution, 
                         ic_update, forecast_chla, second_forecast, n_members)
    ylim <- range(c(layer_scales(p)$y$range$range))
    
    plot.second.fc.da$main <- p
    plot.second.fc.da$ylim <- ylim
    
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
      need(!is.null(first_forecast$forecast_chla),
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
    
    previous_plot <- plot_fc_1day(curr_chla, start_date, forecast_date, ic_distribution, forecast_chla, n_members)
    
    if(input$view_ic_no_da > 0 & input$show_ic == TRUE){
      new_obs <- NA
      chla_obs <- c(curr_chla, new_obs) #vector of observations to use for plotting
      ic_update_no_da = EnKF_inputs_outputs$updated_ic_no_da
      p1 <- plot_fc_update(chla_obs, start_date, forecast_date, ic_distribution, ic_update_no_da, forecast_chla, n_members)
      plot.updated.ic.no.da$main <- p1
      return(p1)
    } else {
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
      need(!is.null(first_forecast$forecast_chla),
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
    second_forecast_no_da = EnKF_inputs_outputs$second_forecast_no_da
    
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
  
  # Repeat plots for comparison at bottom of objective
  
  output$second_fc_da_plot2 <- renderPlot({
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$update_ic > 0,
           message = "Please update the initial condition above.")
    )
    validate(
      need(input$second_forecast_da > 0,
           message = "Please generate a forecast with data assimilation above.")
    )
    
    p <- plot.second.fc.da$main
    return(p)
  })
  
  output$second_fc_no_da_plot2 <- renderPlot({
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$view_ic_no_da > 0,
           message = "Please click 'Run ensemble Kalman filter with missing observation' above.")
    )
    validate(
      need(input$second_forecast_no_da > 0,
           message = "Please generate a forecast with no data assimilation above.")
    )
    p <- plot.second.fc.no.da$main
    return(p)
  })
  
  #### Objective 6 ----
  
  #slickr slides
  output$chla_obs_uc_slides <- renderSlickR({
    slickR(chla_obs_uc_slides) + settings(dots = TRUE)
  })
  
  #repeat of original initial conditions plot
  output$ic_distrib_plot2 <- renderPlotly({
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$plot_low_ic > 0,
           message = "Please click 'Generate distribution'.")
    )
    
    x.lim <- range(obs_uc$ic_distrib_high_x_lim)
    y.lim <- range(obs_uc$ic_distrib_low_y_lim)
    
    p <- plot.ic.uc.distrib$main +
      ylim(y.lim) +
      xlim(x.lim) +
      ggtitle("")
    
    return(ggplotly(p, dynamicTicks = TRUE))
  })
  
  # ic distribution with low obs uncertainty
  plot.ic.distrib.low <- reactiveValues(main=NULL,
                                        og=NULL)
  
  output$ic_distrib_low_plot <- renderPlotly({ 
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$plot_low_ic > 0,
           message = "Please click 'Generate distribution'.")
    )
    

    x.lim <- range(obs_uc$ic_distrib_high_x_lim)
    y.lim <- range(obs_uc$ic_distrib_low_y_lim)
    
    p <- plot.ic.uc.distrib$low +
      ylim(y.lim) +
      xlim(x.lim) +
      ggtitle("")
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # 2 forecasts assimilating data with low uncertainty
  plot.second.fc.obs.uc <- reactiveValues(main=NULL,
                                          low=NULL,
                                          high=NULL)
  
  observe({
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(!is.null(EnKF_inputs_outputs$second_forecast_da),
           message = "Please generate a forecast with data assimilation in Objective 5.")
    )
    validate(
      need(input$plot_low_ic > 0,
           message = "Please click 'Generate distributions'.")
    )
    
    #unpacking values we will need
    intercept = as.numeric(ar.model$intercept)
    ar1 = as.numeric(ar.model$ar1)
    chla_mean = as.numeric(ar.model$chla_mean)
    ic_distribution_low = obs_uc$ic_distribution_low
    ic_distribution_high = obs_uc$ic_distribution_high
    process_distribution = as.numeric(first_forecast$process_distribution)
    ic_update = EnKF_inputs_outputs$updated_ic
    second_forecast = EnKF_inputs_outputs$second_forecast_da
    forecast_dates = EnKF_inputs_outputs$forecast_dates
    curr_chla = as.numeric(first_forecast$curr_chla)
    new_obs = EnKF_inputs_outputs$new_obs
    chla_obs = c(curr_chla, new_obs) #vector of observations to use for plotting
    start_date = first_forecast_dates$start_date
    ic_distribution = as.numeric(first_forecast$ic_distribution)
    forecast_chla = as.numeric(first_forecast$forecast_chla)
    n_members = as.numeric(first_forecast$n_members)
    
    p <- plot_second_forecast(chla_obs, start_date, forecast_dates, ic_distribution, 
                              ic_update, forecast_chla, second_forecast, n_members)
    ylim <- range(c(layer_scales(p)$y$range$range))
    
    #first forecast
    first_forecast_low_obs_uc = intercept + ar1 * (ic_distribution_low - chla_mean) + chla_mean + process_distribution
    first_forecast_low_obs_uc = ifelse(first_forecast_low_obs_uc < 0, 0, first_forecast_low_obs_uc)
    
    first_forecast_high_obs_uc = intercept + ar1 * (ic_distribution_high - chla_mean) + chla_mean + process_distribution
    first_forecast_high_obs_uc = ifelse(first_forecast_high_obs_uc < 0, 0, first_forecast_high_obs_uc)
    
    #more unpacking
    ic_sd_low = obs_uc$ic_sd_low
    ic_sd_high = obs_uc$ic_sd_high

    #update ic
    ic_update_low_obs_uc <- EnKF(forecast = first_forecast_low_obs_uc, 
                                 new_observation = new_obs, ic_sd = ic_sd_low)
    ic_update_high_obs_uc <- EnKF(forecast = first_forecast_high_obs_uc, 
                                 new_observation = new_obs, ic_sd = ic_sd_high)
    
    #second forecast
    second_forecast_low_obs_uc = intercept + ar1 * (ic_update_low_obs_uc - chla_mean) + chla_mean + process_distribution
    second_forecast_low_obs_uc = ifelse(second_forecast_low_obs_uc < 0, 0, second_forecast_low_obs_uc)
    
    second_forecast_high_obs_uc = intercept + ar1 * (ic_update_high_obs_uc - chla_mean) + chla_mean + process_distribution
    second_forecast_high_obs_uc = ifelse(second_forecast_high_obs_uc < 0, 0, second_forecast_high_obs_uc)
    
    p1 <- plot_second_forecast(chla_obs, start_date, forecast_dates, 
                              ic_distribution_low, ic_update_low_obs_uc, 
                              first_forecast_low_obs_uc, second_forecast_low_obs_uc, 
                              n_members)
    low_ylim <- range(c(layer_scales(p1)$y$range$range))
    p2 <- plot_second_forecast(chla_obs, start_date, forecast_dates, 
                              ic_distribution_high, ic_update_high_obs_uc, 
                              first_forecast_high_obs_uc, second_forecast_high_obs_uc, 
                              n_members)
    high_ylim <- range(c(layer_scales(p2)$y$range$range))
    
    final_ylim <- range(c(ylim, low_ylim, high_ylim))
    
    p <- p +
      ylim(final_ylim)
    p1 <- p1 + ylim(final_ylim)
    p2 <- p2 + ylim(final_ylim)
    
    plot.second.fc.obs.uc$main <- p
    plot.second.fc.obs.uc$low <- p1
    plot.second.fc.obs.uc$high <- p2
    
  })
  
  # repeat of 2-forecast plot with DA from obj 5
  output$second_fc_da_plot3 <- renderPlot({
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(!is.null(EnKF_inputs_outputs$second_forecast_da),
           message = "Please generate a forecast with data assimilation in Objective 5.")
    )
    validate(
      need(!is.null(plot.second.fc.obs.uc$main),
           message = "Please generate the distributions above.")
    )
    validate(
      need(input$plot_fc_low_obs_uc > 0,
           message = "Please click 'Plot forecasts with low observation uncertainty'.")
    )
    
    p <- plot.second.fc.obs.uc$main
    return(p)
    
  })
  
  output$second_fc_low_obs_uc <- renderPlot({ 
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(!is.null(EnKF_inputs_outputs$second_forecast_da),
           message = "Please generate a forecast with data assimilation in Objective 5.")
    )
    validate(
      need(!is.null(plot.second.fc.obs.uc$main),
           message = "Please generate the distributions above.")
    )
    validate(
      need(input$plot_fc_low_obs_uc > 0,
           message = "Please click 'Plot forecasts with low observation uncertainty'.")
    )
    
    p <- plot.second.fc.obs.uc$low
    return(p)
    
  })
  
  # Download plot
  output$save_second_fc_low_obs_uc_plot <- downloadHandler(
    filename = function() {
      paste("Q34-Q35-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.second.fc.obs.uc$low, device = device)
    }
  )
  
  #another repeat of original initial conditions plot
  output$ic_distrib_plot3 <- renderPlotly({
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$plot_high_ic > 0,
           message = "Please click 'Generate distribution'.")
    )
    
    x.lim <- range(obs_uc$ic_distrib_high_x_lim)
    y.lim <- range(obs_uc$ic_distrib_low_y_lim)
    
    p <- plot.ic.uc.distrib$main +
      ylim(y.lim) +
      xlim(x.lim) +
      ggtitle("")
    
    return(ggplotly(p, dynamicTicks = TRUE))
  })
  
  # ic distribution with high obs uncertainty
  plot.ic.distrib.high <- reactiveValues(main=NULL)
  
  output$ic_distrib_high_plot <- renderPlotly({ 
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(input$plot_high_ic > 0,
           message = "Please click 'Generate distribution'.")
    )
    
    
    x.lim <- range(obs_uc$ic_distrib_high_x_lim)
    y.lim <- range(obs_uc$ic_distrib_low_y_lim)
    
    p <- plot.ic.uc.distrib$high +
      ylim(y.lim) +
      xlim(x.lim) +
      ggtitle("")
    
    return(ggplotly(p, dynamicTicks = TRUE))
    
  })
  
  # repeat of 2-forecast plot with DA from obj 5
  output$second_fc_da_plot4 <- renderPlot({
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(!is.null(EnKF_inputs_outputs$second_forecast_da),
           message = "Please generate a forecast with data assimilation in Objective 5.")
    )
    validate(
      need(!is.null(plot.second.fc.obs.uc$main),
           message = "Please generate the distributions above.")
    )
    validate(
      need(input$plot_fc_high_obs_uc > 0,
           message = "Please click 'Plot forecasts with high observation uncertainty'.")
    )
    
    p <- plot.second.fc.obs.uc$main
    return(p)
    
  })
  
  output$second_fc_high_obs_uc <- renderPlot({ 
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate and visualize the forecast in Objective 4.")
    )
    validate(
      need(!is.null(EnKF_inputs_outputs$second_forecast_da),
           message = "Please generate a forecast with data assimilation in Objective 5.")
    )
    validate(
      need(!is.null(plot.second.fc.obs.uc$main),
           message = "Please generate the distributions above.")
    )
    validate(
      need(input$plot_fc_high_obs_uc > 0,
           message = "Please click 'Plot forecasts with high observation uncertainty'.")
    )
    
    p <- plot.second.fc.obs.uc$high
    return(p)
    
  })
  
  # Download plot
  output$save_second_fc_high_obs_uc_plot <- downloadHandler(
    filename = function() {
      paste("Q38-Q39-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.second.fc.obs.uc$high, device = device)
    }
  )
  
  #### Objective 7 ----
  
  #slickr slides
  output$chla_frequency_slides <- renderSlickR({
    slickR(chla_frequency_slides) + settings(dots = TRUE)
  })
  
  #create reactive for forecast series plots
  forecast.series.data <- reactiveValues(none=NULL,
                                    weekly=NULL,
                                    daily=NULL)
  forecast.series <- reactiveValues(none=NULL,
                                    weekly=NULL,
                                    daily=NULL)
  forecast.means <- reactiveValues(none=NULL,
                                    weekly=NULL,
                                    daily=NULL)
  plot.forecast.series <- reactiveValues(none=NULL,
                                          weekly=NULL,
                                          daily=NULL)
  plot.forecast.series.w.obs <- reactiveValues(none=NULL,
                                         weekly=NULL,
                                         daily=NULL)
  
  #high priority observer : if user selects a new site, this waits
  #until all upstream things are re-run before re-running forecasts
  #because they take awhile
  values <- reactiveValues(waitForColumns = FALSE)
  observe(priority = 10, {
    lake_data$df
    values$waitForColumns <- TRUE
  })
  observe({
    first_forecast$forecast_chla
    values$waitForColumns <- FALSE
  })
  
  #run all the forecasts
  observe({
    
    validate(
      need(!is.null(lake_data$df),
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(ar.model$chla_mean),
           message = "Please fit an AR model in Objective 3.")
    )
    validate(
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate the forecast in Objective 4.")
    )
    validate(
      need(input$fc_series_no_da > 0,
           message = "Please click 'Run forecasts with no data assimilation'.")
    )
    
    validate(need(!values$waitForColumns, FALSE))
    
    progress <- shiny::Progress$new()
    progress_value = 0.2
    progress$set(message = "Running forecasts",
                 detail = "This may take a while. This window will disappear
                     when forecasts are complete.", value = progress_value)
    
    intercept = as.numeric(ar.model$intercept)
    ar1 = as.numeric(ar.model$ar1)
    chla_mean = as.numeric(ar.model$chla_mean)
    n_members = first_forecast$n_members
    process_distribution = first_forecast$process_distribution
    ic_sd = first_forecast$ic_sd
    
    days_to_forecast = 10
    
    forecast_dates <- seq.Date(from = as.Date(first_forecast_dates$start_date), to = as.Date(first_forecast_dates$start_date) + days_to_forecast, by = 'days')
    
    chla_observations <- lake_data$df %>%
      filter(datetime %in% forecast_dates)
    
    chla_assimilation_frequencies <- c(11,7,1)
    
    for(j in 1:length(chla_assimilation_frequencies)){
      
      chla_assimilation_dates <- forecast_dates[seq(1, length(forecast_dates), chla_assimilation_frequencies[j])]
      
      forecast_data <- lake_data$df %>%
        select(datetime, chla) %>%
        mutate(datetime = as.Date(datetime)) %>%
        filter(datetime %in% forecast_dates) %>%
        mutate(chla = ifelse(datetime %in% chla_assimilation_dates,chla,NA)) 
      
      forecast_series <- tibble(date = rep(forecast_dates, each = n_members*2),
                                      chla = NA_real_,
                                      ensemble_member = rep(1:n_members, times = length(forecast_dates)*2),
                                      data_type = rep(c("fc","ic"), each = n_members, times = length(forecast_dates)))
      
      ic_distribution = first_forecast$ic_distribution
      
      temp_ic <- tibble(date = rep(forecast_dates[1], each = n_members),
                        chla = ic_distribution,
                        ensemble_member = c(1:n_members),
                        data_type = rep("ic", times = n_members))
      
      forecast_series <- forecast_series %>%
        rows_update(temp_ic, by = c("date","ensemble_member","data_type"))
      
      for(i in 2:length(forecast_dates)){
        
        #Generate forecast
        forecast_chla = intercept + ar1 * (ic_distribution - chla_mean) + chla_mean + process_distribution
        forecast_chla = ifelse(forecast_chla < 0, 0, forecast_chla)
        
        #Select current row of forecast_data to see if there is data to use for updating
        new_obs <- forecast_data$chla[i] #Observed chl-a
        
        #Update the initial condition
        ic_update <- EnKF(forecast = forecast_chla, new_observation = new_obs, ic_sd = ic_sd)
        
        #Assign the updated initial condition to be used for the next day's forecast
        ic_distribution <- ic_update
        
        #Build temporary data frame to hold current initial condition and forecast
        temp <- tibble(date = rep(forecast_dates[i], times = n_members*2),
                       chla = c(forecast_chla, ic_update),
                       ensemble_member = rep(1:n_members, times = 2),
                       data_type = rep(c("fc","ic"), each = n_members))
        
        #Update rows of forecast series output data frame
        forecast_series <- forecast_series %>%
          rows_update(temp, by = c("date","ensemble_member","data_type"))
      }
      
      forecast_means <- forecast_series %>%
        filter(data_type == "fc") %>%
        group_by(date) %>%
        summarize(forecast_mean = mean(chla, na.rm = TRUE))
      
      if(chla_assimilation_frequencies[j] == 11){
        forecast.series$none <- forecast_series
        forecast.series.data$none <- forecast_data
        forecast.means$none <- forecast_means
      } else if (chla_assimilation_frequencies[j] == 7){
        forecast.series$weekly <- forecast_series
        forecast.series.data$weekly <- forecast_data
        forecast.means$weekly <- forecast_means
      } else {
        forecast.series$daily <- forecast_series
        forecast.series.data$daily <- forecast_data
        forecast.means$daily <- forecast_means
      }
      
      progress$inc(message = "Running forecasts",
                   detail = "This may take a while. This window will disappear
                     when forecasts are complete.", amount = 0.2)
      
    }
    
    plot.forecast.series$none <- plot_many_forecasts(forecast_data = forecast.series.data$none, forecast_series = forecast.series$none)
    plot.forecast.series$weekly <- plot_many_forecasts(forecast_data = forecast.series.data$weekly, forecast_series = forecast.series$weekly)
    plot.forecast.series$daily <- plot_many_forecasts(forecast_data = forecast.series.data$daily, forecast_series = forecast.series$daily)
    
    progress$inc(message = "Running forecasts",
                 detail = "This may take a while. This window will disappear
                     when forecasts are complete.", amount = 0.1)
    
    plot.forecast.series.w.obs$none <- plot_many_forecasts_with_obs(forecast_data = forecast.series.data$none, forecast_series = forecast.series$none, observations = chla_observations)
    plot.forecast.series.w.obs$weekly <- plot_many_forecasts_with_obs(forecast_data = forecast.series.data$weekly, forecast_series = forecast.series$weekly, observations = chla_observations)
    plot.forecast.series.w.obs$daily <- plot_many_forecasts_with_obs(forecast_data = forecast.series.data$daily, forecast_series = forecast.series$daily, observations = chla_observations)
    
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
  })
  
  output$fc_series_no_da_plot <- renderPlot({ 
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate the forecast in Objective 4.")
    )
    validate(
      need(input$fc_series_no_da > 0,
           message = "Please click 'Run forecasts with no data assimilation'.")
    )
    
    if(input$show_obs == TRUE){
      p <- plot.forecast.series.w.obs$none
    } else {
      p <- plot.forecast.series$none
    }
    
    return(p)
    
  })
  
  # Download plot
  output$save_fc_series_no_da_plot <- downloadHandler(
    filename = function() {
      paste("Q41-Q44-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.forecast.series.w.obs$none, device = device)
    }
  )
  
  # calculate bias and rmse for no da 
  observe({
    
    output$out_bias2 <- renderUI({
      
      validate(
        need(!is.null(autocorrelation_data$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(model_fit_data$df),
             message = "Please fit an AR model in Objective 3.")
      )
      validate(
        need(!is.null(first_forecast$forecast_chla),
             message = "Please generate the forecast in Objective 4.")
      )
      validate(
        need(!is.null(forecast.means$none),
             message = "Please click 'Run forecasts with no data assimilation'.")
      )
      validate(
        need(input$calc_bias2 > 0,
             message = "Please click 'Calculate bias'.")
      )
      
      days_to_forecast = 10
      
      forecast_dates <- seq.Date(from = as.Date(first_forecast_dates$start_date), to = as.Date(first_forecast_dates$start_date) + days_to_forecast, by = 'days')
      
      chla_observations <- lake_data$df %>%
        filter(datetime %in% forecast_dates)
      
      bias <- round(mean(forecast.means$none$forecast_mean - chla_observations$chla, na.rm = TRUE),4) 

      out_bias <- paste("<b>","Bias: ",bias,"</b>", sep = "")
      
      HTML(paste(out_bias))
    })
    
  })
  
  # Text output for RMSE ----
  observe({
    output$out_rmse2 <- renderUI({
      
      validate(
        need(!is.null(autocorrelation_data$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(model_fit_data$df),
             message = "Please fit an AR model above.")
      )
      validate(
        need(!is.null(first_forecast$forecast_chla),
             message = "Please generate the forecast in Objective 4.")
      )
      validate(
        need(!is.null(forecast.means$none),
             message = "Please click 'Run forecasts with no data assimilation'.")
      )
      validate(
        need(input$calc_rmse2 > 0,
             message = "Please click 'Calculate RMSE'.")
      )
      
      days_to_forecast = 10
      
      forecast_dates <- seq.Date(from = as.Date(first_forecast_dates$start_date), to = as.Date(first_forecast_dates$start_date) + days_to_forecast, by = 'days')
      
      chla_observations <- lake_data$df %>%
        filter(datetime %in% forecast_dates)
      
      rmse <- round(sqrt(mean((forecast.means$none$forecast_mean - chla_observations$chla)^2, na.rm = TRUE)), 2)
      
      out_rmse <- paste("<b>","RMSE: ",rmse,"</b>", sep = "")
      
      HTML(paste(out_rmse))
    })
  })
  
  #### weekly data assimilation ----
  
  output$fc_series_weekly_plot <- renderPlot({ 
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate the forecast in Objective 4.")
    )
    validate(
      need(input$fc_series_weekly > 0,
           message = "Please click 'Run forecasts with weekly data assimilation'.")
    )
    
    if(input$show_obs2 == TRUE){
      p <- plot.forecast.series.w.obs$weekly
    } else {
      p <- plot.forecast.series$weekly
    }
    
    return(p)
    
  })
  
  # Download plot
  output$save_fc_series_weekly_plot <- downloadHandler(
    filename = function() {
      paste("Q45-Q48-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.forecast.series.w.obs$weekly, device = device)
    }
  )
  
  # calculate bias and rmse for weekly da
  observe({
    
    output$out_bias3 <- renderUI({
      
      validate(
        need(!is.null(autocorrelation_data$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(model_fit_data$df),
             message = "Please fit an AR model in Objective 3.")
      )
      validate(
        need(!is.null(first_forecast$forecast_chla),
             message = "Please generate the forecast in Objective 4.")
      )
      validate(
        need(!is.null(forecast.means$weekly),
             message = "Please click 'Run forecasts with no data assimilation' above.")
      )
      validate(
        need(input$calc_bias3 > 0,
             message = "Please click 'Calculate bias'.")
      )
      
      days_to_forecast = 10
      
      forecast_dates <- seq.Date(from = as.Date(first_forecast_dates$start_date), to = as.Date(first_forecast_dates$start_date) + days_to_forecast, by = 'days')
      
      chla_observations <- lake_data$df %>%
        filter(datetime %in% forecast_dates)
      
      bias <- round(mean(forecast.means$weekly$forecast_mean - chla_observations$chla, na.rm = TRUE),4) 
      
      out_bias <- paste("<b>","Bias: ",bias,"</b>", sep = "")
      
      HTML(paste(out_bias))
    })
    
  })
  
  # Text output for RMSE ----
  observe({
    output$out_rmse3 <- renderUI({
      
      validate(
        need(!is.null(autocorrelation_data$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(model_fit_data$df),
             message = "Please fit an AR model above.")
      )
      validate(
        need(!is.null(first_forecast$forecast_chla),
             message = "Please generate the forecast in Objective 4.")
      )
      validate(
        need(!is.null(forecast.means$weekly),
             message = "Please click 'Run forecasts with no data assimilation' above.")
      )
      validate(
        need(input$calc_rmse3 > 0,
             message = "Please click 'Calculate RMSE'.")
      )
      
      days_to_forecast = 10
      
      forecast_dates <- seq.Date(from = as.Date(first_forecast_dates$start_date), to = as.Date(first_forecast_dates$start_date) + days_to_forecast, by = 'days')
      
      chla_observations <- lake_data$df %>%
        filter(datetime %in% forecast_dates)
      
      rmse <- round(sqrt(mean((forecast.means$weekly$forecast_mean - chla_observations$chla)^2, na.rm = TRUE)), 2)
      
      out_rmse <- paste("<b>","RMSE: ",rmse,"</b>", sep = "")
      
      HTML(paste(out_rmse))
    })
  })
  
  #### daily data assimilation ----
  
  output$fc_series_daily_plot <- renderPlot({ 
    
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
      need(!is.null(first_forecast$forecast_chla),
           message = "Please generate the forecast in Objective 4.")
    )
    validate(
      need(input$fc_series_daily > 0,
           message = "Please click 'Run forecasts with daily data assimilation'.")
    )
    
    if(input$show_obs3 == TRUE){
      p <- plot.forecast.series.w.obs$daily
    } else {
      p <- plot.forecast.series$daily
    }
    
    return(p)
    
  })
  
  # Download plot
  output$save_fc_series_daily_plot <- downloadHandler(
    filename = function() {
      paste("Q49-Q52-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.forecast.series.w.obs$daily, device = device)
    }
  )
  
  # calculate bias and rmse for no da
  observe({
    
    output$out_bias4 <- renderUI({
      
      validate(
        need(!is.null(autocorrelation_data$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(model_fit_data$df),
             message = "Please fit an AR model in Objective 3.")
      )
      validate(
        need(!is.null(first_forecast$forecast_chla),
             message = "Please generate the forecast in Objective 4.")
      )
      validate(
        need(!is.null(forecast.means$daily),
             message = "Please click 'Run forecasts with no data assimilation' above.")
      )
      validate(
        need(input$calc_bias4 > 0,
             message = "Please click 'Calculate bias'.")
      )
      
      days_to_forecast = 10
      
      forecast_dates <- seq.Date(from = as.Date(first_forecast_dates$start_date), to = as.Date(first_forecast_dates$start_date) + days_to_forecast, by = 'days')
      
      chla_observations <- lake_data$df %>%
        filter(datetime %in% forecast_dates)
      
      bias <- round(mean(forecast.means$daily$forecast_mean - chla_observations$chla, na.rm = TRUE),4) 
      
      out_bias <- paste("<b>","Bias: ",bias,"</b>", sep = "")
      
      HTML(paste(out_bias))
    })
    
  })
  
  # Text output for RMSE ----
  observe({
    output$out_rmse4 <- renderUI({
      
      validate(
        need(!is.null(autocorrelation_data$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(model_fit_data$df),
             message = "Please fit an AR model above.")
      )
      validate(
        need(!is.null(first_forecast$forecast_chla),
             message = "Please generate the forecast in Objective 4.")
      )
      validate(
        need(!is.null(forecast.means$daily),
             message = "Please click 'Run forecasts with no data assimilation' above.")
      )
      validate(
        need(input$calc_rmse4 > 0,
             message = "Please click 'Calculate RMSE'.")
      )
      
      days_to_forecast = 10
      
      forecast_dates <- seq.Date(from = as.Date(first_forecast_dates$start_date), to = as.Date(first_forecast_dates$start_date) + days_to_forecast, by = 'days')
      
      chla_observations <- lake_data$df %>%
        filter(datetime %in% forecast_dates)
      
      rmse <- round(sqrt(mean((forecast.means$daily$forecast_mean - chla_observations$chla)^2, na.rm = TRUE)), 2)
      
      out_rmse <- paste("<b>","RMSE: ",rmse,"</b>", sep = "")
      
      HTML(paste(out_rmse))
    })
  })
  
  #### Objective 8 ----
  
  #create reactive for forecast series plots
  forecast.scenario.data <- reactiveValues(weekly=NULL,
                                         daily=NULL)
  forecast.scenario <- reactiveValues(weekly=NULL,
                                    daily=NULL)
  forecast.scenario.means <- reactiveValues(weekly=NULL,
                                   daily=NULL)
  plot.forecast.scenario <- reactiveValues(weekly=NULL,
                                         daily=NULL)
  plot.forecast.scenario.w.obs <- reactiveValues(weekly=NULL,
                                               daily=NULL)
  
  #run all the forecasts
  observe({

    validate(
      need(input$fc_scenario_weekly > 0,
           message = "Please click 'Run forecasts with weekly data assimilation'.")
    )
    
    progress <- shiny::Progress$new()
    progress_value = 0.2
    progress$set(message = "Running forecasts",
                 detail = "This may take a while. This window will disappear
                     when forecasts are complete.", value = progress_value)
    
    #get data
    lake_data <- read_csv("./data/neon/BARC_chla_microgramsPerLiter.csv", show_col_types = FALSE) %>%
      rename(datetime = Date, chla = V1) %>%
      filter(cumsum(!is.na(chla)) > 0) %>%
      mutate(chla = ifelse(chla < 0, 0, chla))
    
    forecast_start_date <- "2020-09-25"
    forecast_scenario_start_date <- "2018-10-04"
    
    model_data <- lake_data %>%
      filter(datetime < forecast_start_date) %>%
      mutate(chla = na.approx(chla, na.rm = F)) %>% 
      mutate(chla_lag = lag(chla)) %>%
      filter(complete.cases(.))
    
    #fit model
    ar_model <- ar.ols(model_data$chla, order.max = 1, aic = FALSE,
                       intercept = TRUE, demean = TRUE)
    intercept = c(ar_model$x.intercept)
    ar1 = c(ar_model$ar)
    chla_mean = c(ar_model$x.mean)
    mod <- intercept + ar1 * (model_data$chla - chla_mean) + chla_mean
    residuals <- mod - model_data$chla
    n_members = 500
    
    #get ic uncertainty
    high_frequency_data <- read_csv("./data/BARC_chla_microgramsPerLiter_highFrequency.csv", show_col_types = FALSE) %>%
      mutate(date = date(datetime),
             time = hms::as_hms(datetime)) %>%
      filter(date >= "2019-10-09" & date <= "2019-10-12")
    ic_sd_dataframe <- high_frequency_data %>%
      group_by(date) %>%
      summarize(daily_sd_chla = sd(chla, na.rm = TRUE))
    ic_sd <- mean(ic_sd_dataframe$daily_sd_chla, na.rm = TRUE)
    curr_chla <- lake_data %>%
      filter(datetime == forecast_scenario_start_date) %>%
      pull(chla)
    ic_distribution_og <- rnorm(n = n_members, mean = curr_chla, sd = ic_sd)
    
    #get process uncertainty
    sigma <- sd(residuals, na.rm = TRUE) 
    process_distribution <- rnorm(n = 500, mean = 0, sd = sigma)
    
    #forecast set-up
    days_to_forecast = 7
    
    forecast_dates <- seq.Date(from = as.Date(forecast_scenario_start_date), to = as.Date(forecast_scenario_start_date) + days_to_forecast, by = 'days')    
    
    chla_observations <- lake_data %>%
      filter(datetime %in% forecast_dates)
    
    chla_assimilation_frequencies <- c(7,1)
    
    for(j in 1:length(chla_assimilation_frequencies)){
      
      chla_assimilation_dates <- forecast_dates[seq(1, length(forecast_dates), chla_assimilation_frequencies[j])]
      
      forecast_data <- lake_data %>%
        select(datetime, chla) %>%
        mutate(datetime = as.Date(datetime)) %>%
        filter(datetime %in% forecast_dates) %>%
        mutate(chla = ifelse(datetime %in% chla_assimilation_dates,chla,NA)) 
      
      forecast_series <- tibble(date = rep(forecast_dates, each = n_members*2),
                                chla = NA_real_,
                                ensemble_member = rep(1:n_members, times = length(forecast_dates)*2),
                                data_type = rep(c("fc","ic"), each = n_members, times = length(forecast_dates)))
      
      ic_distribution = ic_distribution_og
      
      temp_ic <- tibble(date = rep(forecast_dates[1], each = n_members),
                        chla = ic_distribution,
                        ensemble_member = c(1:n_members),
                        data_type = rep("ic", times = n_members))
      
      forecast_series <- forecast_series %>%
        rows_update(temp_ic, by = c("date","ensemble_member","data_type"))
      
      for(i in 2:length(forecast_dates)){
        
        #Generate forecast
        forecast_chla = intercept + ar1 * (ic_distribution - chla_mean) + chla_mean + process_distribution
        forecast_chla = ifelse(forecast_chla < 0, 0, forecast_chla)
        
        #Select current row of forecast_data to see if there is data to use for updating
        new_obs <- forecast_data$chla[i] #Observed chl-a
        
        #Update the initial condition
        ic_update <- EnKF(forecast = forecast_chla, new_observation = new_obs, ic_sd = ic_sd)
        
        #Assign the updated initial condition to be used for the next day's forecast
        ic_distribution <- ic_update
        
        #Build temporary data frame to hold current initial condition and forecast
        temp <- tibble(date = rep(forecast_dates[i], times = n_members*2),
                       chla = c(forecast_chla, ic_update),
                       ensemble_member = rep(1:n_members, times = 2),
                       data_type = rep(c("fc","ic"), each = n_members))
        
        #Update rows of forecast series output data frame
        forecast_series <- forecast_series %>%
          rows_update(temp, by = c("date","ensemble_member","data_type"))
      }
      
      forecast_means <- forecast_series %>%
        filter(data_type == "fc") %>%
        group_by(date) %>%
        summarize(forecast_mean = mean(chla, na.rm = TRUE))
      
      if (chla_assimilation_frequencies[j] == 7){
        forecast.scenario$weekly <- forecast_series
        forecast.scenario.data$weekly <- forecast_data
        forecast.scenario.means$weekly <- forecast_means
      } else {
        forecast.scenario$daily <- forecast_series
        forecast.scenario.data$daily <- forecast_data
        forecast.scenario.means$daily <- forecast_means
      }
      
      progress$inc(message = "Running forecasts",
                   detail = "This may take a while. This window will disappear
                     when forecasts are complete.", amount = 0.2)
      
    }
    
    plot.forecast.scenario$weekly <- plot_scenario_forecasts(forecast_data = forecast.scenario.data$weekly, forecast_series = forecast.scenario$weekly)
    plot.forecast.scenario$daily <- plot_scenario_forecasts(forecast_data = forecast.scenario.data$daily, forecast_series = forecast.scenario$daily)
    
    progress$inc(message = "Running forecasts",
                 detail = "This may take a while. This window will disappear
                     when forecasts are complete.", amount = 0.1)
    
    plot.forecast.scenario.w.obs$weekly <- plot_scenario_forecasts(forecast_data = forecast.scenario.data$weekly, forecast_series = forecast.scenario$weekly, show_final_obs = TRUE)
    plot.forecast.scenario.w.obs$daily <- plot_scenario_forecasts(forecast_data = forecast.scenario.data$daily, forecast_series = forecast.scenario$daily, show_final_obs = TRUE)
    
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
  })
  
  #plot with weekly data assimilation
  output$fc_scenario_weekly_plot <- renderPlot({ 
    
    validate(
      need(input$fc_scenario_weekly > 0,
           message = "Please click 'Run forecasts with weekly data assimilation'.")
    )
    
    p <- plot.forecast.scenario$weekly
    
    return(p)
    
  })
  
  # Download plot
  output$save_fc_scenario_weekly_plot <- downloadHandler(
    filename = function() {
      paste("Q55-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.forecast.scenario$weekly, device = device)
    }
  )
  
  #plot with daily data assimilation
  output$fc_scenario_daily_plot <- renderPlot({ 
    
    validate(
      need(input$fc_scenario_daily > 0,
           message = "Please click 'Run forecasts with daily data assimilation'.")
    )
    
    p <- plot.forecast.scenario$daily
    
    return(p)
    
  })
  
  # Download plot
  output$save_fc_scenario_daily_plot <- downloadHandler(
    filename = function() {
      paste("Q56-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.forecast.scenario$daily, device = device)
    }
  )
  
  #compare forecast results
  #plot with weekly data assimilation
  output$fc_scenario_weekly_plot2 <- renderPlot({ 
    
    validate(
      need(input$fc_compare > 0,
           message = "Please click 'Compare forecasts made with and without high-frequency sensor'.")
    )
    
    p <- plot.forecast.scenario.w.obs$weekly
    
    return(p)
    
  })
  
  output$fc_scenario_daily_plot2 <- renderPlot({ 
    
    validate(
      need(input$fc_compare > 0,
           message = "Please click 'Compare forecasts made with and without high-frequency sensor'.")
    )
    
    p <- plot.forecast.scenario.w.obs$daily
    
    return(p)
    
  })
  
  #calculate forecast assessment metrics
  # calculate bias and rmse for weekly da
    
    output$out_bias5 <- renderUI({
      
      validate(
        need(input$calc_bias5 > 0,
             message = "Please click 'Calculate bias'.")
      )
      
      #get data
      lake_data <- read_csv("./data/neon/BARC_chla_microgramsPerLiter.csv", show_col_types = FALSE) %>%
        rename(datetime = Date, chla = V1) %>%
        filter(cumsum(!is.na(chla)) > 0) %>%
        mutate(chla = ifelse(chla < 0, 0, chla))
      
      days_to_forecast = 7
      forecast_scenario_start_date <- "2018-10-04"
      
      forecast_dates <- seq.Date(from = as.Date(forecast_scenario_start_date), to = as.Date(forecast_scenario_start_date) + days_to_forecast, by = 'days')
      
      chla_observations <- lake_data %>%
        filter(datetime %in% forecast_dates)
      
      bias <- round(mean(forecast.scenario.means$weekly$forecast_mean - chla_observations$chla, na.rm = TRUE),4) 
      
      out_bias <- paste("<b>","Bias: ",bias,"</b>", sep = "")
      
      HTML(paste(out_bias))
    })
  
  
  # Text output for RMSE ----
    output$out_rmse5 <- renderUI({
      
      validate(
        need(input$calc_rmse5 > 0,
             message = "Please click 'Calculate RMSE'.")
      )
      
      #get data
      lake_data <- read_csv("./data/neon/BARC_chla_microgramsPerLiter.csv", show_col_types = FALSE) %>%
        rename(datetime = Date, chla = V1) %>%
        filter(cumsum(!is.na(chla)) > 0) %>%
        mutate(chla = ifelse(chla < 0, 0, chla))
      
      days_to_forecast = 7
      forecast_scenario_start_date <- "2018-10-04"
      
      forecast_dates <- seq.Date(from = as.Date(forecast_scenario_start_date), to = as.Date(forecast_scenario_start_date) + days_to_forecast, by = 'days')
      
      chla_observations <- lake_data %>%
        filter(datetime %in% forecast_dates)
      
      rmse <- round(sqrt(mean((forecast.scenario.means$weekly$forecast_mean - chla_observations$chla)^2, na.rm = TRUE)), 2)
      
      out_rmse <- paste("<b>","RMSE: ",rmse,"</b>", sep = "")
      
      HTML(paste(out_rmse))
    })
    
    # calculate bias and rmse for daily da
    
    output$out_bias6 <- renderUI({
      
      validate(
        need(input$calc_bias6 > 0,
             message = "Please click 'Calculate bias'.")
      )
      
      #get data
      lake_data <- read_csv("./data/neon/BARC_chla_microgramsPerLiter.csv", show_col_types = FALSE) %>%
        rename(datetime = Date, chla = V1) %>%
        filter(cumsum(!is.na(chla)) > 0) %>%
        mutate(chla = ifelse(chla < 0, 0, chla))
      
      days_to_forecast = 7
      forecast_scenario_start_date <- "2018-10-04"
      
      forecast_dates <- seq.Date(from = as.Date(forecast_scenario_start_date), to = as.Date(forecast_scenario_start_date) + days_to_forecast, by = 'days')
      
      chla_observations <- lake_data %>%
        filter(datetime %in% forecast_dates)
      
      bias <- round(mean(forecast.scenario.means$daily$forecast_mean - chla_observations$chla, na.rm = TRUE),4) 
      
      out_bias <- paste("<b>","Bias: ",bias,"</b>", sep = "")
      
      HTML(paste(out_bias))
    })
    
    
    # Text output for RMSE ----
    output$out_rmse6 <- renderUI({
      
      validate(
        need(input$calc_rmse6 > 0,
             message = "Please click 'Calculate RMSE'.")
      )
      
      #get data
      lake_data <- read_csv("./data/neon/BARC_chla_microgramsPerLiter.csv", show_col_types = FALSE) %>%
        rename(datetime = Date, chla = V1) %>%
        filter(cumsum(!is.na(chla)) > 0) %>%
        mutate(chla = ifelse(chla < 0, 0, chla))
      
      days_to_forecast = 7
      forecast_scenario_start_date <- "2018-10-04"
      
      forecast_dates <- seq.Date(from = as.Date(forecast_scenario_start_date), to = as.Date(forecast_scenario_start_date) + days_to_forecast, by = 'days')
      
      chla_observations <- lake_data %>%
        filter(datetime %in% forecast_dates)
      
      rmse <- round(sqrt(mean((forecast.scenario.means$daily$forecast_mean - chla_observations$chla)^2, na.rm = TRUE)), 2)
      
      out_rmse <- paste("<b>","RMSE: ",rmse,"</b>", sep = "")
      
      HTML(paste(out_rmse))
    })
  
  

  
  ##########OLD
  
  
    #### Navigating Tabs ----
    
    # Navigating Tabs ----
    #* Main Tab ====
    rv1 <- reactiveValues(prev = 0, nxt = 2)
    observeEvent(input$maintab, {
      curr_tab1 <- input$maintab
      rv1$prev <- readr::parse_number(curr_tab1) - 1
      rv1$nxt <- readr::parse_number(curr_tab1) + 1
    })
    
    observe({
      
      toggleState(id = "prevBtn1", condition = rv1$prev > 0)
      if(rv1$nxt > 6 & rv3a$nxt > 8) {
        shinyjs::disable("nextBtn1")
      } else {
        shinyjs::enable("nextBtn1")
      }
      hide(selector = ".page")
    })
    
    
    # Next button
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
      if(curr_tab1 == "mtab6") {
        curr_obj <- input$tabseries3
        idx2 <- which(tab_names$tab_id == curr_obj)
        new_nam <- tab_names$name[idx2 + 1]    } 
      if(curr_tab1 == "mtab6" & rv3a$nxt > 8) {
        updateActionButton(session, inputId = "nextBtn1", label = paste("End of module"))
      } else {
        updateActionButton(session, inputId = "nextBtn1", label = paste(new_nam, ">"))
      }
    })
    
    # Previous button
    observe({
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
      new_nam <- tab_names$name[idx - 1]
      
      if (curr_tab1 == "mtab4") {
        curr_obj <- input$tabseries1
        idx2 <- which(tab_names$tab_id == curr_obj)
        if(curr_obj == "obj1") idx2 <- idx2 - 1 # Move off Activty A label
        new_nam <- tab_names$name[idx2 - 1]
      }
      if (curr_tab1 == "mtab5") {
        curr_obj <- input$tabseries2
        idx2 <- which(tab_names$tab_id == curr_obj)
        if(curr_obj == "obj5") idx2 <- idx2 - 1 # Move off Activty B label
        new_nam <- tab_names$name[idx2 - 1]
      }
      if (curr_tab1 == "mtab6") {
        curr_obj <- input$tabseries3
        idx2 <- which(tab_names$tab_id == curr_obj)
        if(curr_obj == "obj8") idx2 <- idx2 - 1 # Move off Activty C label
        new_nam <- tab_names$name[idx2 - 1]
      }
      if(curr_tab1 == "mtab1") {
        updateActionButton(session, inputId = "prevBtn1", label = paste("Module begins"))
      } else {
        # shinyjs::show(id = "prevBtn1")
        updateActionButton(session, inputId = "prevBtn1", label = paste("<", new_nam))
      }
    })
    
    
    # Advancing Tabs
    observeEvent(input$nextBtn1, {
      
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
      if (curr_tab1 == "mtab4" & rv1a$nxt < 5) {
        curr_obj <- input$tabseries1
        
        updateTabsetPanel(session, "tabseries1",
                          selected = paste0("obj", rv1a$nxt))
        
      } else if (curr_tab1 == "mtab5" & rv2a$nxt < 8) {
        curr_obj <- input$tabseries2
        updateTabsetPanel(session, "tabseries2",
                          selected = paste0("obj", rv2a$nxt))
      } else if (curr_tab1 == "mtab6" & rv3a$nxt < 9) {
        curr_obj <- input$tabseries2
        updateTabsetPanel(session, "tabseries3",
                          selected = paste0("obj", rv3a$nxt))
      } else {
        updateTabsetPanel(session, "tabseries1",
                          selected = "obj1")
        updateTabsetPanel(session, "tabseries2",
                          selected = "obj5")
        updateTabsetPanel(session, "tabseries3",
                          selected = "obj8")
        updateTabsetPanel(session, "maintab",
                          selected = paste0("mtab", rv1$nxt))
      }
      shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
    })
    
    # Moving back through tabs
    observeEvent(input$prevBtn1, {
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
      if (curr_tab1 == "mtab4" & rv1a$prev > 0) {
        curr_obj <- input$tabseries1
        
        updateTabsetPanel(session, "tabseries1",
                          selected = paste0("obj", rv1a$prev))
        
      } else if (curr_tab1 == "mtab5" & rv2a$prev > 4) {
        curr_obj <- input$tabseries2
        updateTabsetPanel(session, "tabseries2",
                          selected = paste0("obj", rv2a$prev))
      } else if (curr_tab1 == "mtab6" & rv3a$prev > 7) {
        curr_obj <- input$tabseries3
        updateTabsetPanel(session, "tabseries3",
                          selected = paste0("obj", rv3a$prev))
      } else {
        updateTabsetPanel(session, "tabseries1",
                          selected = "obj4")
        updateTabsetPanel(session, "tabseries2",
                          selected = "obj7")
        updateTabsetPanel(session, "tabseries3",
                          selected = "obj8")
        updateTabsetPanel(session, "maintab",
                          selected = paste0("mtab", rv1$prev))
      }
      shinyjs::runjs("window.scrollTo(0, 0)")
      
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
  
  # Help buttons ----
  observeEvent(input$help, {
    introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
  })
  observeEvent(input$help2, {
    shinyalert(title = "Resume Progress", text = "Use this field to upload your '.eddie' file to resume your progress.", type = "info")
  })
  
  # Bookmarking
  # use this to check inputs if need to update bookmarking
  # observe({
  #   list_of_inputs <<- reactiveValuesToList(input)
  # })
  # inp <- unlist(names(list_of_inputs))
  bookmarkingWhitelist <- c("plot_chla","plot_lag1","plot_lag2","calc_ac","plot_ac","plot_pacf","fit_model" ,"calc_bias",            
                            "calc_rmse","calc_proc_distrib","plot_high_freq","calc_ic_uc","fc1" ,"fc1_viz" ,             
                            "view_new_obs","update_ic","second_forecast_da","view_ic_no_da","second_forecast_no_da", "plot_low_ic" ,         
                            "plot_fc_low_obs_uc","plot_high_ic","plot_fc_high_obs_uc" , 
                            "fc_series_no_da","calc_bias2","calc_rmse2","fc_series_weekly","calc_bias3","calc_rmse3",           
                            "fc_series_daily","calc_bias4" ,"calc_rmse4"  ,"fc_scenario_weekly" ,"fc_scenario_daily","fc_compare",           
                            "calc_bias5","calc_rmse5", "calc_bias6" ,"calc_rmse6" ,"show_ic","show_obs",             
                            "show_obs2","show_obs3"  )

  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })

  ExcludedIDs <- reactiveVal(value = NULL)

  observe({
    toExclude <- setdiff(names(input), bookmarkingWhitelist)
    setBookmarkExclude(toExclude)
    ExcludedIDs(toExclude)
  })

  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$sel_row <- input$table01_rows_selected
  })

  # Read values from state$values when we restore
  onRestore(function(state) {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab4")
    updateTabsetPanel(session, "tabseries1",
                      selected = "obj1")
  })

  onRestored(function(state) {
    updateSelectizeInput(session, "row_num", selected = state$values$sel_row)
  })
  
    
    
  
  
  
  
  #** Render Report ----
  report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  report2 <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath
  
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
