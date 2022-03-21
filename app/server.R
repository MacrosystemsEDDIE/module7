
# Icons
neonIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28),
  Terrestrial = makeIcon("icons/mountain-icon.png", iconWidth =  28, iconHeight = 28)
)

# Slides for slickR
model_slides <- list.files("www/model_slides", full.names = TRUE)

# Reference for downloading variables
neon_vars <- read.csv("data/neon_variables.csv")
met_pars <- read.csv("data/met_params.csv", fileEncoding = "UTF-8-BOM")


shinyServer(function(input, output, session) {


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
                     when it is downloaded.", value = 0.33)
    #load NEON data and format for input into EnKF
    lake_data$df <- format_enkf_inputs(siteID = siteID$lab, neon_vars = neon_vars)

    progress$set(message = "Loading NOAA forecast data",
                 detail = "This may take a while. This window will disappear
                     when it is downloaded.", value = 0.67)
    noaa_fc$list <- load_noaa_forecast(siteID = siteID$lab, start_date = start_date)
    
    idx <- which(lake_data$df$Date == start_date)
    
    # Historical data
    df <- lake_data$df[(idx-7):(idx+1), ]
    df$chla[nrow(df)] <- NA
    df$Date <- as.Date(df$datetime)
    df$maxUptake <- as.numeric(NA)
    obs_plot$hist <- df
    
    # Future data
    df <- lake_data$df[(idx+1):(idx+35), ]
    df$Date <- as.Date(df$datetime)
    df$maxUptake <- as.numeric(NA)
    obs_plot$future <- df
    
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
  persist_df <- reactiveValues(df = NULL)
  airt_swt <- reactiveValues(df = NULL)
  wtemp_fc_data <- reactiveValues(lst = as.list(rep(NA, 4)), hist = NULL, fut = NULL)
  
  observeEvent(input$table01_rows_selected, {
    output$prompt2 <- renderText({
      "Click on the link below to find out more information about your site."
    })

    ref <- "Air temperature"
    x_var <- neon_vars$id[which(neon_vars$Short_name == ref)][1]
    x_units <- neon_vars$units[which(neon_vars$Short_name == ref)][1]
    x_file <- file.path("data", "neon", paste0(siteID$lab, "_", x_var, "_", x_units, ".csv"))
    validate(
      need(file.exists(x_file), message = paste0(ref, " is not available at this site."))
    )
    xvar <- read.csv(x_file)
    xvar[, 1] <- as.POSIXct(xvar[, 1], tz = "UTC")
    xvar$Date <- as.Date(xvar[, 1])
    xvar <- plyr::ddply(xvar, c("Date"), function(x) mean(x[, 2], na.rm = TRUE)) # Daily average - also puts everything on same timestamp


    ref2 <- "Surface water temperature"
    y_var <- neon_vars$id[which(neon_vars$Short_name == ref2)][1]
    y_units <- neon_vars$units[which(neon_vars$Short_name == ref2)][1]
    y_file <- file.path("data", "neon", paste0(siteID$lab, "_", y_var, "_", y_units, ".csv"))
    validate(
      need(file.exists(y_file), message = paste0(ref2, " is not available at this site."))
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
      need(nrow(df) > 0, message = "No variables at matching timesteps.")
    )
    df$month <- lubridate::month(df$Date)
    df <- df[(df$month %in% 5:10), 1:3]
    colnames(df)[-1] <- c("airt", "wtemp")
    airt_swt$df <- df
    df$Mod <- c(NA, df$wtemp[-nrow(df)])
    persist_df$df <- df


    dat <- data.frame(Date = airt_swt$df$Date, wtemp = airt_swt$df$wtemp, airt = airt_swt$df$airt)

    wtemp_fc_data$hist <- dat[dat$Date <= as.Date("2020-10-02") & dat$Date >= "2020-09-22", ]
    wtemp_fc_data$hist$wtemp[wtemp_fc_data$hist$Date > as.Date("2020-09-25")] <- NA
    wtemp_fc_data$hist$airt[wtemp_fc_data$hist$Date > as.Date("2020-09-25")] <- NA
    wtemp_fc_data$fut <- dat[dat$Date > as.Date("2020-09-25") & dat$Date <= "2020-10-02", ]
    wtemp_fc_data$fut$airt[wtemp_fc_data$fut$Date > as.Date("2020-09-25")] <- NA


  })

  # Read in site data ----
  neon_DT <- reactive({ # view_var
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
    df <- read.csv(file)
    df[, 1] <- as.POSIXct(df[, 1], tz = "UTC")
    df[, -1] <- signif(df[, -1], 4)
    names(df)[ncol(df)] <- read_var

    if(input$view_var == "Surface water temperature") {
      df <- df[df[, 2] == min(df[, 2], na.rm = TRUE), c(1, 3)] # subset to surface temperature
    }

    sel <- tryCatch(df[(selected$sel$pointNumber+1), , drop=FALSE] , error=function(e){NULL})


    return(list(data = df, sel = sel))
  })

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
  output$slck_model <- renderSlickR({
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
    if(is.null(selected2$sel)) {
      df <- wtemp_airtemp()$data
    } else {
      df <- selected2$sel[, 2:4]
    }
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
    formula <- "$$ wtemp = %s * airtemp + %s   ;   r^2 = %s $$"
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
    return(list(data = df, sel = sel))
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
    return(list(data = df, sel = sel))
  })
  
  lm_upar <- reactiveValues(sub = NULL, m = NULL, b = NULL, r2 = NULL, sigma = NULL)
  
  observeEvent(input$add_lm3, {
    if(is.null(selected3$sel)) {
      df <- swr_upar()$data
    } else {
      df <- selected3$sel[, 2:4]
    }
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
      formula <- "$$ uPAR = %s * SWR %s   ;   r^2 = %s $$"
    } else {
      formula <- "$$ uPAR = %s * SWR + %s   ;   r^2 = %s $$"
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
  
  #** Generate IC distribution ----
  observeEvent(input$gen_ic, {
    req(input$table01_rows_selected != "")
    req(!is.null(lake_data$df))
    mn_chla <- lake_data$df$chla[lake_data$df$Date == start_date]
    ic_dist$df <- data.frame(value = rnorm(1000, mn_chla, input$ic_uc))
  })
  
  #** Plot - IC distribution ----
  output$ic_uc_plot <- renderPlot({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(ic_dist$df), "Click 'Generate distribution")
    )
    df <- data.frame(x = lake_data$df$chla[lake_data$df$Date == start_date],
                     label = "Observed")
    
    xlims <- c(df$x -1.5, df$x + 1.5)
    ylims <- c(0,7)
    
    p <- ggplot() +
      geom_vline(xintercept = df$x) +
      geom_density(data = ic_dist$df, aes(value), fill = l.cols[2], alpha = 0.3) +
      ylab("Chlorophyll-a (μg/L)") +
      ylab("Density") +
      coord_cartesian(xlim = xlims, ylim = ylims) +
      theme_bw(base_size = 18)
    
    return(p)
  })
  
  #** Recent obs timeseries ----
  output$ic_obs_plot <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(!is.null(lm_wt$m) & !is.null(lm_upar$m),
           message = "Please prepare inputs in Objective 5.")
    )


    p <- ggplot()

    if(!is.null(ic_dist$df)) {
      quants <- quantile(ic_dist$df$value, c(0.25, 0.75))

      err_bar <- data.frame(x = as.Date(start_date), ymin = quants[1], ymax = quants[2])
      p <- p +
        geom_errorbar(data = err_bar, aes(x, ymin = ymin, ymax = ymax, width = 0.5))
    }

    p <- p +
      geom_point(data = obs_plot$hist, aes(Date, chla, color = "Chlorophyll-a")) +
      geom_vline(xintercept = as.Date(start_date), linetype = "dashed") +
      ylab("Chlorophyll-a (μg/L)") +
      xlab("Date") +
      scale_color_manual(values = c("Chlorophyll-a" = cols[2])) +
      theme_bw(base_size = 12) +
      theme(legend.position = "none")
    return(ggplotly(p, dynamicTicks = TRUE))
  })
  
  #** Run IC forecast ----
  est_out1 <- reactiveValues(out = NULL)
  observeEvent(input$run_fc1, {
    
    req(input$table01_rows_selected != "")
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Running forecast of chlorophyll-a",
                 detail = "This may take a while. This window will disappear
                     when it is downloaded.", value = 0.1)
    
    obs_file <- create_data_assim_inputs(freq_chla = 36,
                                         freq_din = 36,
                                         lake_data = lake_data$df,
                                         start_date = start_date)
    driver_file <- convert_forecast(lm_wt = lm_wt, lm_upar = lm_upar, noaa_fc = noaa_fc, start_date = start_date)
    
    #get initial conditions for forecast
    yini <- get_yini(lake_data = lake_data$df,
                     start_date = start_date)
    
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
                   obs_cv = c(0.01,0.05),#cv for chl-a and DIN, respectively
                   param_cv = 0.1,#for maxUptake
                   init_cond_cv = c(input$ic_uc, 0.1),#cv for chl-a and DIN, respectively
                   state_names = c("chla", "nitrate"),
                   yini = yini)
    
    progress$set(value = 0.9)
    out <- format_enkf_output(est_out = est_out, lake_data = lake_data$df)
    
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
      need(!is.null(est_out1$out),
           message = "Click 'Run forecast'.")
    )
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type1, est_out = est_out1$out, var = "chla")
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
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type1, est_out = est_out1$out, var = "nitrate")
  })

  #** Plot - FC1 - maxUptake UC ----
  output$maxUptake_fc1 <- renderPlotly({
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in Objective 1.")
    )
    validate(
      need(any(!is.null(est_out1$out)),
           message = "Click 'Run forecast'.")
    )
    plot_enkf_out(obs_plot = obs_plot, start_date = start_date, plot_type = input$plot_type1, est_out = est_out1$out, var = "maxUptake")
  })
  
})

# end
