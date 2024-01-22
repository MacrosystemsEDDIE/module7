ui <- function(req) {
  
  tagList( # Added functionality for not losing your settings
    # shinythemes::themeSelector(), # user-defined theme
    # Java to prompt the students to click a button
    # Java script https://community.rstudio.com/t/keeping-track-of-idle-time-during-app-usage/1735
    tags$script("
              (function() {
  var timeoutWarningMsecs = 12 * 60 * 1000;
  var idleTimer;

  function onTimeout() {
    alert('Warning: Session is about to time out! Please click a button to prevent losing progress.');
  }

  function startIdleTimer() {
    if (idleTimer) clearTimeout(idleTimer);
    idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
  }

  $(document).on('shiny:message shiny:inputchanged', startIdleTimer);

})();"),
    tags$style(type = "text/css", "text-align: justify"),
    tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
    tags$head(includeHTML(("google-analytics.html"))),
    fluidPage(
      column(10,
             br(),
             p(tags$b("Teaching materials associated with this module can be found at ",
                      tags$a(href="http://module7.macrosystemseddie.org", 
                             "http://module7.macrosystemseddie.org.", target="_blank")))
      )
    ),
    navbarPage(title = "Module 7: Using Data to Improve Ecological Forecasts",
               position = "static-top", id = "maintab",
               tags$header(
                 fluidRow(
                   column(11,
                          bookmarkButton(id = "bookmarkBtn", label = "Bookmark my progress"),
                          br(), 
                          p(tags$em("At any time, use this button to obtain a link that saves your progress."))
                   ),
                   column(1, align = "right",
                          introBox(
                            actionButton("help", label = "Help", icon = icon("question-circle")), data.step = 7, data.intro = help_text["help", 1]
                          )
                   )
                 )
               ),
               # 1. Overview ----
               tabPanel(introBox(tab_names["mtab1", 2],
                                 data.step = 2,
                                 data.intro = help_text["tab_nav1", 1]
                                 ),
               value = "mtab1",
               introjsUI(), # must include in UI
               introBox(
                 img(src = "project-eddie-banner-2020_green.png", height = 100,
                     width = 1544, top = 5),
                 data.step = 1,
                 data.intro = help_text["welcome", 1]
               ),
               withMathJax(), # NEEDS to be here for rendering eqn's in data.table
               
               tags$style(".btn-file {
             background-color:#98CAB2;
             border-color: #579277;
             }

             .progress-bar {
             background-color: #579277;
             }"),
               # Change progress bar color
               tags$style(paste0("
               .irs-grid-text { font-size: 10pt; }
                                   .irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: ", slider_col, ";
  border-color: ", slider_col, ";
}")),
               includeCSS("www/slider_cols.css"),
               tags$style(HTML("
               .irs-bar {
                        border-color: transparent;
                        background-color: transparent;
                        }
                        #first {
                        border: 4px double red;
                        }
                        #13a_graz {
                        margin-bottom: 10px;
                        }
                        #bla_border {
                        border: 2px solid black;
                        }
                        #bla_border2 {
                        border: 1px solid black;
                        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                        }
                        #txt_j {
                        text-align: justify;
                        }
                        #txt_c {
                        text-align: center;
                        }
                        #txt_l {
                        text-align: left;
                        }
                        #ackn {
                        color: gray;
                        font-size: 12px
                        }
                        #pheno img {
                        transition:transform 0.25s ease;
                        max-width: 100%; width: 100%; height: auto
                        }
                        #nextBtn1:hover {
                        background-color: yellow;
                        }
                        #dl_btn {
                        width:290px
                        }
                        #pheno:hover img{
    -webkit-transform:scale(1.5);
    transform:scale(1.5);
}
                        #wh_link a {
                        color: #FFFFFF
                        }
                        #q6_tab {
                        'border':'1px solid #ddd'
                        }
                        .box.box-solid.box-primary>.box-header {

                }

                .box.box-solid.box-primary{

                background:#B8E0CD
                }
                .box.box-solid.box-success{

                background: #DDE4E1;
                }
                .box.box-solid.box-warning>.box-header {

                }

                .box.box-solid.box-warning{

                background:#FFBE85
                }
                        ")),
               introBox(
                 fluidRow(
                   column(6,
                          #* Module text ====
                          h2("Using Data to Improve Ecological Forecasts"),
                          h3("Focal question"),
                          h4(tags$b(tags$i("How can we use data to improve ecological forecasts?"))),
                          h3("Summary"),
                          p("To be useful for management, ecological forecasts need to be both accurate enough for managers to be able to rely on them for decision-making",tags$i("and"), " include a representation of forecast uncertainty, so managers can properly interpret the probability of future events. To improve forecast accuracy, we can update forecasts with observational data once they become available, a process known as", tags$b("data assimilation.")," Recent improvements in environmental sensor technology and an increase in the number of sensors deployed in ecosystems have increased the availability of data for assimilation to develop and improve forecasts for natural resource management."),
                          p("In this module, you will explore how assimilating data with different amounts of observation uncertainty and at different temporal frequencies affects forecasts of lake water quality at an ecological site of your choice."),
                          h3("Learning Outcomes"),
                          tags$line(),
                          tags$ul(
                            tags$li(id = "txt_j", module_text["LO1", ]),
                            tags$li(id = "txt_j", module_text["LO2", ]),
                            tags$li(id = "txt_j", module_text["LO3", ]),
                            tags$li(id = "txt_j", module_text["LO4", ]),
                            tags$li(id = "txt_j", module_text["LO5", ])
                          )
                   ),
                   column(5, offset = 1,
                          br(), br(), br(),
                          img(src = "mod7_conceptual_figure.png", height = "80%",
                              width = "80%", align = "left")
                   )
                 ), data.step = 8, data.intro = help_text["start", 1]
               )
               ),
               
               # 2. Presentation recap ----
               tabPanel(title = tab_names["mtab2", 2], value = "mtab2",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          hr(),
                          column(4,
                                 h3("Presentation"),
                                 p("The presentation accompanying this module reviews the forecast cycle, introduces the process of using ecological data to improve forecasts (data assimilation), and explains how the amount of uncertainty in observations and model predictions as well as the frequency of observations affect data assimilation."),
                                 p(tags$b("What is data assimilation?")),
                                 tags$ul(
                                   tags$li(module_text["data_assimilation", ])
                                 ),
                                 p(tags$b("How does the amount of uncertainty in model predictions and data affect the process of data assimilation?")),
                                 tags$ul(
                                   tags$li(module_text["da_uncert", ])
                                 ),
                                 p(tags$b("How does the frequency of observations affect data assimilation?")),
                                 tags$ul(
                                   tags$li(module_text["da_freq", ])
                                 ),
                                 p(tags$i("Click through the slides to recap some of the main points from the lecture."))
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Key Slides",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("slides", width = "550px", height = "400px")
                                 )
                          )
                        )
               ),
               
               # 3. Introduction ----
               tabPanel(title = tab_names["mtab3", 2], value = "mtab3",
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(10, 
                                 h3("Workflow for this module"),
                                 tags$ol(
                                   tags$li(id = "txt_j", module_text["workflow1", ]),
                                   tags$li(id = "txt_j", module_text["workflow2", ]),
                                   tags$li(id = "txt_j", module_text["workflow3", ]),
                                   tags$li(id = "txt_j", module_text["workflow4", ])
                                 )
                          )
                        ), hr(),
                        fluidRow(
                          column(6, 
                                 h3("Student Handout"),
                                 p("Within the Introduction and Activities A, B and C tabs there are questions for students to complete as part of this module. These can be completed by writing your answers into the final report template, which can be downloaded as a Word document (.docx) below."),
                                 tags$style(type="text/css", "#stud_dl {background-color:#579277;color: white}"),
                                 conditionalPanel("output.handoutbuilt",
                                                  downloadButton(outputId = "stud_dl", label = "Download Final Report Template")
                                 )
                          ),
                          column(6,
                                 h3("Saving your progress"),
                                 p(style="text-align: justify;", "As you go, fill out answers to questions in the final report Word document. Some of the plots you generate in the Shiny app will be needed for the final report. When prompted, be sure to download these plots so you can copy-paste them into the final report."),
                                 p(style="text-align: justify;", "If you run out of time to finish all the activities you can save your progress and return to it at a later date. Click the 'Bookmark my progress' button at the top of the page and you will obtain a link, which you should save by copy-pasting it at the top of your final report. When you are ready to resume work, paste the link into your web browser, and it will load a Shiny app session that contains your progress."),
                                 br()
                          )
                        ),
                        hr(),
                        fluidRow(
                          hr(),
                          column(10, align = "left",
                                 box(id = "box1", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(8, offset = 1,
                                              h3("Before you start..."),
                                              p(id = "txt_j", "Download your final report (Word document) and input your name and Student ID. Then, answer the following questions in the final report."),
                                              introBox(
                                                h3(tags$b("Think about it!")),
                                                p(tags$b(quest["q1", 1])),
                                                p(tags$b(quest["q2", 1])),
                                                data.step = 5, data.intro = help_text["questions", 1]
                                                )
                                              )
                                       )
                                     )
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Data sources"),
                                 p(HTML(paste0('This module will introduce key concepts within Ecological forecasting through exploration of ', a(href = "https://www.neonscience.org/", "NEON (National Ecological Observatory Network) data", target = "_blank"), ", building a model, and then generating a short-term ecological forecast.")))
                          ),
                          column(6, align = "center",
                                 a(
                                   href = "https://www.neonscience.org/",
                                   img(src = "NSF-NEON-logo.png", title = "NEON - NSF logo"), target = "_blank"
                                 )
                          )
                        )
               ),
               
               # 4. Activity A ----
               tabPanel(title = tab_names["mtab5", 2], value = "mtab5",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity A - Build A Model And Generate A Forecast"),
                                           p(module_text["act_A", ])
                                 )
                          )
                        ),
                        tabsetPanel(id = "tabseries2",
                                    #* Objective 1 - Select site ====
                                    tabPanel(title = "Objective 1 - Select and view a NEON site", value = "obj1",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 1 - Select a Site"),
                                                                p(module_text["obj_01", ])
                                                      )
                                               )
                                             ),
                                             # data.step = 4, data.intro = help_text["objectives", 1], data.position = "top"),
                                             #** NEON Map ====
                                             fluidRow(
                                               #** NEON Intro ----
                                               column(4,
                                                      h2("Site Description"),
                                                      p("Select a site in the table to highlight on the map"),
                                                      conditionalPanel("input.row_num > 25",
                                                                       selectizeInput("row_num", "Select row",
                                                                                      choices = 1:nrow(neon_sites_df),
                                                                                      options = list(
                                                                                        placeholder = 'Please select a row',
                                                                                        onInitialize = I('function() { this.setValue(""); }'))
                                                                       )
                                                      ),
                                                      DTOutput("table01"),
                                                      p(tags$b("Click 'View latest photo' to see the latest image from the webcam on site (this may take 10-30 seconds).")),
                                                      actionButton("view_webcam", label = "View latest photo", icon = icon("eye"))
                                               ),
                                               #** Site map ----
                                               column(4,
                                                      h2("Map of NEON sites"),
                                                      wellPanel(
                                                        leafletOutput("neonmap")
                                                      )
                                               )
                                               
                                               ,
                                               #** Site photo ----
                                               column(4,
                                                      h2("Phenocam"),
                                                      # textOutput("prompt1"),
                                                      wellPanel(
                                                        imageOutput("pheno"),
                                                        p(id = "txt_j", module_text["phenocam", ])
                                                      )
                                               )
                                             ), br(),
                                             #      span(textOutput("site_name1"), style = "font-size: 22px;
                                             # font-style: bold;"),
                                             fluidRow(
                                               wellPanel(
                                                 h4(tags$b("About Site")),
                                                 uiOutput("site_html"),
                                                 textOutput("prompt2"),
                                                 htmlOutput("site_link")
                                               )
                                             ),
                                             fluidRow(
                                               column(10, align = "left",
                                                      box(id = "box3", width = 10, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(7, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q3", 1])),
                                                                   p("If the information for your lake is not on the NEON website then you can put NA (Not Available) as your answer.")
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   p(tags$em(quest["q3a", 1] , width = "90%")),
                                                                   p(tags$em(quest["q3b", 1], width = "90%")),
                                                                   p(tags$em(quest["q3c", 1], width = "90%"))
                                                            ),
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   p(tags$em(quest["q3d", 1] , width = "90%")),
                                                                   p(tags$em(quest["q3e", 1], width = "90%")),
                                                                   p(tags$em(quest["q3f", 1], width = "90%"))
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #*** Next step ----
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will explore the chlorophyll-a data which have been collected at this site by NEON."))
                                             )
                                    ),
                                    #* Objective 2 - Explore data ====
                                    tabPanel(title = "Objective 2 - Explore chlorophyll-a", value = "stab5",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 2 - Explore chlorophyll-a"),
                                                                p(id = "txt_j", module_text["obj_02", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Chlorophyll-a in lakes"),
                                                      p(tags$b("Chlorophyll-a")," concentrations are an indicator of algal (phytoplankton) abundance and biomass in a lake. Phytoplankton are important primary producers at the base of the lake food web, and are therefore necessary for healthy lake ecosystem function. However, an overabundance of phytoplankton can lead to harmful ",tags$b("blooms.")),
                                                      p("Blooms compromise water quality via unsightly scums, clogging of filters at water treatment plants, release of noxious taste and odor compounds, and in some cases release of toxins that pose substantial risk to human and animal health."),
                                                      p("Forecasts of chlorophyll-a concentrations days to weeks into the future can give water managers important information about the likelihood of a bloom event. This permits pre-emptive management to prevent or mitigate water quality concerns caused by blooms.")
                                                      ),
                                               column(8,
                                                      img(src = "hab_Erie.jpg", height = "100%", id = "bla_border",
                                                          width = "100%", tags$style("border: solid 2px black;")),
                                                      br(),
                                                      br(),
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Question"),
                                                                   p(tags$b(quest["q4", 1], width = "90%"))
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #* Time series plot ----
                                             fluidRow(
                                               column(5,
                                                      h3("Explore chlorophyll-a"),
                                                      p(id = "txt_j", "Click 'Plot chlorophyll-a' to view a time series of the real chlorophyll-a data measured at the lake you chose."),
                                                      p(tags$em("Most plots throughout the app are interactive; hover on the plot to see the various options for manipulating the plot that will appear as a menu in the top right corner")),
                                                      p(tags$b("Note that gaps in chlorophyll-a data are normal, as sensor buoys cannot always be left in lakes through the winter in cold climates.")),
                                                      fluidRow(
                                                        column(12, align = "right",
                                                               actionButton("plot_chla", "Plot chlorophyll-a")
                                                        )
                                                      ),
                                                      br(),
                                                      box(id = "box12", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h4("Questions"),
                                                                   p(tags$b(quest["q5", 1]))
                                                            )
                                                          ),
                                                          br()
                                                      ),
                                                      hr(),
                                                      fluidRow(
                                                        br()
                                                      )
                                               ),
                                               column(7,
                                                      h4("Time series of chlorophyll-a"),
                                                      wellPanel(
                                                        plotlyOutput("chla_plot")
                                                      ),
                                                      downloadButton("save_chla_plot", "Download plot", icon = icon("download"))
                                               )
                                             ),
                                             hr(),
                                             #*** Next step ----
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we fit a model to the chlorophyll-a data from your lake that we can use for forecasting.")
                                               )
                                             )
                                    ),
                                    #* Objective 3 - Fit model ====
                                    tabPanel(title = "Objective 3 - Fit model", value = "stab6",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 3 - Fit model"),
                                                                p(id = "txt_j", module_text["obj_03", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Before we fit our model to data..."),
                                                      p(tags$em("Use the slides and text below to understand the forecast model we will be using.")),
                                                      p(tags$b("What is an autoregressive model?")),
                                                      tags$ul(
                                                        tags$li("An ",tags$b("autoregressive model"), " uses past and/or current values of a variable to predict future values. In our case, we are interested in using past and current values of lake chlorophyll-a to predict future chlorophyll-a.")
                                                      ),
                                                      p(tags$b("What is autocorrelation?")),
                                                      tags$ul(
                                                        tags$li(tags$b("Autocorrelation")," is the correspondence between a value and previous values of that variable which have been recently observed. For example, mean daily air temperature over the course of a year exhibits autocorrelation, as today's mean daily air temperature is related to the mean daily air temperatures observed over the days and weeks prior to today.")
                                                      ),
                                                      p(tags$b("What is a lag?")),
                                                      tags$ul(
                                                        tags$li("A ",tags$b("lag")," is a particular amount of time that has passed between when we observe a value we are using as an explanatory, or independent, variable, and when we observe a value that we are trying to predict. For example, if you use today's air temperature to predict tomorrow's air temperature, you are using a 1-day lag of air temperature to predict tomorrow's air temperature. ")
                                                      )
                                               ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("model_slides", width = "640px", height = "480px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(10, align = "left",
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q6", 1]))                                                                   
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Plot 1-day lag of chlorophyll-a"),
                                                      p(id = "txt_j", "Let's explore lags and autocorrelation in chl-a data at your chosen lake site."),
                                                      p(id = "txt_j", "To make it easier to see the 1-day lag in chlorophyll-a on the figure, we will only plot a few months of data."),
                                                      br(),
                                                      actionButton("plot_lag1", "Plot lagged timeseries"),
                                                      br(), br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q7", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("lag_plot1")
                                                      ),
                                                      downloadButton("save_lag_plot1", "Download plot", icon = icon("download"))
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      p(id = "txt_j", "To visualize the relationship between chlorophyll and a 1 day lag of chlorophyll in a different way, we will also plot these two timeseries on a scatterplot. The dashed diagonal line represents the 1:1 line. The closer the points fall to this line, the stronger of the linear relationship between the independent variable (x axis) and the dependent variable (y axis)."),
                                                      p(id = "txt_j", "Note that now, we are plotting the complete dataset."),
                                                      br(),
                                                      actionButton("plot_lag2", "Plot lag scatterplot"),
                                                      br(), br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q8", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("lag_plot2")
                                                      ),
                                                      downloadButton("save_lag_plot2", "Download plot", icon = icon("download"))
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Calculating autocorrelation"),
                                                      p(id = "txt_j", "In addition to visualizing autocorrelation, we can also calculate it. The autocorrelation between chlorophyll-a",tags$em("(Chla)"), " and a 1-day lag of chlorophyll-a ",tags$em("(ChlaLag)")," is represented by the following equation:"),
                                                      wellPanel(
                                                        h4("Autocorrelation:"),
                                                        div("$$Autocorrelation = \\frac {\\sum_{t = 2}^{T} (Chla - \\overline{Chla}) * (ChlaLag - \\overline{Chla})}{\\sum_{t = 1}^{T} (Chla - \\overline{Chla})^2}$$"),
                                                        p("where", tags$em("T")," is the timeseries of observations in the timeseries, and ",tags$em("t"), " represents which observation in that timeseries we are starting with (either the 1st or 2nd observation). Recall that the capital sigma (\\(\\sum_{}\\)) indicates a sum and the overline (\\(\\overline{Chla}\\)) indicates the mean.")
                                                      ),
                                                      br(),
                                                      actionButton("calc_ac", "Calculate autocorrelation"),
                                                      br(),br(),
                                                      wellPanel(
                                                        htmlOutput("out_ac"),
                                                        p("The closer the autocorrelation is to 1, the stronger the autocorrelation between the variable and its lag.")
                                                      ),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q9", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      h3("Plotting autocorrelation"),
                                                      p("Next, we can calculate and plot the autocorrelation values for many different lags of our chlorophyll-a data."),
                                                      actionButton("plot_ac", label = "Plot autocorrelation for many lags"),
                                                      br(),br(),
                                                      wellPanel(
                                                        plotlyOutput("ac_plot"),
                                                      ),
                                                      downloadButton("save_ac_plot", "Download plot", icon = icon("download")),
                                                      br(),br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q10", 1])),
                                                                   p(tags$b(quest["q11", 1]))
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Partial autocorrelation"),
                                                      p("As you may have discovered while answering Q.11, it can be difficult to decide exactly how many lags to include in a forecasting model. Fortunately, forecasters have developed tools to help make this decision. One such tool is the ", tags$b("partial autocorrelation function"), " or ", tags$b("PACF.")," This function calculates the autocorrelation of a particular lag ",tags$em("while removing")," the effects of indirect correlations with other lags."),
                                                      p("To explain another way: the ",tags$b("autocorrelation")," of chlorophyll-a and the 7-day lag of chlorophyll-a is affected by the autocorrelation of chlorophyll-a with the 1-day lag, the 2-day lag, the 3-day lag, and so on, as well as the relationship of the 7-day lag to the 1-day lag, the 2-day lag, the 3-day lag, and so on."),
                                                      p("The PACF avoids this problem. You can think of it as only measuring the effect of one particular set of lagged values (e.g., the 5-day lagged values), while accounting for (and thereby removing the influence of) all other lags."),
                                                      p("Let's plot the PACF of chlorophyll-a data at your chosen lake site."),
                                                      actionButton("plot_pacf",label = "Plot PACF"),
                                                      br(),br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q12", 1])),
                                                                   p(tags$b(quest["q13", 1])),
                                                                   p(tags$b(quest["q14", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("pacf_plot"),
                                                      ),
                                                      downloadButton("save_pacf_plot", "Download plot", icon = icon("download"))
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Fit model!"),
                                                      p("Today, we will fit a simple form of an autoregressive, or AR model, which uses yesterday's chlorophyll-a observation (so, a 1-day lag) to predict today's observation. This model can be written as:"),
                                                      wellPanel(
                                                        div("$$Chla_{t} = \\beta_0 + \\beta_1 * (Chla_{t-1} - \\overline{Chla}) + \\overline{Chla}$$"),
                                                        p("where ",tags$em("Chla")," is our timeseries of chlorophyll-a data, \\(\\beta_0\\) is the intercept parameter, \\(\\beta_1\\) is the coefficient on the 1-day lag of chlorophyll-a, and \\(\\overline{Chla}\\) is the mean of the chlorophyll-a timeseries."),
                                                        p("Note that a subscript of ",tags$em("t")," represents today's chlorophyll-a, while ", tags$em("t-1"), " represents the 1-day lag of chlorophyll-a.")
                                                      ),
                                                      p("Let's fit this model to our data!"),
                                                      actionButton("fit_model",label = "Fit model"),
                                                      br(),br(),
                                                      wellPanel(
                                                        uiOutput("ar_model")
                                                      ),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q15", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      h3("Plot of model predictions vs. observations"),
                                                      wellPanel(
                                                        plotlyOutput("arfit_plot")
                                                      ),
                                                      downloadButton("save_arfit_plot", "Download plot", icon = icon("download"))
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Assess model fit"),
                                                      p("Before we use this model for forecasting, it is a good idea to see how well it fits our data. We have already assessed our model fit visually, using the plot of model predictions vs. observations above. Now we will explore two more methods for assessing model fit:"),
                                                      tags$ul(
                                                        tags$li(tags$b("Bias")," is the mean difference between model predictions and observations. The smaller the absolute value of the bias, the better your model fit.")
                                                      ),
                                                      tags$ul(
                                                        tags$li(tags$b("Root mean square error (RMSE)")," is the mean sum of squared errors (differences between predictions and observations), and can be calculated as:")
                                                      ),
                                                      wellPanel(
                                                        div("$$RMSE = \\sqrt{\\frac{\\sum_{i=1}^{N}(Predicted_i - Observed_i)^2}{N}}$$"),
                                                        p("The closer RMSE is to 0, the better your model fit.")
                                                      )
                                                      ),
                                               column(6,
                                                      h4(tags$em("Let's calculate bias and RMSE for the AR model you just fit!")),
                                                      fluidRow(
                                                        column(6,
                                                               actionButton("calc_bias",label = "Calculate bias")
                                                        ),
                                                        column(6,
                                                               wellPanel(
                                                                 htmlOutput("out_bias")
                                                               )
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               actionButton("calc_rmse",label = "Calculate RMSE")
                                                        ),
                                                        column(6,
                                                               wellPanel(
                                                                 htmlOutput("out_rmse")
                                                               )
                                                        )
                                                      ),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q16", 1])),
                                                                   p(tags$b(quest["q17", 1]))
                                                            )
                                                          )
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             #*** Next step ----
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will use the autoregressive model you fitted to generate a forecast of chlorophyll-a with uncertainty!")
                                               )
                                             )
                                    ),
                                    #* Objective 4 - Generate forecast ====
                                    tabPanel(title = "Objective 4 - Generate forecast", value = "stab5",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 4 - Generate forecast"),
                                                                p(id = "txt_j", module_text["obj_04", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("A note on shifting from model fitting to forecasting"),
                                                      p(id = "txt_j", "So far, we have fit our chlorophyll-a model using yesterday's chlorophyll-a (a 1-day lag) to predict today's chlorophyll-a:"),
                                                      div("$$Chla_{t} = \\beta_0 + \\beta_1 * (Chla_{t-1} - \\overline{Chla}) + \\overline{Chla}$$"),
                                                      p(id = "txt_j", "Now, to forecast, we will need to make a subtle but important change in the way we write our model, to show that instead of predicting today's chlorophyll-a, we are now forecasting tomorrow's chlorophyll-a:"),
                                                      div("$$Chla_{t+1} = \\beta_0 + \\beta_1 * (Chla_{t} - \\overline{Chla}) + \\overline{Chla}$$"),
                                                      p(tags$b("Note the change in the subscripts of the Chla variables from t-1 to t and t to t+1!!")),
                                                      p("In addition, remember that forecasts should include the uncertainty associated with future predictions. Below, we will work through how to calculate the uncertainty of a forecast.")
                                               ),
                                               column(6, align = "center",
                                                      img(src = "model_UC_draft_v2.png", height = "60%", id = "bla_border",
                                                          width = "60%", tags$style("border: solid 2px black;"))
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Understanding forecast uncertainty"),
                                                      p(tags$em("Use the slides and text below to learn about forecast uncertainty.")),
                                                      p(tags$b("What is ecological forecast uncertainty?")),
                                                      tags$ul(
                                                        tags$li("Forecast uncertainty is the range of possible alternate future conditions predicted by a model. We generate multiple different predictions of the future because the future is inherently unknown.")
                                                      ),
                                                      p(tags$b("Where does ecological forecast uncertainty come from?")),
                                                      tags$ul(
                                                        tags$li("Uncertainty comes from natural variability in the environment, imperfect representation of an ecological system in a model, and error when measuring the system. When generating a forecast, uncertainty can come from the structure of the model used, the initial conditions of the model, the parameters of the model, and the data used to drive the model, among other sources.")
                                                      ),
                                                      p(tags$b("Why is uncertainty important to quantify for an ecological forecast?")),
                                                      tags$ul(
                                                        tags$li("Knowing the uncertainty in a forecast allows forecast users to make informed decisions based on the range of forecasted outcomes and prepare accordingly.")
                                                      ),
                                                      p(tags$b("What is an ensemble forecast?")),
                                                      tags$ul(
                                                        tags$li("One way of accounting for uncertainty in forecasts is through an ",tags$b("ensemble forecast.")," Ensemble forecasts are generated by running a model many times with different conditions. In our case, we will run our autoregressive model many times with slightly different conditions to account for uncertainty in the forecast. All the model runs together are referred to as the ",tags$b("ensemble.")," Each individual model run is referred to as an ",tags$b("ensemble member.")," Forecasters typically generate tens to hundreds of ensemble members to build uncertainty into their forecasts.")
                                                      )
                                               ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("fc_uc_slides", width = "700px", height = "525px")
                                                      ),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q18", 1]))
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #** Process Uncertainty ----
                                             fluidRow(
                                               column(12,
                                                      h3("Process Uncertainty")
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h4(tags$b("Process uncertainty")," is uncertainty caused by our inability to model all processes as observed in the real world."),
                                                      p(id = "txt_j", "Our autoregressive chlorophyll-a model uses previous chlorophyll-a values to forecast tomorrow's chlorophyll-a."),
                                                      p(id = "txt_j", "But we know that chlorophyll-a can be affected by other processes as well (such as water temperature, available light for photosynthesis, and nutrients needed for phytoplankton growth) and that our model has simplified or ignored these factors. To account for the uncertainty these simplifications introduce, we can add in ",tags$b("process noise (W)")," at each time step. In this model, chlorophyll-a tomorrow is a function of a 1-day lag of chlorophyll-a plus some noise ",tags$b("(W):")),
                                                      div("$$Chla_{t+1} = \\beta_0 + \\beta_1 * (Chla_{t} - \\overline{Chla}) + \\overline{Chla} + W_t$$")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      wellPanel(
                                                        h4(tags$em("Scroll through the slides below to learn how",tags$b(" process uncertainty")," is calculated and accounted for in a forecast."))
                                                      ),
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("proc_uc_slides", width = "1000px", height = "563px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Calculate process uncertainty distribution"),
                                                      p("We will use the residuals from the model you fit in Objective 3 to calculate a process uncertainty distribution for your forecast."),
                                                      p("Click the button below to calculate and view your process uncertainty distribution."),
                                                      actionButton("calc_proc_distrib","Calculate process uncertainty distribution"),
                                                      br(),br(),
                                                      wellPanel(
                                                        htmlOutput("proc_uc_sd")
                                                      ),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q19", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("proc_uc_distrib")
                                                      ),
                                                      downloadButton("save_proc_uc_distrib_plot", "Download plot", icon = icon("download"))
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Initial Conditions Uncertainty")
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      p(tags$b("Initial conditions")," are the starting conditions used by a model. In our autoregressive model, the initial condition is today's chlorophyll-a,"),
                                                      div("$$Chla_{t}$$"),
                                                      p("which is needed to forecast tomorrow's chlorophyll-a:"),
                                                      wellPanel(
                                                        div("$$Chla_{t+1} = \\beta_0 + \\beta_1 * (Chla_{t} - \\overline{Chla}) + \\overline{Chla} + W_t$$"),
                                                        p("where t = today and t+1 = tomorrow")
                                                      )
                                               ),
                                               column(6,
                                                      p(id = "txt_j", tags$b("Initial conditions uncertainty")," refers to uncertainty arising because the current conditions in an ecosystem - in our case, ",tags$b("lake chlorophyll-a"), " - are not precisely known."),
                                                      p(id = "txt_j", "Even though we have daily measurements of chlorophyll-a from our lake, we know that chlorophyll-a varies throughout the day so this measurement might not capture exactly the chlorophyll-a in our lake at this time. Additionally, there may be observation error in our chlorophyll-a measurements.")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      wellPanel(
                                                        h4(tags$em("Scroll through the slides below to understand how",tags$b(" initial conditions uncertainty "), "is calculated and accounted for in a forecast."))
                                                      ),
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("ic_uc_slides", width = "1000px", height = "563px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   h4("Hint: Read through the slides above before attempting to answer this question!"),
                                                                   p(tags$b(quest["q20", 1]))
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Plot high-frequency data"),
                                                      p("So far, we have been working with daily mean chlorophyll-a values from your chosen lake site to fit our model."),
                                                      p("Now, we will use some high-frequency (5-minute) chlorophyll-a data from our lake to estimate initial conditions uncertainty. For ease of visualization, we will only look at data from a few days. Click the button below to visualize the high-frequency data."),
                                                      actionButton("plot_high_freq","Plot high-frequency data"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q21", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                        wellPanel(
                                                          plotlyOutput("high_freq_plot")
                                                        ),
                                                        downloadButton("save_high_freq_plot", "Download plot", icon = icon("download"))
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Calculate initial condition uncertainty distribution"),
                                                      p("Now, we will calculate the mean daily standard deviation of our high-frequency chl-a measurements and use this value to an initial condition uncertainty distribution for our forecast. Click the button below to calculate initial conditions uncertainty."),
                                                      actionButton("calc_ic_uc","Calculate initial conditions uncertainty"),
                                                      br(),br(),
                                                      wellPanel(
                                                        htmlOutput("ic_uc_sd")
                                                      ),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q22", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("ic_distrib_plot")
                                                      ),
                                                      downloadButton("save_ic_distrib_plot", "Download plot", icon = icon("download"))
                                                      )
                                             ),
                                             br(),
                                             fluidRow(
                                               column(12,
                                                      box(id = "box2", width = 12, status = "warning",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h4(tags$b("To learn more about forecast uncertainty, explore our sister module ",
                                                                             tags$a(href="http://module6.macrosystemseddie.org", 
                                                                                    "Macrosystems EDDIE Module 6: Understanding Uncertainty in Ecological Forecasts.", target="_blank")))
                                                            )
                                                          )
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h2("Forecast!"),
                                                      )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      p("Finally, we are ready to use our autoregressive model to generate a one-day-ahead forecast with uncertainty. Notice that we are using our initial condition and process uncertainty distributions to run our model many times with slightly different random noise (W) and initial conditions values. This allows us to account for uncertainty in our forecast."),
                                                      img(src = "fc_w_uncert.png", height = "90%", id = "bla_border",
                                                          width = "90%", tags$style("border: solid 2px black;"))
                                                      ),
                                               column(6,
                                                      actionButton("fc1","Generate forecast"),
                                                      br(),br(),
                                                      wellPanel(
                                                        plotlyOutput("fc1_plot")
                                                      ),
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Visualize forecast"),
                                                      p("To prepare you for our next activity, let's visualize this forecast in a different way. We will use plots very similar to this one to help explain the process of data assimilation later on."),
                                                      actionButton("fc1_viz","Visualize forecast"),
                                                      br(),br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q23", 1])),
                                                                   p(tags$b(quest["q24", 1])),
                                                                   p(tags$b(quest["q25", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotOutput("fc1_viz_plot")
                                                      ),
                                                      downloadButton("save_fc1_viz_plot", "Download plot", icon = icon("download"))
                                                      )
                                             ),
                                             hr(),
                                             #*** Next step ----
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now that we have generated a forecast, we will explore the effect of data assimilation on forecast accuracy and uncertainty.")
                                               )
                                             )
                                    )
                        )
               ),
               # 6. Activity B ----
               tabPanel(title = tab_names["mtab6", 2], value = "mtab6",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity B - Explore Data Assimilation"),
                                           p(module_text["act_B", ])
                                 )
                          )
                        ),
                        tabsetPanel(id = "tabseries3",
                                    #* Objective 5 - Assimilate data ====
                                    tabPanel(title = "Objective 5 - Assimilate data", value = "stab7",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 5 - Assimilate data"),
                                                                p(id = "txt_j", module_text["obj_05", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      p("Now that we have generated a forecast with uncertainty, we are going to explore the effect of data assimilation on our forecast. Remember, ",tags$b("data assimilation")," is the process of using observed data to update our forecast model as the data become available."),
                                               ),
                                               column(6,
                                                      p("Let's pretend that a day has passed since we made our first forecast, and we now have a new observation that we can use to update our forecast."),
                                                      p("We will use this observation to update our forecast ",tags$b("initial condition.")," We will do this using a statistical technique called an ",tags$b("ensemble Kalman filter."))
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(5,
                                                      h3("Understanding the ensemble Kalman filter"),
                                                      h4(tags$em("What is an ensemble Kalman filter?")),
                                                      p("An ",tags$b("ensemble Kalman filter")," is a statistical technique that updates model predictions to more closely match the most recently observed data, while accounting for uncertainty in both model predictions and observations. While there are many techniques that can be used to assimilate data in ecological forecasts, the benefits of an ensemble Kalman filter are:"),
                                                      tags$ol(
                                                        tags$li("It is ",tags$b("designed to be used with model ensembles,")," and so is an ideal method for forecasts which include uncertainty."),
                                                        tags$li("It ",tags$b("accounts for uncertainty in both model predictions and observations,")," rather than assuming that observations are 'true' and have no uncertainty. As a result, when a model is updated with an ensemble Kalman filter, the updated state of the model will not always perfectly match the new observations, because the ensemble Kalman filter integrates information from both the model predictions and the observations."),
                                                        tags$li("It can be ",tags$b("used to update multiple variables and parameters")," within a model, even if not all of the variables and parameters are observed. For example, suppose you have a model that predicts both water temperature and air temperature, but you only have observations of water temperature. An ensemble Kalman filter can use the relationship between water and air temperature in the model to update both variables as well as relevant model parameters using just the water temperature observations.")
                                                      ),
                                                      p("For today, we will use a simplified version of the ensemble Kalman filter that just updates the initial condition of chlorophyll-a using new observations when they become available.")
                                               ),
                                               column(6, offset = 1,
                                                      br(),br(),
                                                      img(src = "EnKF_figure.png", height = "90%", id = "bla_border",
                                                          width = "90%", align = "center"),
                                                      p("Image adapted from: Reichle, R. H., Walker, J. P., Koster, R. D., & Houser, P. R. (2002). ", a("Extended versus Ensemble Kalman Filtering for Land Data Assimilation", href = "https://doi.org/10.1175/1525-7541(2002)003%3C0728:EVEKFF%3E2.0.CO;2", target = "_blank"), ", Journal of Hydrometeorology, 3(6).")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q26", 1]))
                                                            )
                                                          )
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Assimilate data to update the forecast initial condition"),
                                                      p(tags$b("We have recorded a new observation!"), "Click the button to view it on the plot to the right."),
                                                      actionButton("view_new_obs","View new observation"),
                                                      br(),br(),
                                                      wellPanel(
                                                        htmlOutput("new_obs")
                                                      ),
                                                      p("Now, click the button below to run the ensemble Kalman filter and use this new observation to update the forecast initial condition, which will appear on the plot to the right."),
                                                      actionButton("update_ic","Update initial condition with ensemble Kalman filter"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q27", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotOutput("updated_ic_plot")
                                                      ),
                                                      downloadButton("save_updated_ic_plot", "Download plot", icon = icon("download"))
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Make a second forecast after assimilating data"),
                                                      p("Now we will make a second forecast for the next day using our updated initial conditions."),
                                                      p("We will plot both of our forecasts together with the initial conditions for each forecast."),
                                                      actionButton("second_forecast_da","Generate forecast"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q28", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotOutput("second_fc_da_plot")
                                                      ),
                                                      downloadButton("save_second_fc_da_plot", "Download plot", icon = icon("download"))
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Updating initial conditions when observations are missing"),
                                                      p("We have explored how the ensemble Kalman filter can update the forecast initial condition using a new observation.",tags$b("But what if there is no observation to use for updating?")," What will be the outcome of the applying the ensemble Kalman filter in this situation?"),
                                                      p("Let's pretend that instead of recording a new observation, a thunderstorm came up and we were unable to sample at your chosen lake. We will run the ensemble Kalman filter with an 'NA' in place of an observation."),
                                                      p("Click the button below to see what happens to the initial condition when an observation is missing"),
                                                      actionButton("view_ic_no_da","Run ensemble Kalman filter with missing observation"),
                                                      br(),br(),
                                                      conditionalPanel("input.view_ic_no_da > 0",
                                                                       checkboxInput("show_ic", "Click to show/remove the initial condition for 2020-09-26.", value = TRUE),
                                                                       ),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q29", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotOutput("updated_ic_no_da_plot")
                                                      ),
                                                      downloadButton("save_updated_ic_no_da_plot", "Download plot", icon = icon("download"))
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Make a second forecast when no data have been assimilated"),
                                                      p("Now we will make a second forecast. You have seen that when an observation is missing, the initial condition cannot be updated. Let's see how this affects the second forecast."),
                                                      p("We will plot both of our forecasts together with the initial conditions for each forecast"),
                                                      actionButton("second_forecast_no_da","Generate forecast"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q30", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      wellPanel(
                                                        plotOutput("second_fc_no_da_plot")
                                                      ),
                                                      downloadButton("save_second_fc_no_da_plot", "Download plot", icon = icon("download"))
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Compare forecasts generated with and without data assimilation"),
                                                      fluidRow(
                                                        column(6,
                                                               h4("Forecast with data assimilation"),
                                                               wellPanel(
                                                                 plotOutput("second_fc_da_plot2")
                                                               )
                                                               ),
                                                        column(6,
                                                               h4("Forecast without data assimilation"),
                                                               wellPanel(
                                                                 plotOutput("second_fc_no_da_plot2")
                                                               )
                                                               )
                                                      ),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q31", 1]))
                                                            )
                                                          )
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             #*** Next step ----
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we will explore how observation uncertainty affects data assimilation and forecasts.")
                                               )
                                             )
                                    ),
                                    #* Objective 6 - Explore observation uncertainty ====
                                    tabPanel(title = "Objective 6 - Explore observation uncertainty", value = "stab8",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 6 - Explore observation uncertainty"),
                                                                p(id = "txt_j", module_text["obj_06", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("Make a prediction"),
                                                      p("We have explored the effect of data assimilation vs. no data assimilation on forecasts using an ensemble Kalman filter, which accounts for uncertainty in both model predictions and observations."),
                                                      p("Now, imagine that we have purchased a new water quality sensor, which takes incredibly accurate chlorophyll-a measurements, thus decreasing our observation uncertainty. How might this decrease in observation uncertainty affect our forecasts?"),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q32", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      img(src = "LSPA_buoy_and_friends.png", height = "90%", id = "bla_border",
                                                          width = "90%", align = "center"),
                                                      p(tags$em("Members of the Virginia Tech Reservoir Group and Lake Sunapee Protective Association with the Lake Sunapee buoy. Photo credit: "))
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Generate initial condition distribution with low observation uncertainty"),
                                                      p("Let's plot an initial conditions distribution that reflects the lower uncertainty due to our new water quality sensor."),
                                                      p("We will also plot the original initial condition you generated in Objective 4 for comparison."),
                                                      actionButton("plot_low_ic","Generate distribution")
                                                      ),
                                               column(4,
                                                      h4("Initial condition distribution with low observation uncertainty"),
                                                      wellPanel(
                                                        plotlyOutput("ic_distrib_low_plot")
                                                      )
                                                      ),
                                               column(4,
                                                      h4("Original initial condition distribution generated in Objective 4"),
                                                      wellPanel(
                                                        plotlyOutput("ic_distrib_plot2")
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Compare forecasts made with low observation uncertainty to previous forecasts"),
                                                      p("Now, we will replicate the two-forecast plot with data assimilation that we created above, but this time with lower observation uncertainty."),
                                                      actionButton("plot_fc_low_obs_uc","Plot forecasts with low observation uncertainty"),
                                                      fluidRow(
                                                        column(6,
                                                               h4("Forecasts assimilating data with low observation uncertainty"),
                                                               wellPanel(
                                                                 plotOutput("second_fc_low_obs_uc")
                                                               ),
                                                               downloadButton("save_second_fc_low_obs_uc_plot", "Download plot", icon = icon("download"))
                                                        ),
                                                        column(6,
                                                               h4("Previous forecasts with data assimilation"),
                                                               wellPanel(
                                                                 plotOutput("second_fc_da_plot3")
                                                               )
                                                        )
                                                      )
                                                      )
                                             ),
                                             #*** Next step ----
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will explore the effect of changes in data frequency on data assimilation and forecast output.")
                                               )
                                             )
                                    ),
                                    #* Objective 9 - Explore data frequency ====
                                    tabPanel(title = tab_names["stab9", 2], value = "stab9",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab9", 2]),
                                                                p(id = "txt_j", module_text["obj_09", ])
                                                      )
                                               )
                                             ),
                                             #** Key terms ----
                                             fluidRow(
                                               column(11, offset = 1,
                                                      h3("Key terms")
                                               ),
                                               column(4, offset = 1,
                                                      tags$ul(
                                                        tags$li(tags$b("Data frequency "), module_text["data_frequency", ]),
                                                        tags$li(tags$b("Data latency "), module_text["data_latency", ])
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #** Chl-a Slides ----
                                             fluidRow(
                                               column(12,
                                                      h1("How ", tags$em("frequently"), " do we collect data measurements?"),
                                                      h2(tags$b("Chlorophyll-a"))
                                               ),
                                               column(4,
                                                      h3("Sensor"),
                                                      p(data_frequency_latency["Chl-a sensor", 2], id = "txt_j"),
                                                      p(data_frequency_latency["Chl-a sensor", 3], id = "txt_j"),
                                                      h3("Lab Water Sample Measurement"),
                                                      p(data_frequency_latency["Laboratory analysis of water sample for chl-a", 2], id = "txt_j"),
                                                      p(data_frequency_latency["Laboratory analysis of water sample for chl-a", 3], id = "txt_j")
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        slickROutput("chla_slides2")
                                                      )
                                               )
                                               # column(3,
                                               #        h3("Lab Measurement"),
                                               #        p(data_frequency_latency["Laboratory analysis of water sample for chl-a", 2], id = "txt_j"),
                                               #        p(data_frequency_latency["Laboratory analysis of water sample for chl-a", 3], id = "txt_j")
                                               # )
                                             ),
                                             hr(),
                                             #** Nitrate Slides ----
                                             # fluidRow(
                                             #   column(12,
                                             #          h2(tags$b("Nitrate"))
                                             #   ),
                                             #   column(3,
                                             #          h3("Sensor"),
                                             #          p(data_frequency_latency["Nitrate sensor", 2], id = "txt_j"),
                                             #          p(data_frequency_latency["Nitrate sensor", 3], id = "txt_j")
                                             #   ),
                                             #   column(6,
                                             #          wellPanel(
                                             #            slickROutput("nitrate_slides2")
                                             #          )
                                             #   ),
                                             #   column(3,
                                             #          h3("Lab Measurement"),
                                             #          p(data_frequency_latency["Laboratory analysis of water sample for nitrate", 2], id = "txt_j"),
                                             #          p(data_frequency_latency["Laboratory analysis of water sample for nitrate", 3], id = "txt_j")
                                             #   )
                                             # ),
                                             #** Questions ----
                                             #hr(),
                                             fluidRow(
                                               column(10, offset = 1,
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[38], label = quest[qid[38], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[39], label = quest[qid[39], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             hr(),
                                             #** Explore data assimilation frequency ----
                                             fluidRow(
                                               column(12,
                                                      h3("Explore data assimilation frequency")
                                               ),
                                               column(4,
                                                      p("Use the slider below to adjust the observation uncertainty and then generate a forecast that assimilates observations with the observation uncertainty you specified."),
                                                      # checkboxGroupInput("da_freq_da", label = "Data to assimilate:", 
                                                      #                    choices = view_vars$lname[1:2]),
                                                      sliderInput("da_freq_chla", "Chl-a assimilation frequency (days)",
                                                                  min = 1, max = 30, step = 1, value = 30),
                                                      # sliderInput("da_freq_nitrate", "Nitrate assimilation frequency (days)",
                                                      #             min = 1, max = 30, step = 1, value = 30),
                                                      actionButton("run_fc_da_freq", label = div("Run Forecast", icon("running"))),
                                                      h4("RMSE of forecast"),
                                                      wellPanel(
                                                        textOutput("rmse_da_freq")
                                                      ),
                                                      actionButton("save_rmse2", "Save RMSE", icon = icon("save")),
                                                      br(),
                                                      DTOutput("da_freq_rmse"),
                                                      br(),
                                                      p(tags$b("Note:"), " You can also select a row in the table BEFORE clicking 'Save RMSE' to overwrite a row in the table.")
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        plotlyOutput("fc_da_freq_plot"),
                                                        sliderInput("nday_da_freq", "N days", min = 1, max = 35, value = 1, step = 1,
                                                                    animate = TRUE),
                                                        checkboxInput("add_obs_da_freq", "Add observations"),
                                                        radioButtons("plot_type_da_freq", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE)
                                                        ),
                                                      #** Questions ----
                                                      hr(),
                                                      fluidRow(
                                                        column(10, offset = 1,
                                                               box(id = "box4", width = 12, status = "primary",
                                                                   solidHeader = TRUE,
                                                                   fluidRow(
                                                                     column(10, offset = 1,
                                                                            h3("Questions"),
                                                                            textAreaInput2(inputId = qid[40], label = quest[qid[40], ], width = "90%"),
                                                                            textAreaInput2(inputId = qid[41], label = quest[qid[41], ], width = "90%"),
                                                                            textAreaInput2(inputId = qid[42], label = quest[qid[42], ], width = "90%")
                                                                     )
                                                                   )
                                                               )
                                                        )
                                                      ),
                                                      hr()
                                                      )
                                               ),
                                             hr(),
                                             #*** Next step ----
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Synthesize your understanding of data assimilation in ecological forecasting to assimilate data into forecasts used for water resource management.")
                                                      )
                                               )
                                             ),
                                    # * Activity B - Summary ====
                                    tabPanel(title = tab_names["stab10", 2], value = "stab10",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab10", 2]),
                                                                p(id = "txt_j", module_text["act_B_summ", ]),
                                                                p("Remember, the Shiny app will disconnect if you leave it idle for 10 minutes, so make sure to download your '.eddie' file at the bottom of the page to checkpoint your progress.")
                                                      )
                                               )
                                             ),
                                             #** Questions ----
                                             hr(),
                                             fluidRow(
                                               column(10, offset = 1,
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[43], label = quest[qid[43], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[44], label = quest[qid[44], ], width = "90%")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr()
                                    )
                        )
               ),
               # 7. Activity C ----
               tabPanel(title = tab_names["mtab7", 2], value = "mtab7",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity C - Management Scenario"),
                                           p(module_text["act_C", ])
                                 )
                          )
                        ),
                        tabsetPanel(id = "tabseries4",
                                    #* Objective 10 - Management Scenario ====
                                    tabPanel(title = tab_names["stab11", 2], value = "stab11",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab11", 2]),
                                                                p(id = "txt_j", "Now that you have explored the effect of observation uncertainty and data collection frequency on data assimilation, you will apply your knowledge to a lake management scenario.")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Scenario"),
                                                      p("Green Reservoir, one of several drinking water supply reservoirs for a city of about 100,000 people, suffers from algal blooms which sometimes release toxins that could threaten the health of water consumers. Forecasts of chl-a allow Green Reservoir managers to plan ahead to source water from other reservoirs and/or pre-emptively issue drinking water warnings."),
                                                      p("The water authority that manages Green Reservoir has developed a forecast system that assimilates monthly chlorophyll-a observations which are collected manually at the beginning of each month by water authority personnel. But managers are interested in exploring whether they can increase the accuracy of their forecasts by investing in a high-frequency chlorophyll-a sensor."),
                                                      p("The water authority of Green Reservoir has budgeted $15,000 for new forecast sensors. After doing some market research, the forecast development team at the water authority has identified two options for chl-a sensors: "),
                                                      p("Sensor A costs $15,000, including the cost of the technology needed to wirelessly transmit data from this sensor to computers that will run the forecast model as well as personnel to install and maintain the sensor."),
                                                      p("Sensor B costs $20,000, including the cost of the technology and personnel. Sensor B is more expensive because it is able to make more precise observations (less observation error) and is somewhat more reliable than Sensor A (less likely to experience sensor malfunction).")
                                                      ),
                                               column(6,
                                                      img(src = "Objective10_Scenario1.jpg", height = "80%", id = "bla_border",
                                                          width = "80%", tags$style("border: solid 2px black;")),
                                                      p("Algal Bloom - Kelly's Island, Lake Erie by NOAA Great Lakes Environmental Research Laboratory")
                                                      )
                                               
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      p("As a forecasting expert, you have been brought in as a consultant to help the water authority’s forecasting team decide whether to ask the water authority for an additional $5,000 to cover the costs of Sensor B."),
                                                      br(),
                                                      wellPanel(
                                                        h4("Make a decision"),
                                                        radioButtons("data_collec1", "Based on what you have learned in Activities A and B, which sensor do you recommend?", choices = data_collect_options$text, selected = character(0)
                                                                     )
                                                        ),
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[45], label = quest[qid[45], ], width = "90%")
                                                            )
                                                            )
                                                          )
                                                      ),
                                               column(6,
                                                      img(src = "mgmt_tradeoff.png", height = "80%", id = "bla_border",
                                                          width = "80%", tags$style("border: solid 2px black;")),
                                                      p("Tradeoffs for management.")
                                                      )
                                             ),
                                             #** Expensive sensor ----
                                             hr(),
                                             conditionalPanel("input.data_collec1 != ''", #chose expensive sensor
                                               fluidRow(
                                                 column(4,
                                                        h3("Run forecast with no sensor"),
                                                        p("For comparison, click the button below to generate the forecast that managers are using now, without a sensor. In this case, the most recent observation occurred on Oct. 2, 2020, when water authority personnel traveled to Green Reservoir and collected a manual chlorophyll-a sample."),
                                                        p("Managers of Green Reservoir know that concentrations of over 25 ug/L of chl-a can lead to water quality concerns in drinking water. So they will use a threshold of 25 ug/L as a trigger for issuing a drinking water warning to the public."),
                                                        actionButton("run_fc_dec3a", "Run forecast"),
                                                        conditionalPanel("input.run_fc_dec3a > 0",
                                                                         box(id = "box4", width = 12, status = "primary",
                                                                             solidHeader = TRUE,
                                                                             fluidRow(
                                                                               column(10, offset = 1,
                                                                                      h3("Questions"),
                                                                                      textAreaInput2(inputId = qid[46], label = quest[qid[46], ], width = "90%")
                                                                               )
                                                                             )
                                                                         )
                                                        ),
                                                        p("Check the box below to see the observations on the day the forecast was issued and on Oct. 27."),
                                                        checkboxInput("add_obs_actc3a", "Add observations")
                                                 ),
                                                 column(8,
                                                        wellPanel(
                                                          plotlyOutput("fc_dec3a")
                                                        )
                                                 )
                            
                                               ),
                                               
                                               #run forecast with chosen sensor (expensive)
                                               hr(),
                                               fluidRow(
                                                 column(4,
                                                        h3("Run forecast with Sensor B (expensive sensor)"),
                                                        actionButton("run_fc_dec1a", "Run forecast"),
                                                        conditionalPanel("input.run_fc_dec1a > 0",
                                                                         box(id = "box4", width = 12, status = "primary",
                                                                             solidHeader = TRUE,
                                                                             fluidRow(
                                                                               column(10, offset = 1,
                                                                                      h3("Questions"),
                                                                                      textAreaInput2(inputId = qid[47], label = quest[qid[47], ], width = "90%")
                                                                               )
                                                                             )
                                                                         )
                                                        ),
                                                        p("Check the box below to see the observations on the day the forecast was issued and on Oct. 27."),
                                                        checkboxInput("add_obs_actc1a", "Add observations")
                                                 ),
                                                 column(8,
                                                        wellPanel(
                                                          plotlyOutput("fc_dec1a")
                                                        )
                                                 )
                                               ),
                                               
                                               #run forecast with non-chosen sensor (cheap)
                                               hr(),
                                               fluidRow(
                                                 column(4,
                                                        h3("Run forecast with Sensor A (cheaper sensor)"),
                                                        actionButton("run_fc_dec2a", "Run forecast"),
                                                        conditionalPanel("input.run_fc_dec2a > 0",
                                                                         box(id = "box4", width = 12, status = "primary",
                                                                             solidHeader = TRUE,
                                                                             fluidRow(
                                                                               column(10, offset = 1,
                                                                                      h3("Questions"),
                                                                                      textAreaInput2(inputId = qid[48], label = quest[qid[48], ], width = "90%")
                                                                               )
                                                                             )
                                                                         )
                                                        ),
                                                        p("Check the box below to see the observations on the day the forecast was issued and on Oct. 27."),
                                                        checkboxInput("add_obs_actc2a", "Add observations")
                                                 ),
                                                 column(8,
                                                        wellPanel(
                                                          plotlyOutput("fc_dec2a")
                                                        )
                                                 )
                                               ),
                                               #** Questions ----
                                               fluidRow(
                                                 column(10, offset = 1,
                                                        box(id = "box4", width = 12, status = "primary",
                                                            solidHeader = TRUE,
                                                            fluidRow(
                                                              column(10, offset = 1,
                                                                     h3("Questions"),
                                                                     textAreaInput2(inputId = qid[49], label = quest[qid[49], ], width = "90%"),
                                                                     textAreaInput2(inputId = qid[50], label = quest[qid[50], ], width = "90%"),
                                                                     textAreaInput2(inputId = qid[51], label = quest[qid[51], ], width = "90%"),
                                                                     textAreaInput2(inputId = qid[52], label = quest[qid[52], ], width = "90%")
                                                              )
                                                            )
                                                        )
                                                 )
                                               )
                                             )#,
                                             
                                             
                                             # #** Cheap sensor ----
                                             # 
                                             # conditionalPanel("input.data_collec1 == 'Sensor A'",
                                             #                  #run forecast with no sensor
                                             #                  fluidRow(
                                             #                    column(4,
                                             #                           h3("Run forecast with no sensor"),
                                             #                           p("For comparison, click the button below to generate the forecast that managers are using now, without a sensor. In this case, the most recent observation occurred on Oct. 2, 2020, when water authority personnel traveled to Green Reservoir and collected a manual chlorophyll-a sample."),
                                             #                           p("Managers of Green Reservoir know that concentrations of over 25 ug/L of chl-a can lead to water quality concerns in drinking water. So they will use a threshold of 25 ug/L as a trigger for issuing a drinking water warning to the public."),
                                             #                           actionButton("run_fc_dec3b", "Run forecast"),
                                             #                           conditionalPanel("input.run_fc_dec3b > 0",
                                             #                                            box(id = "box4", width = 12, status = "primary",
                                             #                                                solidHeader = TRUE,
                                             #                                                fluidRow(
                                             #                                                  column(10, offset = 1,
                                             #                                                         h3("Questions"),
                                             #                                                         textAreaInput2(inputId = qid[46], label = quest[qid[46], ], width = "90%")
                                             #                                                  )
                                             #                                                )
                                             #                                            ),
                                             #                           ),
                                             #                           p("Check the box below to see the observations on the day the forecast was issued and on Oct. 27."),
                                             #                           checkboxInput("add_obs_actc3b", "Add observations")
                                             #                    ),
                                             #                    column(8,
                                             #                           wellPanel(
                                             #                             plotlyOutput("fc_dec3b")
                                             #                           )
                                             #                    )
                                             #        
                                             #                  ),
                                             #                  #run forecast with chosen sensor (cheap)
                                             #                  hr(),
                                             #                  fluidRow(
                                             #                    column(4,
                                             #                           h3("Run forecast with Sensor A (cheaper sensor)"),
                                             #                           p("Click the button below to see a daily forecast for the next 30 days generated using data from the cheaper sensor."),
                                             #                           actionButton("run_fc_dec2b", "Run forecast"),
                                             #                           conditionalPanel("input.run_fc_dec2b > 0",
                                             #                                            box(id = "box4", width = 12, status = "primary",
                                             #                                                solidHeader = TRUE,
                                             #                                                fluidRow(
                                             #                                                  column(10, offset = 1,
                                             #                                                         h3("Questions"),
                                             #                                                         textAreaInput2(inputId = qid[47], label = quest[qid[47], ], width = "90%")
                                             #                                                  )
                                             #                                                )
                                             #                                            ),
                                             #                           ),
                                             #                           p("Check the box below to see the observations on the day the forecast was issued and on Oct. 27."),
                                             #                           checkboxInput("add_obs_actc2b", "Add observations")
                                             #                    ),
                                             #                    column(8,
                                             #                           wellPanel(
                                             #                             plotlyOutput("fc_dec2b")
                                             #                           )
                                             #                    )
                                             #                    
                                             #                  ),
                                             #                  #run forecast with non-chosen sensor (expensive)
                                             #                  hr(),
                                             #                  fluidRow(
                                             #                    column(4,
                                             #                           h3("Run forecast with Sensor B (expensive sensor)"),
                                             #                           actionButton("run_fc_dec1b", "Run forecast"),
                                             #                           conditionalPanel("input.run_fc_dec1b > 0",
                                             #                                            box(id = "box4", width = 12, status = "primary",
                                             #                                                solidHeader = TRUE,
                                             #                                                fluidRow(
                                             #                                                  column(10, offset = 1,
                                             #                                                         h3("Questions"),
                                             #                                                         textAreaInput2(inputId = qid[48], label = quest[qid[48], ], width = "90%")
                                             #                                                  )
                                             #                                                )
                                             #                                            ),
                                             #                           ),
                                             #                           p("Check the box below to see the observations on the day the forecast was issued and on Oct. 27."),
                                             #                           checkboxInput("add_obs_actc1b", "Add observations")
                                             #                    ),
                                             #                    column(8,
                                             #                           wellPanel(
                                             #                             plotlyOutput("fc_dec1b")
                                             #                           )
                                             #                    )
                                             #                  ),
                                             #                  #** Questions ----
                                             #                  fluidRow(
                                             #                    column(10, offset = 1,
                                             #                           box(id = "box4", width = 12, status = "primary",
                                             #                               solidHeader = TRUE,
                                             #                               fluidRow(
                                             #                                 column(10, offset = 1,
                                             #                                        h3("Questions"),
                                             #                                        textAreaInput2(inputId = qid[49], label = quest[qid[49], ], width = "90%"),
                                             #                                        textAreaInput2(inputId = qid[50], label = quest[qid[50], ], width = "90%"),
                                             #                                        textAreaInput2(inputId = qid[51], label = quest[qid[51], ], width = "90%"),
                                             #                                        textAreaInput2(inputId = qid[52], label = quest[qid[52], ], width = "90%")
                                             #                                 )
                                             #                               )
                                             #                           )
                                             #                    )
                                             #                  ),
                                             # )
                                             # hr(),
                                             # fluidRow(
                                             #   column(6,
                                             #          h3("Validate forecast"),
                                             #          actionButton("valid_fc_dec1", "Valid forecast")
                                             #   ),
                                             #   column(6,
                                             #          wellPanel(
                                             #            h3("VALIDATE PLOT"),
                                             #            plotlyOutput("valid_plot1")
                                             #          )
                                             #   )
                                             # ),
                                             
                                             # # Old version
                                             # hr(),
                                             # fluidRow(
                                             #   column(12,
                                             #          wellPanel(style = paste0("background: ", obj_bg),
                                             #                    h3(tab_names["stab12", 2], " - Old version"),
                                             #                    p(id = "txt_j", module_text["obj_10", ])
                                             #          )
                                             #   )
                                             # ),
                                             # hr(),
                                             # fluidRow(
                                             #   column(3, offset = 2,
                                             #     h3(quest[qid[48], ]),
                                             #     radioButtons("scen_select", label = "", choices = reservoir_sites, selected = character(0))
                                             #   )
                                             # ),
                                             # hr(),
                                             # conditionalPanel("input.scen_select == null",
                                             #                  wellPanel(
                                             #                    p(tags$em("Select a scenario above."))
                                             #                  )
                                             # ),
                                             # conditionalPanel("input.scen_select == 'Green Reservoir'",
                                             #                  fluidRow(
                                             #                    column(4, offset = 1,
                                             #                           h3("Scenario"),
                                             #                           p(id = "txt_j", module_text["scenario1", ]),
                                             #                           img(src = "Objective10_Scenario1.jpg", height = "80%", id = "bla_border",
                                             #                               width = "80%", tags$style("border: solid 2px black;")),
                                             #                           p("Algal Bloom - Kelly's Island, Lake Erie by NOAA Great Lakes Environmental Research Laboratory")
                                             #                           ),
                                             #                    column(6, offset = 1,
                                             #                           h3("Chlorophyll-a Data Collection"),
                                             #                           wellPanel(
                                             #                             slickROutput("chla_slides3")
                                             #                             )
                                             #                           )
                                             #                    ),
                                             #                  ),
                                             # conditionalPanel("input.scen_select == 'Brown Reservoir'",
                                             #                  fluidRow(
                                             #                    column(4, offset = 1,
                                             #                           h3("Scenario"),
                                             #                           p(id = "txt_j", module_text["scenario2", ]),
                                             #                           img(src = "Objective10_Scenario2.jpg", height = "80%", id = "bla_border",
                                             #                               width = "80%", tags$style("border: solid 2px black;")),
                                             #                           p("Sao Simao Reservoir, Brazil (NASA, International Space Station Science, 11/22/07) by NASA's Marshall Space Flight Center.")
                                             #                           ),
                                             #                    column(6, offset = 1,
                                             #                           h3("Nitrate Data Collection"),
                                             #                           wellPanel(
                                             #                             slickROutput("nitrate_slides3")
                                             #                             )
                                             #                           )
                                             #                    )
                                             #                  ),
                                             # hr(),
                                             # fluidRow(
                                             #   column(5,
                                             #          h3("Design Water Quality Monitoring Program"),
                                             #          p(id = "txt_j", module_text["reservoir_monitoring_program", ]),
                                             #          br(),
                                             #          p(tags$b("Target period:"), "1 year"),
                                             #          p(tags$b("Budget:"), "$10,000"),
                                             #          br(),
                                             #          h4("Things to consider:"),
                                             #          wellPanel(
                                             #            tags$ol(
                                             #              tags$li(id = "txt_j", tags$b("If you choose to purchase sensors:")),
                                             #              tags$ul(
                                             #                tags$li("Be sure to purchase a buoy, reservoir access field equipment, and at least one field personnel trip to the reservoir (all of these are needed to deploy the sensors in the reservoir). "),
                                             #                tags$li("Consider whether you would like to invest in wireless data streaming capability. Wireless data streaming will reduce your data latency. If you do not invest in wireless data streaming, you MUST budget for personnel trips to the reservoir to maintain sensors and collect data in order to retrieve data from the sensors."),
                                             #                tags$li("Consider whether you would like to invest in personnel hours for a field technician to maintain the sensors. Regular sensor maintenance by a field technician reduces the likelihood of sensor malfunction. Maintenance is recommended at least twice a month. With no investment in personnel to maintain sensors, sensor data streams cannot be restored once they malfunction (in other words, once your sensor goes down, you’re out of luck!).")
                                             #              ),
                                             #              tags$br(),
                                             #              tags$li(tags$b("If you choose to invest in personnel hours to collect field samples:")),
                                             #              tags$ul(
                                             #                tags$li("Be sure you purchase reservoir access and manual sampling field equipment (required equipment to collect water samples)."),
                                             #                tags$li("You must budget for the cost of analyzing those samples in the laboratory as well.")
                                             #              )
                                             #            )
                                             #          )
                                             #   ),
                                             #   column(6, offset = 1,
                                             #          h3("Budgetary items"),
                                             #          wellPanel(
                                             #            DTOutput("wq_monitoring_tab")
                                             #            )
                                             #          )
                                             #   ),
                                             # hr(),
                                             # fluidRow(
                                             #   column(4,
                                             #          h4("Check the items below which you want to include in your monitoring program."),
                                             #          p(tags$b("Sensors")),
                                             #          checkboxInput("chla_sens", budget_options$label[1]),
                                             #          checkboxInput("nitrate_sens", budget_options$label[2]),
                                             #          checkboxInput("data_stream", budget_options$label[3]),
                                             #          p(tags$b("Field Equipment")),
                                             #          checkboxInput("buoy", budget_options$label[4]),
                                             #          checkboxInput("res_access", budget_options$label[5]),
                                             #          checkboxInput("man_samp", budget_options$label[6]),
                                             #          checkboxInput("sens_main", budget_options$label[7]),
                                             #          p(tags$b("Field Personnel")),
                                             #          checkboxInput("deploy_sens", budget_options$label[8]),
                                             #          conditionalPanel("input.deploy_sens",
                                             #                           wellPanel(
                                             #                             radioButtons("ndeploy_sens", "Visits for the year", choices = c(1, 12, 24), inline = TRUE)
                                             #                             )
                                             #                           ),
                                             #          checkboxInput("chla_samp", budget_options$label[9]),
                                             #          conditionalPanel("input.chla_samp",
                                             #                           # numericInput("nchla_samp", "Frequency per week", value = 1, min = 1, max = 7)
                                             #                           radioButtons("nchla_samp", "Frequency per week", choices = c(1, 2, 7), inline = TRUE)
                                             #          ),
                                             #          checkboxInput("nitrate_samp", budget_options$label[10]),
                                             #          conditionalPanel("input.nitrate_samp",
                                             #                           radioButtons("nnitrate_samp", "Frequency per week", choices = c(1, 2, 7), inline = TRUE)
                                             #          ),
                                             #          p(tags$b("Laboratory Analysis")),
                                             #          checkboxInput("analyze_chla", budget_options$label[11]),
                                             #          conditionalPanel("input.analyze_chla",
                                             #                           radioButtons("nanalyze_chla", "Number of samples per week", choices = c(1, 2, 7), inline = TRUE)
                                             #          ),
                                             #          checkboxInput("analyze_nitrate", budget_options$label[12]),
                                             #          conditionalPanel("input.analyze_nitrate",
                                             #                           radioButtons("nanalyze_nitrate", "Number of samples per week", choices = c(1, 2, 7), inline = TRUE)
                                             #                           )
                                             #          ),
                                             #   column(8,
                                             #          h4("Budget plot"),
                                             #          wellPanel(
                                             #            plotOutput("budget_plot")
                                             #            ),
                                             #          h3("Budget summary"),
                                             #          DTOutput("budget_table"),
                                             #          wellPanel(
                                             #            textOutput("total_exp")
                                             #          ),
                                             #          actionButton("submit_budget", "Submit budget")
                                             #          )
                                             #   ),
                                             # hr(),
                                             # conditionalPanel("input.submit_budget == 0",
                                             #                  wellPanel(
                                             #                    p(tags$em("Submit your budget using the button above."))
                                             #                  )
                                             # ),
                                             # conditionalPanel("input.submit_budget > 0",
                                             #                  fluidRow(
                                             #                    column(10, offset = 1,
                                             #                           box(id = "box4", width = 12, status = "primary",
                                             #                               solidHeader = TRUE,
                                             #                               fluidRow(
                                             #                                 column(10, offset = 1,
                                             #                                        h3("Questions"),
                                             #                                        textAreaInput2(inputId = qid[49], label = quest[qid[49], ], width = "90%"),
                                             #                                        textAreaInput2(inputId = qid[50], label = quest[qid[50], ], width = "90%"),
                                             #                                        textAreaInput2(inputId = qid[51], label = quest[qid[51], ], width = "90%"),
                                             #                                 )
                                             #                                 )
                                             #                               )
                                             #                           )
                                             #                    )
                                             #                  ),
                                             # hr()
                                             ),
                                    #* Activity C - Summary ====
                                    tabPanel(title = tab_names["stab12", 2], value = "stab12",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab12", 2])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Completed Activity C!"),
                                                      p("This is the end of Activity C. Now you can generate your final report which will input all your answers and figures into a Microsoft Word document which you can download and submit to your instructor.")
                                               ),
                                               column(4,
                                                      h3("Generate Report"),
                                                      p("This will take the answers you have input into this app and generate a Microsoft Word document (.docx) document with your answers which you can download and make further edits before submitting."),
                                                      actionButton("generate2", "Generate Report (.docx)", icon = icon("file"), width = "190px", class = "btn-primary"
                                                                   # id = "dl_btn", # This is the only button that shows up when the app is loaded
                                                                   # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                      ), br(), br(),
                                                      tags$style(type="text/css", "#download2 {background-color:#579277;color: white}"),
                                                      conditionalPanel(condition = "output.reportbuilt2", # This button appears after the report has been generated and is ready for download.
                                                                       downloadButton("download2", "Download Report", width = "60px", style = "width:190px;"
                                                                       )
                                                      )
                                                      
                                               ),
                                               column(4,
                                                      h3(tags$b("Questions to be completed:")),
                                                      wellPanel(
                                                        htmlOutput("check_list2")
                                                      )
                                               )
                                             )
                                    )
                        )
               )
    ),
    # Tab navigation buttons ----
    br(), hr(),
    introBox(
      # h4("Use the buttons below to navigate through the tabs", align = "center"),
      box(width = 12, status = "success",
          solidHeader = TRUE,
          fluidRow(
            
            column(5, align = "center",
                   br(),
                   hover_action_button(
                     inputId = "prevBtn1",
                     label = "< Previous",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   bsTooltip("prevBtn1", title = "Navigate to previous tab", placement = "left", trigger = "hover"),
                   br(), br()
                   
            ),
            column(2, align = "center",
                   br(),
                   tags$style(type="text/css", paste0("#download_answers {background-color:#579277;color: white; padding:15px; font-size:18px;}")),
                   hover_download_button(outputId = "download_answers",
                                         label = "Save progress",
                                         class = "butt1",
                                         button_animation = "glow"),
                   br(), br()
            ),
            column(5, align = "center",
                   br(),
                   use_hover(popback = TRUE),
                   hover_action_button(
                     inputId = "nextBtn1",
                     label = "Next >",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   bsTooltip("nextBtn1", title = "Navigate to next tab", placement = "right", trigger = "hover"),
                   br(), br()
                   # )
            )
          )
      ), data.step = 3, data.intro = help_text["tab_nav2", 1], data.position = "right"
    ),
    hr(),
    fluidRow(
      column(8, offset = 1,
             br(),
             p(module_text["acknowledgement", ], id = "ackn"),
             p(app_update_txt, id = "ackn")
      )
    )
  )
}

shinyUI(ui)

# end
