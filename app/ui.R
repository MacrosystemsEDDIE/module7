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
               tabPanel(title = tab_names["mtab4", 2], value = "mtab4",
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
                        tabsetPanel(id = "tabseries1",
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
                                    tabPanel(title = "Objective 2 - Explore chlorophyll-a data", value = "obj2",
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
                                    tabPanel(title = "Objective 3 - Fit model", value = "obj3",
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
                                    tabPanel(title = "Objective 4 - Generate forecast", value = "obj4",
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
               tabPanel(title = tab_names["mtab5", 2], value = "mtab5",
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
                        tabsetPanel(id = "tabseries2",
                                    #* Objective 5 - Assimilate data ====
                                    tabPanel(title = "Objective 5 - Assimilate data", value = "obj5",
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
                                                      ),
                                                      box(id = "box2", width = 12, status = "warning",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h4("Important note!"),
                                                                   p("Depending on the lake you chose, you may see a plot that looks like this, where the lower end of the forecast distribution appears truncated:"),
                                                                   img(src = "zero_truncation_example.png", height = "100%", id = "bla_border",
                                                                       width = "100%", align = "center"),
                                                                   p("This is because our very simple forecast model may sometimes predict negative chlorophyll-a, which is not physically possible. If such a prediction is generated, it is set to 0. Alternative approaches could include choosing a different forecast model or log-transforming chlorophyll-a data before fitting a model. However, for our learning purposes today, we will continue with this simple approach.")
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
                                    tabPanel(title = "Objective 6 - Explore observation uncertainty", value = "obj6",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 6 - Explore observation uncertainty"),
                                                                p(id = "txt_j", module_text["obj_06", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h3("What is observation uncertainty?"),
                                                      h4(tags$em("Key terms:")),
                                                      tags$ul(
                                                        tags$li(tags$b("Observation uncertainty")," is the error associated with measurement of a variable. Importantly, ",tags$b("observation uncertainty is different from initial conditions uncertainty,")," which is uncertainty regarding the starting conditions of a model. Greater observation uncertainty can lead to greater initial conditions uncertainty."),
                                                        tags$li("A ",tags$b("sensor")," is a device that responds to a stimulus (such as heat, light, sound, pressure, magnetism, or motion) and transmits an electric impulse which is converted into a meaningful measurement for users.")
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("How much observation uncertainty is associatiated with chlorophyll-a measurements in lakes?"),
                                                      h4(tags$em("Different methods of collecting data have varying levels of observation uncertainty. Read the text and scroll through the slides below to learn about observation uncertainty in chl-a measurements."))
                                               ),
                                               column(4,
                                                      h4("In-Lake Sensor"),
                                                      p(tags$b("How it works:")),
                                                      p("Chlorophyll-a (chl-a) sensors send a beam of light into the water column and measure how much light is fluoresced back to the sensor by chlorophyll-a in phytoplankton cells. Fluorescence intensity is then converted to an estimate of chlorophyll-a concentration. Chlorophyll-a sensors can be deployed at a fixed depth in a lake or lowered through the water column on a cable."),
                                                      p(tags$b("Observation uncertainty:")),
                                                      p("Sensor accuracy may be affected by the presence of substances that alter water color, such as suspended sediment or dissolved organic matter. In addition, there is uncertainty associated with the equation that converts fluorescence to chlorophyll-a concentration."),
                                                      h4("Lab Analysis of Water Sample"),
                                                      p(tags$b("How it works:")),
                                                      p("Water samples are collected, processed, and inserted into a spectrophotometer that uses the sample's light absorption at a wavelength of 665 nm to estimate the chlorophyll-a concentration."),
                                                      p(tags$b("Observation uncertainty:")),
                                                      p("Water samples must be processed (transported back to the lab, filtered, and extracted using ethanol) before they are inserted into the spectrophotometer. Errors in processing may increase uncertainty in observations. In addition, there is uncertainty associated with the equation used to estimate chl-a concentration based on absorption.")
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        slickROutput("chla_obs_uc_slides", width = "800px", height = "450px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Make a prediction"),
                                                      p("We have explored the effect of data assimilation vs. no data assimilation on forecasts using an ensemble Kalman filter, which accounts for uncertainty in both model predictions and observations."),
                                                      p("Now, imagine that we have purchased a new water quality sensor, which takes incredibly precise chlorophyll-a measurements, thus decreasing our observation uncertainty. How might this decrease in observation uncertainty affect our forecasts?"),
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
                                                      actionButton("plot_low_ic","Generate distribution"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q33", 1]))
                                                            )
                                                          )
                                                      )
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
                                                               h4("Figure A. Forecasts assimilating data with low observation uncertainty"),
                                                               wellPanel(
                                                                 plotOutput("second_fc_low_obs_uc")
                                                               ),
                                                               downloadButton("save_second_fc_low_obs_uc_plot", "Download plot", icon = icon("download"))
                                                        ),
                                                        column(6,
                                                               h4("Figure B. Previous forecasts with data assimilation"),
                                                               wellPanel(
                                                                 plotOutput("second_fc_da_plot3")
                                                               )
                                                        )
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
                                                                   p(tags$b(quest["q34", 1])),
                                                                   p(tags$b(quest["q35", 1]))
                                                            )
                                                          )
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Make a second prediction"),
                                                      p("Now, imagine that our water quality sensor has malfunctioned (oh no!) leading to higher-than-normal observation uncertainty in our chlorophyll-a observations."),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q36", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      img(src = "fouled_EXO.png", height = "90%", id = "bla_border",
                                                          width = "90%", align = "center"),
                                                      p(tags$em("A fouled EXO water quality sensor. Photo credit: Adrienne Breef-Pilz"))
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Generate initial condition distribution with high observation uncertainty"),
                                                      p("Let's plot an initial conditions distribution with high uncertainty."),
                                                      p("We will also plot the original initial condition you generated in Objective 4 for comparison."),
                                                      actionButton("plot_high_ic","Generate distribution"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q37", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(4,
                                                      h4("Initial condition distribution with high observation uncertainty"),
                                                      wellPanel(
                                                        plotlyOutput("ic_distrib_high_plot")
                                                      )
                                               ),
                                               column(4,
                                                      h4("Original initial condition distribution generated in Objective 4"),
                                                      wellPanel(
                                                        plotlyOutput("ic_distrib_plot3")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Compare forecasts made with high observation uncertainty to previous forecasts"),
                                                      p("Now, we will replicate the two-forecast plot with data assimilation that we created above, but this time with higher observation uncertainty."),
                                                      actionButton("plot_fc_high_obs_uc","Plot forecasts with high observation uncertainty"),
                                                      fluidRow(
                                                        column(6,
                                                               h4("Figure C. Forecasts assimilating data with high observation uncertainty"),
                                                               wellPanel(
                                                                 plotOutput("second_fc_high_obs_uc")
                                                               ),
                                                               downloadButton("save_second_fc_high_obs_uc_plot", "Download plot", icon = icon("download"))
                                                        ),
                                                        column(6,
                                                               h4("Figure D. Previous forecasts with data assimilation"),
                                                               wellPanel(
                                                                 plotOutput("second_fc_da_plot4")
                                                               )
                                                        )
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
                                                                   p(tags$b(quest["q38", 1])),
                                                                   p(tags$b(quest["q39", 1]))
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
                                                      p("We will explore the effect of changes in data assimilation frequency on forecast output and forecast accuracy.")
                                               )
                                             )
                                    ),
                                    #* Objective 7 - Explore data assimilation frequency ====
                                    tabPanel(title = "Objective 7 - Explore data assimilation frequency", value = "obj7",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 7 - Explore data assimilation frequency"),
                                                                p(id = "txt_j", module_text["obj_07", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h3("How often can we assimilate chlorophyll-a data into forecasts?"),
                                                      h4(tags$em("The availability of data for assimilation into forecasts depends on both",tags$b("data frequency")," and ",tags$b("data latency."))),
                                                      tags$ul(
                                                        tags$li(tags$b("Data frequency")," is the amount of time that passes between measurements."),
                                                        tags$li(tags$b("Data latency")," is the time between when a measurement is made and when it is available for assimilation into forecasts.")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3(""),
                                                      h4(tags$em("Different methods of collecting data have varying levels of data frequency and latency. Read the text and scroll through the slides below to learn about data frequency and latency of chl-a measurements."))
                                               ),
                                               column(4,
                                                      h4("In-Lake Sensor"),
                                                      p(tags$b("Frequency:")),
                                                      p("If deployed in a lake, chlorophyll-a sensor data can be collected at a ",tags$b("high data frequency")," (e.g., every minute or even more often). Otherwise, data are collected however frequently a human travels to the lake to collect data."),
                                                      p(tags$b("Latency:")),
                                                      p("Chlorophyll-a sensor data may be streamed wirelessly from the lake to a computer, resulting in ",tags$b("low data latency.")," Otherwise, a human must travel to the lake, download the sensor data, and upload it to a computer, increasing latency."),
                                                      h4("Lab Analysis of Water Sample"),
                                                      p(tags$b("Frequency:")),
                                                      p("Typically, water samples are not collected every day as it requires a human to travel to the lake and take measurements. As a result, ",tags$b("the frequency of these data can be highly variable,")," ranging from multiple times a week to once a year."),
                                                      p(tags$b("Latency:")),
                                                      p("It usually takes at least a week to conduct laboratory analyses of water samples for chlorophyll-a, and the process can be much longer (e.g., several weeks or months) depending on the resources available to the researcher, resulting in ",tags$b("high data latency."))
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        slickROutput("chla_frequency_slides", width = "800px", height = "450px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("How does the frequency of data assimilation affect forecast performance?"),
                                                      p("We are going explore the effect of data assimilation on forecast accuracy - does going to the effort of collecting lots of data and assimilating it into our predictions really improve our forecasts?"),
                                                      p("To answer this question, we will generate multiple series of one-day-ahead forecasts over a period of 10 days. For each series of forecasts, we will assimilate data at different time intervals (one series with no data assimilation, one series with weekly data assimilation, and one series with daily data assimilation) and compare the accuracy of the resulting forecasts."),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q40", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      img(src = "sampling.png", height = "90%", id = "bla_border",
                                                          width = "90%", align = "center"),
                                                      p(tags$em("Members of the Virginia Tech Reservoir Group sampling water quality. Photo credit: Cayelan Carey"))
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Forecasts with no data assimilation"),
                                                      p("Click the button below to generate a series of 10, 1-day-ahead forecasts with no data available for assimilation during the forecast period."),
                                                      actionButton("fc_series_no_da","Run forecasts with no data assimilation"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q41", 1])),
                                                                   p(tags$b(quest["q42", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(8,
                                                      wellPanel(
                                                        plotOutput("fc_series_no_da_plot")
                                                      ),
                                                      downloadButton("save_fc_series_no_da_plot", "Download plot", icon = icon("download")),
                                                      conditionalPanel("input.fc_series_no_da > 0",
                                                                       checkboxInput("show_obs", "Click to show/remove the observations during the forecast period.", value = FALSE),
                                                      )
                                                      )
                                             ),
                                             br(),
                                             fluidRow(
                                               column(6,
                                                      h4(tags$em("Assess series of 1-day-ahead forecasts with no data assimilation")),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q43", 1])),
                                                                   p(tags$b(quest["q44", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      br(),br(),
                                                      fluidRow(
                                                        column(6,
                                                               actionButton("calc_bias2",label = "Calculate bias")
                                                        ),
                                                        column(6,
                                                               wellPanel(
                                                                 htmlOutput("out_bias2")
                                                               )
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               actionButton("calc_rmse2",label = "Calculate RMSE")
                                                        ),
                                                        column(6,
                                                               wellPanel(
                                                                 htmlOutput("out_rmse2")
                                                               )
                                                        )
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Forecasts with weekly data assimilation"),
                                                      p("Click the button below to generate a series of 10, 1-day-ahead forecasts with weekly data available for assimilation during the forecast period."),
                                                      actionButton("fc_series_weekly","Run forecasts with weekly data assimilation"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q45", 1])),
                                                                   p(tags$b(quest["q46", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        plotOutput("fc_series_weekly_plot")
                                                      ),
                                                      downloadButton("save_fc_series_weekly_plot", "Download plot", icon = icon("download")),
                                                      conditionalPanel("input.fc_series_weekly > 0",
                                                                       checkboxInput("show_obs2", "Click to show/remove the observations during the forecast period.", value = FALSE),
                                                      )
                                               )
                                             ),
                                             br(),
                                             fluidRow(
                                               column(6,
                                                      h4(tags$em("Assess series of 1-day-ahead forecasts with weekly data assimilation")),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q47", 1])),
                                                                   p(tags$b(quest["q48", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      br(),br(),
                                                      fluidRow(
                                                        column(6,
                                                               actionButton("calc_bias3",label = "Calculate bias")
                                                        ),
                                                        column(6,
                                                               wellPanel(
                                                                 htmlOutput("out_bias3")
                                                               )
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               actionButton("calc_rmse3",label = "Calculate RMSE")
                                                        ),
                                                        column(6,
                                                               wellPanel(
                                                                 htmlOutput("out_rmse3")
                                                               )
                                                        )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Forecasts with daily data assimilation"),
                                                      p("Click the button below to generate a series of 10, 1-day-ahead forecasts with daily data available for assimilation during the forecast period."),
                                                      actionButton("fc_series_daily","Run forecasts with daily data assimilation"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q49", 1])),
                                                                   p(tags$b(quest["q50", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        plotOutput("fc_series_daily_plot")
                                                      ),
                                                      downloadButton("save_fc_series_daily_plot", "Download plot", icon = icon("download")),
                                                      conditionalPanel("input.fc_series_daily > 0",
                                                                       checkboxInput("show_obs3", "Click to show/remove the observations during the forecast period.", value = FALSE),
                                                      )
                                               )
                                             ),
                                             br(),
                                             fluidRow(
                                               column(6,
                                                      h4(tags$em("Assess series of 1-day-ahead forecasts with daily data assimilation")),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q51", 1])),
                                                                   p(tags$b(quest["q52", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      br(),br(),
                                                      fluidRow(
                                                        column(6,
                                                               actionButton("calc_bias4",label = "Calculate bias")
                                                        ),
                                                        column(6,
                                                               wellPanel(
                                                                 htmlOutput("out_bias4")
                                                               )
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(6,
                                                               actionButton("calc_rmse4",label = "Calculate RMSE")
                                                        ),
                                                        column(6,
                                                               wellPanel(
                                                                 htmlOutput("out_rmse4")
                                                               )
                                                        )
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
                                                                   p(tags$b(quest["q53", 1]))
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
                                                      p("You will complete a scenario exercise to make management decisions using forecasts generated with different frequencies of data assimilation.")
                                                      )
                                               )
                                             )
                        )
               ),
               # 7. Activity C ----
               tabPanel(title = tab_names["mtab6", 2], value = "mtab6",
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
                        tabsetPanel(id = "tabseries3",
                                    #* Objective 8 - Management Scenario ====
                                    tabPanel(title = "Objective 8 - Management scenario", value = "obj8",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 8 - Management scenario"),
                                                                p(id = "txt_j", "Make management decisions using ecological forecasts generated with different levels of observation uncertainty and different frequencies of data assimilation."))
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Scenario: Mitigating toxin exposure risk at Green Reservoir"),
                                                      p("Green Reservoir is a popular recreational reservoir for fishing, swimming, and boating. Periodically, the lake experiences harmful algal blooms that may pose a threat to lake users due to algal toxin exposure."),
                                                      p("The management authority for Green Reservoir has a ",tags$b("water quality threshold of 10 ug/L for chlorophyll-a.")," If chlorophyll-a surpasses this threshold, the lake must be closed for recreation. If the lake is closed unnecessarily, lake users become frustrated and make complaints to the management authority. But if the lake is not closed and chlorophyll-a levels surpass the water quality threshold, the management authority is putting lake users at risk of exposure to algal toxins."),
                                                      p("Currently, managers at Green Reservoir issue a daily, 1-day-ahead forecast of lake chlorophyll-a levels, which are informed by weekly observations of chlorophyll-a that are obtained by managers manually collecting a water sample and analyzing it in the water quality lab."),
                                                      p("The Green Reservoir management authority is exploring the idea of purchasing a new water quality sensor that will automatically collect daily chlorophyll-a measurements to inform their forecasting system. Chlorophyll-a sensors are expensive (~15K USD), and the authority wants to know if this substantial investment will result in improvements in forecast accuracy."),
                                                      ),
                                               column(6,
                                                      img(src = "Objective10_Scenario1.jpg", height = "80%", id = "bla_border",
                                                          width = "80%", tags$style("border: solid 2px black;")),
                                                      p("Algal Bloom - Kelly's Island, Lake Erie by NOAA Great Lakes Environmental Research Laboratory")
                                                      )
                                               
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Make a preliminary recommendation"),
                                                      p("As a forecasting expert, you have been brought in as a consultant to help the Green Reservoir management authority forecasting team decide whether to invest in a new high-frequency sensor. Luckily, the management authority has been able to temporarily borrow a high-frequency sensor - similar to the one they wish to purchase - from a neighboring locality that you may use on a trial basis to help make your recommendation."),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q54", 1]))
                                                            )
                                                          )
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Trial the high-frequency sensor"),
                                                      p("Before making your final recommendation to the Green Reservoir management authority, you decide to run some trials with the high-frequency sensor that the authority has on loan. You deploy the high-frequency sensor in Green Reservoir for one week, and then compare forecasts made using the management authority's current system with forecasts made using the high-frequency sensor.")
                                                      )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h4("Forecasts using the current data collection method"),
                                                      p("First, you explore the current forecasting system operated by the management authority. You generate a series of seven, 1-day-ahead forecasts that are only informed by a single, weekly observation from Oct. 4, which was collected manually by water authority personnel. This is the only observation that will be assimilated during the forecast period. Because Saturdays are the most popular day for recreation at the lake, ",tags$b("you will focus on the forecast generated for Saturday, October 11")," as you assess your series of forecasts."),
                                                      p("Click the button below to generate forecasts informed by a single observation on Oct. 4."),
                                                      actionButton("fc_scenario_weekly","Run forecasts with weekly data assimilation"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q55", 1]))
                                                            )
                                                          )
                                                      )
                                                      ),
                                               column(6,
                                                      br(),br(),
                                                      wellPanel(
                                                        plotOutput("fc_scenario_weekly_plot")
                                                      ),
                                                      downloadButton("save_fc_scenario_weekly_plot", "Download plot", icon = icon("download"))
                                                      )
                                             ),
                                             br(),
                                             fluidRow(
                                               column(6,
                                                      h4("Forecasts using the borrowed high-frequency sensor"),
                                                      p("For comparison, you then generate the same series of forecasts using data collected by the high-frequency sensor you deployed on a trial basis in Green Reservoir."),
                                                      p("Click the button below to generate forecasts informed by a daily observations from the high-frequency sensor."),
                                                      actionButton("fc_scenario_daily","Run forecasts with daily data assimilation"),
                                                      br(),br(),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q56", 1]))
                                                            )
                                                          )
                                                      ),
                                                      box(id = "box2", width = 12, status = "warning",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   br(),
                                                                   p(tags$b("Note: you may be wondering why the forecast and initial conditions distributions do not line up at all with some of the observations on this plot. In fact, this is a great illustration of one of the weaknesses of the ensemble Kalman filter - if the observation for a particular day falls completely outside the forecast distribution for that day, it is not always possible for the ensemble Kalman filter to adjust the initial condition distribution enough to match the observation!"))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      br(),br(),
                                                      wellPanel(
                                                        plotOutput("fc_scenario_daily_plot")
                                                      ),
                                                      downloadButton("save_fc_scenario_daily_plot", "Download plot", icon = icon("download"))
                                               )
                                             ),
                                             br(),
                                             fluidRow(
                                               column(12,
                                                      h4("Visually assess forecasts"),
                                                      p("Finally, you assess both series of forecasts using an observation of chlorophyll-a made on Saturday, October 11. Let's plot the results."),
                                                      actionButton("fc_compare","Compare forecasts made with and without high-frequency sensor")
                                                      )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h4(tags$em("Forecasts without high-frequency sensor")),
                                                      wellPanel(
                                                        plotOutput("fc_scenario_weekly_plot2")
                                                      )
                                                      ),
                                               column(6,
                                                      h4(tags$em("Forecasts with high-frequency sensor")),
                                                      wellPanel(
                                                        plotOutput("fc_scenario_daily_plot2")
                                                      )
                                                      )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q57", 1])),
                                                                   p(tags$b(quest["q58", 1]))
                                                            )
                                                          )
                                                      )
                                                      )
                                             ),
                                             br(),
                                             fluidRow(
                                               column(12,
                                                      h4("Calculate forecast assessment metrics"),
                                                      p("In addition to visual inspection of the forecast for Saturday, Oct. 11, you also calculate bias and RMSE for both series of forecasts for all the days from Oct. 4 to Oct. 11."),
                                                      p("Click the buttons below to calculate bias and RMSE for forecasts generated with and without data from the high-frequency sensor.")
                                                      )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h4(tags$em("Assessment metrics for forecasts without high-frequency sensor")),
                                                      wellPanel(
                                                        fluidRow(
                                                          column(6,
                                                                 actionButton("calc_bias5","Calculate bias"),
                                                                 br(),br(),
                                                                 actionButton("calc_rmse5","Calculate RMSE")
                                                                 ),
                                                          column(6,
                                                                 htmlOutput("out_bias5"),
                                                                 br(),br(),
                                                                 htmlOutput("out_rmse5")
                                                                 )
                                                        )
                                                      )
                                                      ),
                                               column(6,
                                                      h4(tags$em("Assessment metrics for forecasts without high-frequency sensor")),
                                                      wellPanel(
                                                        fluidRow(
                                                          column(6,
                                                                 actionButton("calc_bias6","Calculate bias"),
                                                                 br(),br(),
                                                                 actionButton("calc_rmse6","Calculate RMSE")
                                                          ),
                                                          column(6,
                                                                 htmlOutput("out_bias6"),
                                                                 br(),br(),
                                                                 htmlOutput("out_rmse6")
                                                          )
                                                        )
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Make a final recommendation"),
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q59", 1])),
                                                                   p(tags$b(quest["q60", 1])),
                                                                   p(tags$b(quest["q61", 1]))
                                                            )
                                                          )
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4, offset = 1,
                                                      h2("Data & Forecasts"),
                                                      p("We have now explored how data assimilation can be used to improve the accuracy of ecological forecasts. As you can see, the effect of assimilating data depends on the frequency of data assimilation and the observation uncertainty of the available data. In addition, while having high-frequency data for assimilation often improves forecast accuracy, having more data does not guarantee that forecasts will be accurate enough to guide management decision-making. Other factors, such as the quality of the forecast model or the intrinsic predictability of the forecasted variable, are equally important to providing actionable forecasts.")
                                               ),
                                               column(5, offset = 1,
                                                      br(), br(), br(),
                                                      img(src = "mod7_conceptual_figure.png", height = "80%",
                                                          width = "80%", align = "left", alt = "A conceptual figure showing data from a lake buoy being used to update a forecast.")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h2("Completed Module!"),
                                                      p("You have completed the module! Congratulations! Please check through the answers in your final report and be sure you have copy-pasted in all the required plots before you submit your report to your instructor."),
                                                      p("Your instructor may ask you to prepare a short presentation of your forecasts with assimilated data (Objective 7) to share with other students in the class. This will enable you to compare the effects of data assimilation on forecasts among lakes in different eco-regions."),
                                                      p("You’ve now made forecasts informed with data - well done! If you are interested in learning more about how forecasts are generated, forecast uncertainty, and use of forecasts for decision-making, we encourage you to visit other Macrosystems EDDIE modules:"),
                                                      fluidRow(
                                                        column(10, align = "left",
                                                               box(id = "box15", width = 12, status = "primary",
                                                                   solidHeader = TRUE,
                                                                   fluidRow(
                                                                     column(10, offset = 1,
                                                                            h3("Additional Macrosystems EDDIE Ecological Forecasting Modules")
                                                                     )
                                                                   ),
                                                                   fluidRow(
                                                                     column(3, offset = 1,
                                                                            img(src = "mod5_viz_v2.png", height = "100%",
                                                                                width = "100%", align = "left", alt = "A conceptual figure of the steps in the forecast cycle.")
                                                                     ),
                                                                     column(7, offset = 0,
                                                                            p(style="text-align: justify;", tags$a(href = "https://macrosystemseddie.shinyapps.io/module5/", "Macrosystems EDDIE Module 5: Introduction to Ecological Forecasting:"),"This module will introduce students to the basic components of an ecological forecast; how a simple forecasting model is constructed; how changes to model inputs affect forecast uncertainty; and how productivity forecasts vary across ecoclimatic regions.")
                                                                     )
                                                                   ),
                                                                   hr(),
                                                                   fluidRow(
                                                                     column(3, offset = 1,
                                                                            img(src = "mod6_conceptual_fig.png", height = "100%",
                                                                                width = "100%", align = "left", alt = "A diagram showing a forecast with uncertainty.")
                                                                     ),
                                                                     column(7, offset = 0,
                                                                            p(style="text-align: justify;", tags$a(href = "https://macrosystemseddie.shinyapps.io/module6/", "Macrosystems EDDIE Module 6: Understanding Uncertainty in Ecological Forecasts:"),"This module will introduce students to the concept of uncertainty within an ecological forecast; where uncertainty in a forecast comes from; how uncertainty can be quantified within a forecast; and how uncertainty can be managed.")
                                                                     )
                                                                   ),
                                                                   hr(),
                                                                   fluidRow(
                                                                     column(3, offset = 1,
                                                                            img(src = "Mod8_conceptual_fig.png", height = "100%",
                                                                                width = "100%", align = "left", alt = "A conceptual figure showing how forecasts can be visualized, communicated, and used for decision-making.")
                                                                     ),
                                                                     column(7, offset = 0,
                                                                            p(style="text-align: justify;", tags$a(href = "https://macrosystemseddie.shinyapps.io/module8/", "Macrosystems EDDIE Module 8: Using Ecological Forecasts to Guide Decision Making:"),"This module will teach students the basic components of an ecological forecast; how to connect forecast visualizations to forecast user needs for aiding decision-making; and to create their own visualizations of probabilistic forecasts of ecological variables for a specific forecast user.")
                                                                     )
                                                                   ),
                                                                   hr()
                                                                   )
                                                               )
                                                        )
                                                      )
                                               )
                                             ) #end tab panel
                        )
               )
    ),
    # Tab navigation buttons ----
    br(), hr(),
    useShinyjs(),
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
