# Load required libraries
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE))
suppressPackageStartupMessages(library(shinyBS, quietly = TRUE))
suppressPackageStartupMessages(library(shinydashboard, quietly = TRUE))
suppressPackageStartupMessages(library(rintrojs, quietly = TRUE))
suppressPackageStartupMessages(library(slickR, quietly = TRUE))
suppressPackageStartupMessages(library(sortable, quietly = TRUE))
suppressPackageStartupMessages(library(ncdf4, quietly = TRUE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE))
suppressPackageStartupMessages(library(stringr, quietly = TRUE))
suppressPackageStartupMessages(library(hover, quietly = TRUE))

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)

# colors for theme
obj_bg <- "#D4ECE1"
ques_bg <- "#B8E0CD"
nav_bg <- "#DDE4E1"
nav_butt <- "#31ED92"
nav_txt <- "#000000" # white = #fff; black = #000000
slider_col <- "#2CB572"

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
    # tags$head(includeHTML(("google-analytics.html"))),
    fluidPage(
      column(1, offset = 11, align = "right",
             introBox(
               actionButton("help", label = "", icon = icon("question-circle")), data.step = 7, data.intro = help_text["help", 1]
             )
      )
    ),
    navbarPage(title = "Module 7: Using Data to Improve Ecological Forecasts",
               position = "static-top", id = "maintab",
               tags$header(
                 fluidRow(
                   column(2,
                          fileInput("upload_answers", "Resume Progress", accept = c(".eddie", ".rds"))
                   ),
                   column(2,
                          actionButton("help2", label = "", icon = icon("question-circle"))
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
                          p("To be useful for management, ecological forecasts need to be both accurate enough for managers to be able to rely on them for decision-making",tags$i("and"), " include a representation of forecast uncertainty, so managers can properly interpret the probability of future events. To improve forecast accuracy, we can update forecasts with observational data once they become available, a process known as", tags$b("data assimilation"),". Recent improvements in environmental sensor technology and an increase in the number of sensors deployed in ecosystems have resulted in an increase in the availability of data for assimilation to help develop and improve forecasts for natural resource management."),
                          p("One important test case in which data assimilation may greatly improve the use of ecological forecasts for management is algal blooms in lakes. Algal blooms are increasing in frequency and severity across many freshwater lakes and can substantially impact water quality. Forecasts of the likelihood of future algal blooms may be useful tools in helping water resource managers to mitigate the effect of these blooms. Forecasts can give managers time to take preemptive actions to prevent blooms, such as applying algaecide, or to make plans to reduce the bloom's impact on human health, such as closing recreational beaches."),
                          p("In this module, we will address the following question: ",tags$b(tags$i("How can we use data to improve a forecast of lake algal biomass?"))),
                          p("We will address this question by building an ecological model to predict chlorophyll-a (a metric of algal biomass and primary productivity), generating ecological forecasts, and using data science approaches to integrate the most recently collected data into a forecast, a process known as data assimilation. Assimilating the most recent observations into a forecast model allows the forecaster to update the initial conditions, or the starting conditions, of the model, with the goal of improving forecast accuracy. For example, if our task is to generate a forecast of chlorophyll-a concentrations in a lake for tomorrow, it is likely that our forecast will be more accurate if we use today’s measurement of chlorophyll-a as the initial condition for our model, rather than last week’s measurement."),
                          p("We will explore how assimilating different types of data (e.g., chlorophyll-a, nutrient concentrations, water temperature) at different temporal frequencies (e.g., daily, weekly) affects forecast output. Finally, we will assimilate different types of data into forecasts and examine how data assimilation affects water resource management decisions."),
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
               ),
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
                          column(4, offset = 1,
                                 h3("Workflow for this module"),
                                 tags$ol(
                                   tags$li(id = "txt_j", module_text["workflow1", ]),
                                   tags$li(id = "txt_j", module_text["workflow2", ]),
                                   tags$li(id = "txt_j", module_text["workflow3", ]),
                                   tags$li(id = "txt_j", module_text["workflow4", ])
                                 )
                          ),
                          column(6, align = "center", offset = 1,
                                 br(), br(),
                                 img(src = "Mod7_Introduction.png", height = "80%", id = "bla_border",
                                     width = "80%", tags$style("border: solid 2px black;"))
                                 
                          )
                        ), hr(),
                        fluidRow(
                          column(7, offset = 1,
                                 h3("Student Activities"),
                                 p(module_text["student_activities", ]),
                                 box(width = 12, status = "warning",
                                     solidHeader = TRUE,
                                     p(tags$b("WARNING:"), " The Shiny app will disconnect from the server if it is left idle for 10 minutes. If this happens you will lose all your inputs into the app. It is recommended to download the user input at the end of the class, but you can also download throughout the class."),
                                 ),
                                 p("Alternatively, you can download the questions as a Word (.docx) file  and record your answers there. If you opt for this option, you can hide the green question boxes by unchecking the box below."),
                                 checkboxInput("show_q1", "Show questions", value = TRUE),
                                 tags$style(type="text/css", "#stud_dl {background-color:#579277;color: white}"),
                                 conditionalPanel("output.handoutbuilt",
                                                  downloadButton(outputId = "stud_dl", label = "Download Student Handout"),
                                 )
                          ),
                        ), hr(),
                        #* Generate report buttons ====
                        fluidRow(
                          column(4,offset = 1,
                                 h3("Saving your progress"),
                                 p(id = "txt_j", "If you run out of time to finish all the activities you can save your progress and return to it at a later date. Click the 'Save Progress' button at the bottom of the page and a file 'module7_answers_ID_number.eddie' will download. Store this file in a safe place locally on your computer."),
                                 img(src = "save_button.png", height = "100%", id = "bla_border",
                                     width = "100%", tags$style("border: solid 2px black;")),
                                 br(),
                                 hr(),
                                 h3("Resuming your progress"),
                                 img(src = "resume_button.png", height = "100%", id = "bla_border",
                                     width = "100%", tags$style("border: solid 2px black;")),
                                 br(),
                                 p(id = "txt_j", "To reload the app input you can upload the downloaded '.eddie' file at the top of this pae and it will populate your answers into the Shiny app."),
                                 p(id = "txt_j", HTML(paste0(tags$b("Note:"), " You will need to navigate to tabs Objective 1 in Activity A after uploading your file for the site selection to load there."))),
                                 p(id = "txt_j", "Currently the plots do not save to the file.  If you generated plots during your last session, you will need to reload the data and reproduce the plots before generating your report.")
                          ),
                          column(4, offset = 1,
                                 introBox(
                                   h3("Generate Report"),
                                   p(module_text["generate_report", ]),
                                   actionButton("generate", "Generate Report (.docx)", icon = icon("file"), width = "190px", class = "btn-primary"
                                   ), br(), br(),
                                   data.step = 6, data.intro = help_text["finish", 1]
                                 ),
                                 tags$style(type="text/css", "#download {background-color:#579277;color: white}"),
                                 conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                                                  downloadButton("download", "Download Report", width = "60px", style = "width:190px;"
                                                  )), br(),
                                 h5(tags$b("Questions still to be completed:")),
                                 wellPanel(
                                   htmlOutput("check_list")
                                 )
                          )
                        ),
                        fluidRow(
                          hr(),
                          column(10, align = "left",
                                 box(id = "box1", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(8, offset = 1,
                                              h3("Before you start..."),
                                              p("Input your name and Student ID and this will be added to your final report."),
                                              textInput("name", "Name:"),
                                              textInput("id_number", "ID number:"),
                                              introBox(
                                                h3(tags$b("Think about it!")),
                                                p("Note: The size of these text boxes can be adjusted by clicking and dragging the bottom right of the text box."),
                                                textAreaInput2(inputId = qid[1], label = quest[qid[1], 1]),
                                                textAreaInput2(inputId = qid[2], label = quest[qid[2], 1]),
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
                                 p(HTML(paste0('This module will introduce key concepts within Ecological forecasting through exploration of ', a(href = "https://www.neonscience.org/", "NEON (National Ecological Observation Network) data", target = "_blank"), ", building a model, and then generating a short-term ecological forecast.")))
                          ),
                          column(6, align = "center",
                                 a(
                                   href = "https://www.neonscience.org/",
                                   img(src = "NSF-NEON-logo.png", title = "NEON - NSF logo"), target = "_blank"
                                 )
                          )
                        )
               ),
               
               # 4. Site Selection ----
               tabPanel(title = tab_names["mtab4", 2], value = "mtab4",
                        tags$style(".nav-tabs {
  background-color: #DDE4E1;
  border-color: #FFF;

}

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: #4D6A5C;
border-color: #FFF;
}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #FFF;
    background-color: #4D6A5C;
}"),
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 h3("Site Selection: Select a NEON site and visualize the data"),
                                 p("Complete Objectives 1-2 to familiarize yourself with the data from your selected site and learn about the data you will be using.")
                          )
                        ),
                        
                        #* Objective 1 - Select and view site ====
                        tabsetPanel(id = "tabseries1",
                                    tabPanel(title = tab_names["stab1", 2],
                                             value = "stab1", id = "wh_link",
                                             tags$style("outline: 5px dotted green;"),
                                             # introBox(
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
                                                                                        onInitialize = I('function() { this.setValue(""); }')),
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
                                               ),
                                             ),
                                             fluidRow(
                                               column(10, align = "left",
                                                      box(id = "box3", width = 10, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(7, offset = 1,
                                                                   h3("Questions"),
                                                                   h4(quest[qid[3], 1],tags$b("If the information for your lake is not on the NEON website then you can input NA (Not Available) into the text box."))
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   textInput(inputId = qid[4], label = quest[qid[4], 1] , width = "90%"),
                                                                   textInput(inputId = qid[5], label = quest[qid[5], 1], width = "90%"),
                                                                   textInput(inputId = qid[6], label = quest[qid[6], 1], width = "90%")
                                                            ),
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   textInput(inputId = qid[7], label = quest[qid[7], 1] , width = "90%"),
                                                                   textInput(inputId = qid[8], label = quest[qid[8], 1], width = "90%"),
                                                                   textInput(inputId = qid[9], label = quest[qid[9], 1], width = "90%")
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
                                                      p("We will explore the data which has been measured at this site by NEON."))
                                             )
                                    ),
                                    #* Objective 2 - Explore data ----
                                    tabPanel(title = tab_names["stab2", 2],  value = "stab2",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab2", 2]),
                                                                p("If there are some variables which you are not familiar with, visit the ", a(href = "https://data.neonscience.org/home", "NEON Data Portal", target = "_blank"), "and click 'Explore Data Products' to learn more about how the data are collected.")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(8, offset = 2,
                                                      h3("Variable descriptions"),
                                                      DT::DTOutput("var_desc")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               #** Data Table ----
                                               column(4,
                                                      h3("Data Table"),
                                                      p("This is a Shiny data table. It is interactive and allows you to navigate through the data table by searching or clicking through the different pages."),
                                                      DT::DTOutput("neon_datatable")
                                               ),
                                               #** Plot of data ----
                                               column(8,
                                                      h3("Data Plot"),
                                                      p("All plots in this Shiny app are generated using Plotly. This allows you to hover your mouse over the plot to get information from each of the plots. You can inspect the data closely by clicking and zooming into particular areas. There is a tool box at the top of the plot which has the selection function required for Q6."),
                                                      selectizeInput("view_var", "Select variable",
                                                                     choices = unique(neon_vars$Short_name),
                                                                     options = list(
                                                                       placeholder = 'Please select a variable',
                                                                       onInitialize = I('function() { this.setValue(""); }')),
                                                      ),
                                                      plotlyOutput("var_plot"),
                                                      useShinyjs(),  # Set up shinyjs
                                                      wellPanel(
                                                        h4("Variable Description"),
                                                        textOutput("txt_out")
                                                      )
                                               )
                                             ), hr(),
                                             fluidRow(
                                               column(10, offset = 1,
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   h5(tags$b(quest["q4", 1])), 
                                                                   box(width = 10, status = "warning",
                                                                       solidHeader = TRUE,
                                                                       p(tags$b("WARNING:"), " Careful! If you delete variables from the data table by deleting text in the left column, the server will disconnect and you will lose any responses entered since the last time you saved your progress!"),
                                                                   ),
                                                                   DTOutput("q4_tab"),
                                                                   bsTooltip("q4_tab", title = "Double click the cell to edit", placement = "top", trigger = "hover"),
                                                                   br(),
                                                                   textAreaInput2(inputId = qid[11], label = quest[qid[11], ], width = "90%")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will build models that will allow us to predict water temperature and underwater light."))
                                             )
                                    ),
                                    #* Objective 3 - Explore variable relationships ====
                                    tabPanel(title = tab_names["stab3", 2], value = "stab3",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab3", 2]),
                                                                p(id = "txt_j", module_text["obj_03", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               #** Data Table ----
                                               column(4,
                                                      h3("Investigate variable relationships"),
                                                      p("For Q.6 you will keep 'Chlorophyll-a' as the y-variable and explore its relationship with the other variables at this site."),
                                                      selectizeInput("y_var", "Select Y variable",
                                                                     choices = unique(neon_vars$Short_name),
                                                                     options = list(
                                                                       placeholder = 'Please select a variable',
                                                                       onInitialize = I('function() { this.setValue("Chlorophyll-a"); }'))),
                                                      selectizeInput("x_var", "Select X variable",
                                                                     choices = unique(neon_vars$Short_name),
                                                                     options = list(
                                                                       placeholder = 'Please select a variable',
                                                                       onInitialize = I('function() { this.setValue(""); }'))),
                                                      #p("While for Q.7, you can select any two variables and investigate if there is any relationship. e.g. air temperature and surface water temperature")
                                               ),
                                               #** Plot of data ----
                                               column(6,
                                                      h3("Comparison Plot"),
                                                      wellPanel(
                                                        plotlyOutput("xy_plot")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               hr(),
                                               column(10, align = "left",
                                                      box(id = "box5", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest[qid[12], ])),
                                                                   textAreaInput2(inputId = qid[13], label = quest[qid[13], ],
                                                                                  width = "90%"),
                                                                   textAreaInput2(inputId = qid[14], label = quest[qid[14], ],
                                                                                  width = "90%"),
                                                                   textAreaInput2(inputId = qid[15], label = quest[qid[15], ],
                                                                                  width = "90%"),
                                                                   textAreaInput2(inputId = qid[16], label = quest[qid[16], ],
                                                                                  width = "90%"),
                                                                   # textAreaInput2(inputId = qid[17], label = quest[qid[17], ],
                                                                   #                width = "90%"),
                                                                   br(),
                                                                   actionButton("submitButtonQ6", "Submit Q.6 answers"),
                                                                   br(),
                                                                   conditionalPanel("input.submitButtonQ6 > 0",
                                                                                    p(tags$i("Thanks for submitting your answers! They've been used to populate a table for a question later on in Activity A.")),
                                                                                    br()),
                                                                   br()
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #*** Next step ----
                                             fluidRow(
                                               column(12,
                                                      h3("Next step"),
                                                      p("Next we will use these data and the identified related variables to help build our ecological model.")
                                               )
                                             )
                                    )
                        )
               ),
               
               # 5. Activity A ----
               tabPanel(title = tab_names["mtab5", 2], value = "mtab5",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity A - Build A Model With Uncertainty"),
                                           p(module_text["act_A", ])
                                 )
                          ),
                        ),
                        tabsetPanel(id = "tabseries2",
                                    #* Objective 4 - Understand model ====
                                    tabPanel(title = tab_names["stab4", 2], value = "stab4",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab4", 2]),
                                                                p(id = "txt_j", module_text["obj_04", ])
                                                      )
                                               )
                                             ),
                                             #* Intro text ====
                                               #** NEON Intro ----
                                             fluidRow(
                                               column(4,
                                                      h3("What is a Model?"),
                                                      h4("Read through this section and scroll through the slides"),
                                                      p(id = "txt_j", module_text["model1", ]),
                                                      p(id = "txt_j", module_text["model2", ]),
                                                      p(id = "txt_j", module_text["model3", ]),
                                                      p(id = "txt_j", module_text["mod_desc", ]),
                                                      p(id = "txt_j", module_text["phyto_chla", ]),
                                                      p(tags$i("Click through the images to see how we can go from a conceptual food web model to a mathematical representation of the interaction of Nutrient (N) and Phytoplankton (P)."), id = "txt_j")
                                               ),
                                               column(8,
                                                      br(), br(), br(),
                                                      h5("Click on the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("slck_model", width = "600px", height = "450px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12, align = "left",
                                                      box(id = "box6", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(8, offset = 1,
                                                                   h3("Questions"),
                                                                   h5(tags$b(quest[qid[17], 1])),
                                                                   radioButtons(qid[18], quest[qid[18], 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   radioButtons(qid[19], quest[qid[19], 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   radioButtons(qid[20], quest[qid[20], 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   br(),
                                                                   actionButton("submitButtonQ7", "Submit Q.7 answers"),
                                                                   br(),
                                                                   conditionalPanel("input.submitButtonQ7 > 0",
                                                                                    p(tags$i("Thanks for submitting your answers! They've been used to populate a table for a question later on in Activity A.")),
                                                                                    br()),
                                                                   br()
                                                            ),
                                                            column(2,
                                                                   wellPanel(
                                                                     useShinyjs(),  # Set up shinyjs
                                                                     actionButton("ans_btn2", "Check answers"),
                                                                     textOutput("q7a_ans"),
                                                                     textOutput("q7b_ans"),
                                                                     textOutput("q7c_ans")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             
                                             #** Sort state and process variables ====
                                             h2(tags$b("Exercise")),
                                             p(id = "txt_j", "When working with ecological models, the terms 'state variable' and 'parameter' are used. Using the model diagram above, can you identify which are state variables or parameters?"),
                                             p(id = "txt_j", module_text["state_var", 1]),
                                             p(id = "txt_j", module_text["parameter", 1]),
                                             
                                             fluidRow(
                                               column(12, align = "left",
                                                      box(id = "box7", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(8, offset = 1,
                                                                   h5(tags$b(quest[qid[21], 1])),
                                                                   bucket_list(
                                                                     header = "",
                                                                     group_name = "bucket_list_group",
                                                                     orientation = "horizontal",
                                                                     add_rank_list(
                                                                       text = tags$b("Drag from here"),
                                                                       labels = sample(c(state_vars, process_vars)),
                                                                       input_id = "rank_list_1"
                                                                     ),
                                                                     add_rank_list(
                                                                       text = tags$b("State variable"),
                                                                       labels = NULL,
                                                                       input_id = "rank_list_2"
                                                                     ),
                                                                     add_rank_list(
                                                                       text = tags$b("Parameter"),
                                                                       labels = NULL,
                                                                       input_id = "rank_list_3"
                                                                     )
                                                                   ),
                                                                   br(),
                                                                   textAreaInput2(inputId = qid[22], label = quest[qid[22], ], width = "90%"),
                                                                   br(),
                                                                   DTOutput("q9_tab"),
                                                                   br(),
                                                                   br()
                                                            ),
                                                            column(2,
                                                                   wellPanel(
                                                                     useShinyjs(),  # Set up shinyjs
                                                                     actionButton("ans_btn", "Check answers"),
                                                                     textOutput("state_ans"),
                                                                     textOutput("proc_ans")
                                                                   )
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
                                                      p("Now we will use this information about the model to build a model to forecast primary productivity in our chosen site.")
                                               )
                                             )
                                    ),
                                    #* Objective 5 - Prepare inputs ====
                                    tabPanel(title = tab_names["stab5", 2], value = "stab5",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab5", 2]),
                                                                p(id = "txt_j", module_text["obj_05", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h4("Linear Regression"),
                                                      p(id = "txt_j", module_text["linear_regression", ]),
                                                      p("The equation form for a linear regression is: "),
                                                      p(withMathJax("$$y = mx + b $$"), style = "font-size: 20px;"),
                                               ),
                                               column(6, align = "center",
                                                      img(src = "linear_regression_example.png",
                                                          height = "50%",
                                                          width = "50%"),
                                                      p(tags$em("An example plot showing surface water temperature vs. air temperature with a regression line added (orange dashed) with the corresponding equation."))
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Air vs Surface water temperature"),
                                                      wellPanel(
                                                        plotlyOutput("at_wt")
                                                      ),
                                                      p("Because we are using real NEON data, we will need to run",tags$b(" data quality assurance and quality control (QAQC)")," procedures on our data before using the data as inputs to our model. Please click the button below to remove data that are incorrect due to sensor error."),
                                                      actionButton("run_qaqc1", "Run QAQC"),
                                                      conditionalPanel("input.run_qaqc1 > 0",
                                                      p("Now, you can add a linear regression to the QAQCed dataset."),
                                                      actionButton("add_lm2", "Add linear regression"),
                                                      br(),
                                                      wellPanel(
                                                        p(tags$b("Linear regression equation:")),
                                                        uiOutput('lm2_eqn')
                                                      ))
                                               ),
                                               column(6,
                                                      h3("Shortwave radiation vs underwater PAR"),
                                                      wellPanel(
                                                        plotlyOutput("sw_upar")
                                                      ),
                                                      p("Because we are using real NEON data, we will need to run",tags$b(" data quality assurance and quality control (QAQC)")," procedures on our data before using the data as inputs to our model. Please click the button below to remove data that are below the threshold at which the sensor can reliably quantify underwater light."),
                                                      actionButton("run_qaqc2", "Run QAQC"),
                                                      conditionalPanel("input.run_qaqc2 > 0",
                                                                       p("Now, you can add a linear regression to the QAQCed dataset."),
                                                                       actionButton("add_lm3", "Add linear regression"),
                                                                       br(),
                                                                       wellPanel(
                                                                         p(tags$b("Linear regression equation:")),
                                                                         uiOutput('lm3_eqn')
                                                                       ))
                                               ),
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h3("Convert NOAA weather forecast"),
                                                      p("The model we are using to predict chl-a uses data on a daily timestep so we will aggregate the hourly weather forecast to daily averages. Then, we need to convert our air temperature forecast into water temperature and the shortwave forecast into underwater PAR, so we will use the linear model you developed above to convert the 30 members in the ensemble from air temperature (predictor variable) to surface water temperature (response variable) and shortwave radiation (predictor variable) to underwater PAR (response variable)."),
                                                      actionButton("conv_fc", "Convert forecast!", icon = icon("exchange-alt")),
                                                      br(),
                                                      wellPanel(
                                                        plotlyOutput("conv_plot", height = "600px"),
                                                      ),
                                                      hr()
                                               )
                                             ),
                                             hr(),
                                             #*** Next step ----
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we have converted the weather forecast data into inputs that are used by our model (surface water temperature and underwater PAR), we will use them to generate a forecast of primary productivity with the model we built in Objective 5.")
                                               )
                                             )
                                    ),
                                    #* Objective 6 - Forecast! ====
                                    tabPanel(title = tab_names["stab6", 2], value = "stab6",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab6", 2]),
                                                                p(id = "txt_j", module_text["obj_06", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Initial Condition(s) Uncertainty"),
                                                      p(module_text["init_uncert", ]),
                                                      p("Here, we are focused on the initial condition of chlorophyll-a. Even though we have measurements of chlorophyll-a from our lake, we know that chlorophyll-a varies rapidly throughout the day and also over the surface of the lake, so one measurement might not capture perfectly the exact chlorophyll-a in our lake at this time.")
                                               ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("ic_uc_slides", width = "640px", height = "360px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h4("Forecasting with Initial Condition Uncertainty"),
                                                      p("To account for initial condition uncertainty we can generate a distribution around this value and then run our model with slightly different initial conditions to account for this uncertainty."),
                                                      p("Use the input below to set the value of the initial condition for chlorophyll-a."),
                                                      numericInput("ic_val", "Initial condition value:", min = 0.5,
                                                                   value = 7, max = 20, step = 0.5),
                                                      actionButton("gen_ic1", "Generate distribution"),
                                                      hr(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[25], label = quest[qid[25], ], width = "90%"),
                                                                   br()
                                                            )
                                                          )
                                                      )
                                               ),
                                               # column(4,
                                               #        h4("Recent Observations"),
                                               #        wellPanel(
                                               #          plotlyOutput("ic_obs_plot")
                                               #        )
                                               # ),
                                               column(4, offset = 2,
                                                      h4("Distribution of Initial Conditions"),
                                                      wellPanel(
                                                        plotOutput("ic_uc_plot1")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h4("Forecasting with Initial Condition Uncertainty"),
                                                      p("To account for initial condition uncertainty we can generate a distribution around this value and then run our model with slightly different initial conditions to account for this uncertainty."),
                                                      p("Use the slider below to adjust the standard deviation and then generate a normal distribution around the observation"),
                                                      sliderInput("ic_uc", "Standard deviation", min = 0, max = 5, value = 0.5, 
                                                                  step = 0.5),
                                                      actionButton("gen_ic2", "Generate distribution"),
                                                      hr(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[26], label = quest[qid[26], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[27], label = quest[qid[27], ], width = "90%"),
                                                                   br()
                                                            )
                                                          )
                                                      )
                                               ),
                                               # column(4,
                                               #        h4("Recent Observations"),
                                               #        wellPanel(
                                               #          plotlyOutput("ic_obs_plot")
                                               #        )
                                               # ),
                                               column(4, offset = 2,
                                                      h4("Distribution of Initial Conditions"),
                                                      wellPanel(
                                                        plotOutput("ic_uc_plot2")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Forecast!"),
                                                      p("Now we will generate forecasts with different initial conditions for each of our models."),
                                                      br(),
                                                      actionButton("run_fc1", label = div("Run Forecast", icon("running"))),
                                                      br(),
                                                      p("We will use multiple different initial condtions in the forecast ensemble. These will be sampled from the distribution generated above."),
                                                      
                                                      sliderInput("n_mem1", "No. of members", min = 5, max = 100, value = 30, step = 5),
                                                      br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[28], label = quest[qid[28], ], width = "90%"),
                                                                   br()
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        plotlyOutput("chla_fc1"),
                                                        sliderInput("run_fc1_nday", "Number of forecast days", min = 1, max = 35, 
                                                                    value = 1, 
                                                                    step = 1, animate = TRUE),
                                                        radioButtons("plot_type1", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE)
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #*** Next step ----
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will assimilate observed data into your forecast!")
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
                                           h2("Activity B - Assimilate Data"),
                                           p(module_text["act_B", ])
                                 )
                          ),
                        ),
                        tabsetPanel(id = "tabseries3",
                                    # #* Activity B - Overview ====
                                    # tabPanel(title = tab_names["stab7", 2], value = "stab7",
                                    #          fluidRow(
                                    #            column(12,
                                    #                   wellPanel(style = paste0("background: ", obj_bg),
                                    #                             h3("Overview"),
                                    #                             p(id = "txt_j", module_text["act_B_overview", ])
                                    #                   )
                                    #            )
                                    #          )
                                    # ),
                                    #* Objective 7 - Assimilate data ====
                                    tabPanel(title = tab_names["stab7", 2], value = "stab7",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab7", 2]),
                                                                p(id = "txt_j", module_text["obj_07", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Ensemble Kalman filter"),
                                                      p("Data assimilation can be done using a variety of methods. Today, we'll be using an approach called the ",tags$b("ensemble Kalman filter.")),
                                                      p(module_text["enkf_filter2", ], id = "txt_j"),
                                                      p(module_text["enkf_filter3", ], id = "txt_j")
                                               ),
                                               column(7, offset = 1,
                                                      img(src = "EnKF_figure.png", height = "80%", id = "bla_border",
                                                          width = "80%", align = "center"),
                                                      p("Image adapted from: Reichle, R. H., Walker, J. P., Koster, R. D., & Houser, P. R. (2002). ", a("Extended versus Ensemble Kalman Filtering for Land Data Assimilation", href = "https://doi.org/10.1175/1525-7541(2002)003%3C0728:EVEKFF%3E2.0.CO;2", target = "_blank"), ", Journal of Hydrometeorology, 3(6).")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4, offset = 1,
                                                      h4("Today"),
                                                      p("We will use your converted forecasts of water temperature and underwater PAR, as well as observations of chlorophyll-a from your selected lake, to generate forecasts of chlorophyll-a."),
                                                      br(),
                                                      p("We will use an ensemble Kalman filter to conduct data assimilation.")
                                               ),
                                               column(5, offset = 1,
                                                      h4("Types of forecasts:"),
                                                      tags$ol(
                                                        tags$li(id = "txt_j", tags$b("No assimilation:"), module_text["obj7_no_assim", ]),
                                                        tags$li(id = "txt_j", tags$b("Chl-a assimilation:"), module_text["obj7_chla_assim", ])
                                                        #tags$li(id = "txt_j", tags$b("Nitrate assimilation:"), module_text["obj7_nitrate_assim", ]),
                                                        #tags$li(id = "txt_j", tags$b("Chl-a and nitrate assimilation:"), module_text["obj7_chla_nitrate_assim", ])
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(10, offset = 1,
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[29], label = quest[qid[29], ], width = "90%"),
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #** No Data Assimilation ----
                                             fluidRow(
                                               column(4,
                                                      h3("Forecast with no data assimilation"),
                                                      p("Click the button below to generate and plot your first series of forecasts."),
                                                      actionButton("run_fc_no_da", label = div("Run Forecast", icon("running"))),
                                                      sliderInput("n_mem_no_da", "No. of members", min = 5, max = 100, value = 30, step = 5),
                                                      p("You can use the slider underneath the plan to advance the forecasts each day OR you can click the play button on the right side of the slider to animate the forecast over time."),
                                                      p(tags$b("Note:"), " The play button only works when the 'Plot type' is set to 'Line'.")
                                               ),
                                               column(8,
                                                      h3("Forecast with NO DA"),
                                                      wellPanel(
                                                        plotlyOutput("chla_fc_no_da"),
                                                        sliderInput("nday_no_da", "N days", min = 1, max = 35, value = 1, step = 1,
                                                                    animate = TRUE),
                                                        checkboxInput("add_obs_no_da", "Add observations"),
                                                        radioButtons("plot_type_no_da", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE)
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("Assess the forecast"),
                                                      p(module_text["fc_accur", ]),
                                                      tags$ol(
                                                        tags$li(tags$b("Predicted vs. observed plots: "), module_text["pred_v_obs_plot", ]),
                                                        tags$li(tags$b("Root mean square error (RMSE): "), module_text["rmse2", ])
                                                        ),
                                                      div("$$RMSE = \\sqrt{\\sum_{n}^{i=1}\\frac{(P_{i} - O_{i})^2 }n}$$"),
                                                      p(tags$em("where P is equal to the predicted value and O is equal to the observed value and n is equal to the total number of data points.")),
                                                      p("Click the button below to compare your forecast to observational data."),
                                                      actionButton('assess_fc_no_da', label = div("Assess forecast",
                                                                                                  icon("clipboard-check"))),
                                                      br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[30], label = quest[qid[30], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[31], label = quest[qid[31], ], width = "90%"),
                                                                   br()
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(4, offset = 1,
                                                      h3("Predicted vs. Observed"),
                                                      wellPanel(
                                                        plotOutput("chla_fc_assess_no_da")
                                                        ),
                                                      wellPanel(
                                                        textOutput("rmse_no_da")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             #** Chl-a Assimilation ----
                                             fluidRow(
                                               column(4,
                                                      h3("Forecast with chl-a data assimilation"),
                                                      p("Now, we will generate a series of daily forecasts that assimilate chl-a data which is available once per week."),
                                                      p("Click the button below to generate and plot your second forecast"),
                                                      sliderInput("n_mem_chla_assim", "No. of members", min = 5, max = 100, 
                                                                  value = 30, step = 5),
                                                      actionButton("run_fc_chla_assim", label = div("Run Forecast", icon("running"))),
                                                      br(),
                                                      conditionalPanel("input.run_fc_chla_assim > 0",
                                                                       selectInput("view_var_chla_assim", "Select variable", 
                                                                                   choices = view_vars$lname),
                                                                       p("Check the box under the plot to add observations to your forecast. Note that chl-a data points that are assimilated into the forecast are plotted in pink."),
                                                                       p("For some lake sites, there are missing chl-a data! In these cases, you may not be able to assimilate chl-a data every week. We will revisit the importance of missing data in Objective 9 and Activity C.")
                                                      )
                                               ),
                                               column(8,
                                                      h3("Forecast with chl-a DA"),
                                                      wellPanel(
                                                        plotlyOutput("chla_fc_chla_assim"),
                                                        sliderInput("nday_chla_assim", "N days", min = 1, max = 35, value = 1, step = 1,
                                                                    animate = TRUE),
                                                        checkboxInput("add_obs_chla_assim", "Add observations"),
                                                        radioButtons("plot_type_chla_assim", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE)
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4, offset = 1,
                                                      h3("Assess the forecast"),
                                                      p("Click the button below to compare your forecast to observational data."),
                                                      actionButton('assess_fc_chla_assim', label = div("Assess forecast",
                                                                                                       icon("clipboard-check"))),
                                                      br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[32], label = quest[qid[32], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[33], label = quest[qid[33], ], width = "90%"),
                                                                   br()
                                                                   )
                                                            )
                                                          )
                                                      ),
                                               column(4, offset = 2,
                                                      h3("1:1 Plot with chl-a DA"),
                                                      wellPanel(
                                                        plotOutput("chla_fc_assess_chla_assim")
                                                      ),
                                                      h4("RMSE of forecast"),
                                                      wellPanel(
                                                        textOutput("rmse_chla_assim")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             # #** Nitrate Assimilation ----
                                             # fluidRow(
                                             #   column(4,
                                             #          h3("Forecast with nitrate data assimilation"),
                                             #          p("Now, we will generate a forecast that assimilates nitrate data at the weekly timestep."),
                                             #          p("Click the button below to generate and plot your third forecast"),
                                             #          sliderInput("n_mem_nitrate_assim", "No. of members", min = 5, max = 100, 
                                             #                      value = 30, step = 5),
                                             #          actionButton("run_fc_nitrate_assim", label = div("Run Forecast", icon("running"))),
                                             #          br(),
                                             #          conditionalPanel("input.run_fc_nitrate_assim > 0",
                                             #                           selectInput("view_var_nitrate_assim", "Select variable", 
                                             #                                       choices = view_vars$lname),
                                             #                           p("Check the box under the plot to add observations to your forecast. Note that chl-a data points that are assimilated into the forecast are plotted in pink."),
                                             #                           p("For some lake sites, there are missing chl-a data! In these cases, you may not be able to assimilate chl-a data every week. We will revisit the importance of missing data in Objective 9 and Activity C.")
                                             #          )
                                             #   ),
                                             #   column(8,
                                             #          h3("Forecast with nitrate DA"),
                                             #          wellPanel(
                                             #            plotlyOutput("chla_fc_nitrate_assim"),
                                             #            sliderInput("nday_nitrate_assim", "N days", min = 1, max = 35, value = 1, step = 1,
                                             #                        animate = TRUE),
                                             #            checkboxInput("add_obs_nitrate_assim", "Add observations"),
                                             #            radioButtons("plot_type_nitrate_assim", "Plot type", c("Line", "Distribution"),
                                             #                         inline = TRUE)
                                             #          )
                                             #   )
                                             # ),
                                             # fluidRow(
                                             #   column(4, offset = 1,
                                             #          h3("Assess the forecast"),
                                             #          p("Click the button below to compare your forecast to observational data."),
                                             #          actionButton('assess_fc_nitrate_assim', label = div("Assess forecast",
                                             #                                                              icon("clipboard-check"))),
                                             #          br(),
                                             #          box(id = "box2", width = 12, status = "primary",
                                             #              solidHeader = TRUE,
                                             #              fluidRow(
                                             #                column(10, offset = 1,
                                             #                       h3("Questions"),
                                             #                       textAreaInput2(inputId = qid[35], label = quest[qid[35], ], width = "90%"),
                                             #                       textAreaInput2(inputId = qid[36], label = quest[qid[36], ], width = "90%"),
                                             #                       br()
                                             #                       )
                                             #                )
                                             #              )
                                             #          ),
                                             #   column(4, offset = 2,
                                             #          h3("1:1 Plot with nitrate DA"),
                                             #          wellPanel(
                                             #            plotOutput("chla_fc_assess_nitrate_assim")
                                             #          ),
                                             #          h4("RMSE of forecast"),
                                             #          wellPanel(
                                             #            textOutput("rmse_nitrate_assim")
                                             #          )
                                             #   )
                                             # ),
                                             # hr(),
                                             # #** Chl-a & Nitrate Assimilation ----
                                             # fluidRow(
                                             #   column(4,
                                             #          h3("Forecast with chl-a & nitrate data assimilation"),
                                             #          p("Now, we will generate a forecast that assimilates both chlorphyll-a and nitrate data at the weekly timestep."),
                                             #          p("Click the button below to generate and plot your fourth forecast"),
                                             #          sliderInput("n_mem_both_assim", "No. of members", min = 5, max = 100, 
                                             #                      value = 30, step = 5),
                                             #          actionButton("run_fc_both_assim", label = div("Run Forecast", icon("running"))),
                                             #          br(),
                                             #          conditionalPanel("input.run_fc_both_assim > 0",
                                             #                           selectInput("view_var_both_assim", "Select variable", 
                                             #                                       choices = view_vars$lname),
                                             #                           p("Check the box under the plot to add observations to your forecast. Note that chl-a data points that are assimilated into the forecast are plotted in pink."),
                                             #                           p("For some lake sites, there are missing chl-a data! In these cases, you may not be able to assimilate chl-a data every week. We will revisit the importance of missing data in Objective 9 and Activity C.")
                                             #          )
                                             #   ),
                                             #   column(8,
                                             #          h3("Forecast with Chl-a & Nitrate DA"),
                                             #          wellPanel(
                                             #            plotlyOutput("chla_fc_both_assim"),
                                             #            sliderInput("nday_both_assim", "N days", min = 1, max = 35, value = 1, step = 1,
                                             #                        animate = TRUE),
                                             #            checkboxInput("add_obs_both_assim", "Add observations"),
                                             #            radioButtons("plot_type_both_assim", "Plot type", c("Line", "Distribution"),
                                             #                         inline = TRUE)
                                             #          )
                                             #   )
                                             # ),
                                             # fluidRow(
                                             #   column(4, offset = 1,
                                             #          h3("Assess the forecast"),
                                             #          p("Click the button below to compare your forecast to observational data."),
                                             #          actionButton('assess_fc_both_assim', label = div("Assess forecast",
                                             #                                                           icon("clipboard-check"))),
                                             #          br(),
                                             #          box(id = "box2", width = 12, status = "primary",
                                             #              solidHeader = TRUE,
                                             #              fluidRow(
                                             #                column(10, offset = 1,
                                             #                       h3("Questions"),
                                             #                       textAreaInput2(inputId = qid[37], label = quest[qid[37], ], width = "90%"),
                                             #                       textAreaInput2(inputId = qid[38], label = quest[qid[38], ], width = "90%"),
                                             #                       br()
                                             #                       )
                                             #                )
                                             #              )
                                             #          ),
                                             #   column(4, offset = 2,
                                             #          h3("Assess Chl-a & Nitrate data assimilation"),
                                             #          wellPanel(
                                             #            plotOutput("chla_fc_assess_both_assim")
                                             #          ),
                                             #          h4("RMSE of forecast"),
                                             #          wellPanel(
                                             #            textOutput("rmse_both_assim")
                                             #            )
                                             #          )
                                             #   ),
                                             # hr(),
                                             #** Compare all forecasts ----
                                             fluidRow(
                                               column(4,
                                                      h3("Comparison of forecasts"),
                                                      p("We have generated two sets of different forecasts with and without chlorophyll-a data assimilation. Now we want to compare our methods and see which method had the best skill."),
                                                      actionButton("compare_da", "Compare methods"),
                                                      # selectInput("view_var_da_method", "Select variable", 
                                                      #             choices = view_vars$lname),
                                                      h3("Table of RMSE"),
                                                      DTOutput("da_method_tab")
                                               ),
                                               column(8,
                                                      h3("Plot of forecasts with and without DA"),
                                                      wellPanel(
                                                        plotlyOutput("da_method_plot")
                                                      )#,
                                                      # wellPanel(
                                                      #   plotOutput("all_assess_plot")
                                                      # )
                                               )
                                               ),
                                               hr(),
                                               #** Revisit hypothesis ----
                                             fluidRow(
                                               column(10, offset = 1,
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[34], label = quest[qid[34], ], width = "90%"),
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
                                                      p("Now we will explore how observation uncertainty affects data assimilation.")
                                               )
                                             )
                                    ),
                                    #* Objective 8 - Explore observation uncertainty ====
                                    tabPanel(title = tab_names["stab8", 2], value = "stab8",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab8", 2]),
                                                                p(id = "txt_j", module_text["obj_08", ])
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #** Key terms ----
                                             fluidRow(
                                               column(11, offset = 1,
                                                      h3("Key terms")
                                               ),
                                               column(6, offset = 1,
                                                      tags$ul(
                                                        tags$li(tags$b("Observational uncertainty "), module_text["obs_uc", ]),
                                                        tags$li("A ", tags$b("sensor "), module_text["sensor", ])
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #** Chl-a Slides ----
                                             fluidRow(
                                               column(12,
                                                      h1("How do we collect data measurements?"),
                                                      h2(tags$b("Chlorophyll-a"))
                                               ),
                                               column(4,
                                                      h3("Sensor"),
                                                      p(data_collection_methods[7, 3], id = "txt_j"),
                                                      p(data_collection_methods[7, 4], id = "txt_j"),
                                                      h3("Lab Water Sample Measurement"),
                                                      p(data_collection_methods[8, 3], id = "txt_j"),
                                                      p(data_collection_methods[8, 4], id = "txt_j")
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        slickROutput("chla_slides", width = "650px", height = "360px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             # #** Nitrate Slides ----
                                             # fluidRow(
                                             #   column(12,
                                             #          h2(tags$b("Nitrate"))
                                             #   ),
                                             #   column(4,
                                             #          h3("Sensor"),
                                             #          p(data_collection_methods[5, 3], id = "txt_j"),
                                             #          p(data_collection_methods[5, 4], id = "txt_j"),
                                             #          h3("Lab Measurement"),
                                             #          p(data_collection_methods[6, 3], id = "txt_j"),
                                             #          p(data_collection_methods[6, 4], id = "txt_j")
                                             #   ),
                                             #   column(8,
                                             #          wellPanel(
                                             #            slickROutput("nitrate_slides", width = "650px", height = "360px")
                                             #          )
                                             #   )
                                             # ),
                                             # hr(),
                                             #** Questions ----
                                             fluidRow(
                                               column(10, offset = 1,
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[35], label = quest[qid[35], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[36], label = quest[qid[36], ], width = "90%")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #** Explore observation uncertainty ----
                                             fluidRow(
                                               column(12,
                                                      h3("Explore observation uncertainty")
                                               ),
                                               column(4,
                                                      p("Use the slider below to adjust the observation uncertainty and then generate a forecast that assimilates observations with the observation uncertainty you specified."),
                                                      # checkboxGroupInput("obs_uc_da", label = "Data to assimilate:", 
                                                      #                    choices = view_vars$lname[1:2]),
                                                      sliderInput("obs_uc_chla", "Chl-a observation uncertainty",
                                                                  min = 0, max = 5, step = 0.5, value = 0.5),
                                                      # sliderInput("obs_uc_nitrate", "Nitrate observation uncertainty",
                                                      #             min = 0, max = 5, step = 0.5, value = 0.5),
                                                      actionButton("run_fc_obs_uc", label = div("Run Forecast", icon("running"))),
                                                      h4("RMSE of forecast"),
                                                      wellPanel(
                                                        textOutput("rmse_obs_uc")
                                                      ),
                                                      actionButton("save_rmse", "Save RMSE", icon = icon("save")),
                                                      br(),
                                                      DTOutput("obs_uc_rmse"),
                                                      br(),
                                                      p(tags$b("Note:"), " You can also select a row in the table BEFORE clicking 'Save RMSE' to overwrite a row in the table.")
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        plotlyOutput("fc_obs_uc_plot"),
                                                        sliderInput("nday_obs_uc", "N days", min = 1, max = 35, value = 1, step = 1,
                                                                    animate = TRUE),
                                                        checkboxInput("add_obs_obs_uc", "Add observations"),
                                                        radioButtons("plot_type_obs_uc", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE)
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
                                                                   textAreaInput2(inputId = qid[37], label = quest[qid[37], ], width = "90%")
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
                                                      hr(),
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
                                                                   textAreaInput2(inputId = qid[44], label = quest[qid[44], ], width = "90%"),
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
                          ),
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
                                                      p("Sensor A costs $15,000, including the cost of the technology needed to wirelessly stream data from this sensor to computers that will run the forecast model as well as personnel to install and maintain the sensor."),
                                                      p("Sensor B costs $20,000, including the cost of the technology and personnel. Sensor B is more expensive because it is able to make more precise observations (less observation error) and is somewhat more reliable than Sensor A (less likely to experience sensor malfunction)."),
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
                                                                         ),
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
                                                                         ),
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
                                                                         ),
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
