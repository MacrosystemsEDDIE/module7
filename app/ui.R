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
# suppressPackageStartupMessages(library(ggforce, quietly = TRUE)) # Only for geom_ellipse (doesn't work in plotly!)

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

               # 1. Module Overview ----
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
                          h3("Summary"),
                          p(id = "txt_j", module_text["eco_forecast", ]),
                          p(id = "txt_j", module_text["this_module", ]),
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
                                 p("The presentation accompanying this module covers the introduction to data assimilation, sources of forecast uncertainty and the importance and relevance of quantifying uncertainty within ecological forecasts."),
                                 p("What is data assimilation?"),
                                 tags$ul(
                                   tags$li(module_text["data_assimilation", ])
                                 ),
                                 p("How does the amount of uncertainty in model predictions and data affect the process of data assimilation?"),
                                 tags$ul(
                                   tags$li(module_text["da_uncert", ])
                                 ),
                                 p("How does the frequency of observations affect data assimilation"),
                                 tags$ul(
                                   tags$li(module_text["da_freq", ])
                                 ),
                                 p("Click through the slides to recap some of the main points from the lecture.")
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Key Slides",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("slides", width = "600px", height = "450px")
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
                          column(5,
                                 h3("Workflow for this module"),
                                 tags$ol(
                                   tags$li(id = "txt_j", module_text["workflow1", ]),
                                   tags$li(id = "txt_j", module_text["workflow2", ]),
                                   tags$li(id = "txt_j", module_text["workflow3", ]),
                                   tags$li(id = "txt_j", module_text["workflow4", ])
                                   # tags$li(id = "txt_j", module_text["workflow5", ]),
                                   # tags$li(id = "txt_j", module_text["workflow6", ])
                                 )
                          ),
                          column(6, align = "center", offset = 1,
                                 br(), br(),
                                 img(src = "activity_outline.png", height = "80%", id = "bla_border",
                                     width = "80%", tags$style("border: solid 2px black;"))

                          )
                        ), hr(),
                        fluidRow(
                          column(7, offset = 1,
                                 h3("Student Activities"),
                                 p(module_text["student_activities", ]),
                                 box(width = 12, status = "warning",
                                     solidHeader = TRUE,
                                     p(tags$b("WARNING:"), " The Shiny app will disconnect from the server if it is left idle for 15 minutes. If this happens you will lose all your inputs into the app. It is recommended to download the user input at the end of the class, but you can also download throughout the class."),
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
                                 h3("Save your progress"),
                                 p(id = "txt_j", module_text["save_progress", ]),
                                 br(),
                                 h3("Resume your progress"),
                                 p(id = "txt_j", "To reload the app input you can upload the downloaded '.eddie' file below and it will populate your answers into the Shiny app."),
                                 fileInput("upload_answers", "Upload data", accept = c(".eddie", ".rds")), # B77C2C
                                 p(id = "txt_j", HTML(paste0(tags$b("Note:"), " You will need to navigate to tabs Objective 1 in Activity A after uploading your file for the site selection to load there."))),
                                 p(id = "txt_j", "Currently the plots do not save to the file.  If you generated plots during your last session, you will need to reload the data and reproduce the plots before generating your report.")
                          ),
                          column(4, offset = 1,
                                 introBox(
                                   h3("Generate Report"),
                                   p(module_text["generate_report", ]),
                                   actionButton("generate", "Generate Report (.docx)", icon = icon("file"), width = "190px", class = "btn-primary"
                                                # id = "dl_btn", # This is the only button that shows up when the app is loaded
                                                # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
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
                                     ),

                                 ),
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
                                 p("Complete objectives 1-2 to familiarize yourself with the data from your selected site and learn about the data you will be using.")
                          )
                        ),

                        #* Objective 1 - Select and view site ====
                        tabsetPanel(id = "tabseries1",
                                    tabPanel(title = tab_names["stab1", 2],

                                             value = "stab1", id = "wh_link",
                                             tags$style("outline: 5px dotted green;"),
                                             #* Objective 1 ====
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
                                                                   h4(quest[qid[3], 1]),
                                                                   p("If the information for your lake is not on the NEON website then you can input NA (Not Available) into the text box.")
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
                                                                   textAreaInput2(inputId = qid[10], label = quest[qid[10], ], width = "90%"),
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
                                                      p("We will build models that will allow us to predict water temperature."))
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
                                                      p("For Q. 7 you will keep 'Chlorophyll-a' as the y-variable and explore its relationship with the other variables at this site."),
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
                                                      p("While for Q. 8, you can select any two variables and investigate if there is any relationship. e.g. air temperature and surface water temperature")
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
                                                                   h4(quest["q7", 1]),
                                                                   p("TABLE FOR ANSWERS"),
                                                                   # DTOutput('q7_tab'),
                                                                   br(),
                                                                   h4(quest[qid[12], ]),
                                                                   textAreaInput2(inputId = qid[12], label = "",
                                                                                  width = "90%"),
                                                                   br()
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             fluidRow(
                                               hr(),
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
                                                                p(id = "txt_j", module_text["obj_03", ])
                                                                )
                                                      )
                                               ),
                                             fluidRow(
                                               column(12, align = "center",
                                                      img(src = "02-build-model.png", height = "30%",
                                                          width = "30%")
                                               )
                                             ), br(), br(), hr(),
                                             #* Intro text ====
                                             fluidRow(
                                               # conditionalPanel(condition = "input.site_html > 1",
                                               #** NEON Intro ----
                                               column(4,
                                                      h3("What is a Model?"),
                                                      h4("Read through this section and scroll through the slides"),
                                                      p(id = "txt_j", module_text["model1", ]),
                                                      p(id = "txt_j", module_text["model2", ]),
                                                      p(id = "txt_j", module_text["model3", ]),
                                                      p(id = "txt_j", module_text["mod_desc", ]),
                                                      p(id = "txt_j", module_text["phyto_chla", ]),
                                                      p("Click through the images to see how we can go from a conceptual food web model to a mathematical representation of the interaction of Nitrogen (N) and Phytoplankton (P).", id = "txt_j")
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
                                               column(10, align = "left",
                                                      box(id = "box6", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   h4(quest[qid[12], 1]),
                                                                   radioButtons(qid[13], quest[qid[13], 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   radioButtons(qid[14], quest[qid[14], 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   radioButtons(qid[15], quest[qid[15], 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   radioButtons(qid[16], quest[qid[16], 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   br()
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
                                                                   h4(quest[qid[16], 1]),
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
                                                                   h4(quest[qid[17], 1]),
                                                                   radioButtons(qid[18], quest[qid[18], 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   radioButtons(qid[19], quest[qid[19], 1], choices = mod_choices, inline = TRUE, selected = character(0)),
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
                                                      p("You can add a linear regression to the whole data or a subset by selecting data points using the 'Box Select' or 'Lasso Select' tool. This may be required if you have many points around 0 or you want to exclude obvious outliers."),
                                                      actionButton("add_lm2", "Add linear regression"),
                                                      p("Clear selected points and regression line"),
                                                      actionButton("clear_sel2", "Clear plot"),
                                                      br(),
                                                      wellPanel(
                                                        p(tags$b("Linear regression equation:")),
                                                        uiOutput('lm2_eqn')
                                                      )
                                               ),
                                               column(6,
                                                      h3("Shortwave radiation vs underwater PAR"),
                                                      wellPanel(
                                                        plotlyOutput("sw_upar")
                                                      ),
                                                      p("You can add a linear regression to the whole data or a subset by selecting data points using the 'Box Select' or 'Lasso Select' tool. This may be required if you have many points around 0 or you want to exclude obvious outliers."),
                                                      actionButton("add_lm3", "Add linear regression"),
                                                      p("Clear selected points and regression line"),
                                                      actionButton("clear_sel3", "Clear plot"),
                                                      br(),
                                                      wellPanel(
                                                        p(tags$b("Linear regression equation:")),
                                                        uiOutput('lm3_eqn')
                                                      )
                                               ),
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h3("Convert NOAA weather forecast"),
                                                      p("The model we are using uses data on a daily timestep so we will aggregate the hourly weather forecast to daily averages first and then use the linear model to convert the 30 members in the ensemble from air temperature (predictor variable) to surface water temperature (response variable) and shortwave radiation (predictor variable) to underwater PAR (response variable)."),
                                                      actionButton("conv_fc", "Convert forecast!", icon = icon("exchange-alt")),
                                                      br(),
                                                      wellPanel(
                                                        plotlyOutput("conv_plot", height = "600px"),
                                                      ),
                                                      hr()
                                               )
                                             ),
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
                                                      h3("Initial Condition Uncertainty"),
                                                      p(module_text["init_uncert", ]),
                                                      p("Even though we have measurements of water temperature from our lake, we know that water temperature varies throughout the day so this measurement might not capture exactly the temperature in our lake at this time.")
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
                                                      h4("Forecasting with Initial Conditions Uncertainty"),
                                                      p("To account for initial condition uncertainty we can generate a distribution around this value and then run our model with slightly different initial conditions to account for this uncertainty."),
                                                      p("Use the slider below to adjust the standard deviation and then generate a normal distribution around the observation"),
                                                      sliderInput("ic_uc", "Standard deviation", min = 0.05, max = 0.5, value = 0.1, step = 0.05),
                                                      actionButton("gen_ic", "Generate distribution")
                                               ),
                                               column(4,
                                                      h4("Recent Observations"),
                                                      wellPanel(
                                                        plotlyOutput("ic_obs_plot")
                                                      )
                                               ),
                                               column(4,
                                                      h4("Distribution of Initial Conditions"),
                                                      wellPanel(
                                                        plotOutput("ic_uc_plot")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Forecast!"),
                                                      p("Now we will generate forecasts with different initial conditions for each of our models."),
                                                      br(),
                                                      actionButton("run_fc1", "Run forecast"),
                                                      br(),
                                                      p("We will use 100 different initial condtions in the forecast ensemble. These will be sampled from the distribution generated above."),
                                                      
                                                      sliderInput("n_mem1", "No. of members", min = 5, max = 100, value = 30, step = 5),
                                                      br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   # textAreaInput2(inputId = qid[26], label = quest[qid[26], ], width = "90%"),
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        plotlyOutput("chla_fc1"),
                                                        radioButtons("plot_type1", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE)
                                                      ),
                                                      wellPanel(
                                                        plotlyOutput("nitrate_fc1")
                                                      ),
                                                      wellPanel(
                                                        plotlyOutput("maxUptake_fc1")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("TBD...")
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
                                           h2("Activity B - XXXXX"),
                                           p(module_text["act_B", ])
                                 )
                          ),
                        ),
                        tabsetPanel(id = "tabseries3",
                                    #* Activity B - Overview ====
                                    tabPanel(title = tab_names["stab7", 2], value = "stab7",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Overview"),
                                                                p(id = "txt_j", module_text["act_B_overview", ])
                                                                )
                                                      )
                                               )
                                             ),
                                    #* Objective 7 - Assimilate data ====
                                    tabPanel(title = tab_names["stab8", 2], value = "stab7",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab8", 2]),
                                                                p(id = "txt_j", module_text["obj_07", ])
                                                                )
                                                      )
                                               )
                                             ),
                                    #* Objective 8 - Explore observation uncertainty ====
                                    tabPanel(title = tab_names["stab9", 2], value = "stab9",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab9", 2]),
                                                                p(id = "txt_j", module_text["obj_08", ])
                                                                )
                                                      )
                                               )
                                             ),
                                    #* Objective 9 - Driver Uncertainty ====
                                    tabPanel(title = tab_names["stab10", 2], value = "stab10",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab9", 2]),
                                                                p(id = "txt_j", module_text["obj_09", ])
                                                                )
                                                      )
                                               )
                                             ) ,
                                    # * Activity B - Summary ====
                                    tabPanel(title = tab_names["stab11", 2], value = "stab11",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab11", 2]),
                                                                p(id = "txt_j", module_text["act_B_summ", ]),
                                                                p("Remember, the Shiny app will disconnect if you leave it idle for 10 minutes, so make sure to download your '.eddie' file at the bottom of the page to checkpoint your progress.")
                                                                )
                                                      )
                                               )
                                             )
                                    )
                        ),
               # 6. Activity C ----
               tabPanel(title = tab_names["mtab7", 2], value = "mtab7",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity C - Managing Uncertainty"),
                                           p(module_text["act_C", ])
                                           )
                                 ),
                          ),
                        tabsetPanel(id = "tabseries4",
                                    #* Objective 10 - Management Scenario ====
                                    tabPanel(title = tab_names["stab12", 2], value = "stab12",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3(tab_names["stab11", 2]),
                                                                p(id = "txt_j", module_text["obj_10", ])
                                                                )
                                                      )
                                               )
                                             ),
                                    #* Activity C - Summary ====
                                    tabPanel(title = tab_names["stab13", 2], value = "stab13",
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
                                         label = "Download user input",
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
