#UI section for "Patient Data Analyser" application
fluidPage(
  #including shinyjs features
  shinyjs::useShinyjs(),
  fluidRow(
    style = "background-color: darkblue;
    color: white;
    margin-bottom: 1rem;
    padding-left: 2rem;
    padding: 0rem 0rem 1rem 1rem;",
           # Application title
           titlePanel("Patient Data Analyser")),
  #define tabset panel to create the tabs
  tabsetPanel(
    id = "tabs",
    #Demographic dataset tab
    tabPanel(title = "Demographic",
             value = "demographic_tab",
             #calling the UI function of data preview module to show the demographic data
             DataPreviewUI("demographic_data_preview"),
             fluidRow(
               style = "margin-top:1rem;",
               column(width = 6,
                      #switch button to show/hide the visuals
                      shinyWidgets::materialSwitch(
                        inputId = "demographic_show_visuals_togg",
                        label = tags$div("Show Visuals",
                                         style = "font-size:16px;font-weight:600;"), 
                        value = FALSE,
                        inline = TRUE,
                        status = "primary",
                      )),
               column(width = 6)),
             #div for entire visual section
             shinyjs::hidden(tags$div(id = "demographic_dta_visuals_div",
                                      fluidRow(
                                        style = "margin-top:1rem;",
                                        column(width = 6,
                                               tags$div(style = "display: flex;align-items: baseline;",
                                                        h3("Data Visualization"),
                                                        #download button for demographic visualization report
                                                        downloadButton(outputId = "demographic_report_download",
                                                                       label =   "Download Report",
                                                                       class = "custom-btn-color",
                                                                       style = "margin-left: 1rem;
                                                                       background-color:#3b5998;color: white;"))),
                                        column(width = 6,
                                               style = "text-align:right;",
                                               #group radio button for grouping variables (SEX,RACE and ETHNIC)
                                               shinyWidgets::radioGroupButtons(
                                                 inputId = "demographic_dim_toggle",
                                                 label = "Grouping Variables",
                                                 choices = c("SEX","RACE","ETHNIC"),
                                                 selected = "SEX",
                                                 status = "primary",
                                                 checkIcon = list(
                                                   yes = icon("ok", 
                                                              lib = "glyphicon"),
                                                   no = icon("remove",
                                                             lib = "glyphicon"))))),
                                      fluidRow(
                                        style = "margin-top:1.5rem;",
                                        column(width = 6,
                                               #plotoutput to generate the patient distribution bar plot for demographic data
                                               shinycssloaders::withSpinner(plotOutput("patient_distribution_bar"),type = 8)),
                                        column(width = 6,
                                               #plotoutput to generate the age distribution box plot for demographic data
                                               shinycssloaders::withSpinner(plotOutput("age_distribution_boxplot"),type = 8))),
                                      fluidRow(
                                        style = "margin-top:2.5rem;margin-bottom: 2.2rem;",
                                        column(width = 6,
                                               #plotoutput to generate the age groups ratio bar plot for demographic data
                                               shinycssloaders::withSpinner(plotOutput("age_groups_ratio_barplot"),type = 8)),
                                        column(width = 6,
                                               #uioutput to show the insights for demographic data
                                               shinycssloaders::withSpinner(uiOutput("demographic_insights_ui"),type = 8)))))
    ),
    #adverse event dataset tab
    tabPanel(
      title = "Adverse Event",
      value = "adverse_event_tab",
      #calling the UI function of data preview module to show the adverse event data
      DataPreviewUI("adverse_event_data_preview"),
      fluidRow(
        style = "margin-top:1rem;",
        column(width = 6,
               #switch button to show/hide the visuals
               shinyWidgets::materialSwitch(
                 inputId = "adv_event_show_visuals_togg",
                 label = tags$div("Show Visuals",
                                  style = "font-size:16px;font-weight:600;"), 
                 value = FALSE,
                 inline = TRUE,
                 status = "primary",
               )),
        column(width = 6)),
      #div for entire visual section
      shinyjs::hidden(tags$div(id = "adv_event_dta_visuals_div",
                               fluidRow(
                                 style = "margin-top:1rem;",
                                 column(width = 6,
                                        tags$div(style = "display: flex;align-items: baseline;",
                                                 h3("Data Visualization"),
                                                 #download button for adverse event visualization report
                                                 downloadButton(outputId = "adv_event_report_download",
                                                                label =   "Download Report",
                                                                class = "custom-btn-color",
                                                                style = "margin-left: 1rem;
                                                                background-color:#3b5998;color: white;")))),
                               fluidRow(
                                 style = "margin-top:2.5rem;",
                                 column(width = 6,
                                        #plotoutput to generate the age distribution density plot for adverse event data
                                        shinycssloaders::withSpinner(plotOutput("age_distribution_density_plot"),type = 8)),
                                 column(width = 6,
                                        #plotoutput to generate the top 5 severe adverse event bar plot for adverse event data
                                        shinycssloaders::withSpinner(plotOutput("top5_severe_adv_evt_barplot"),type = 8))),
                               fluidRow(
                                 style = "margin-top:2.5rem;margin-bottom: 2.2rem;",
                                 column(width = 6,
                                        #plotoutput to generate the severity sex ratio bar plot for adverse event data
                                        shinycssloaders::withSpinner(plotOutput("severity_sex_ratio_barplot"),type = 8)),
                                 column(width = 6,
                                        #uioutput to show the insights for adverse event data
                                        shinycssloaders::withSpinner(uiOutput("adverse_event_insights_ui"),type = 8))))))
  )
)