library(shiny)
library(shinyjs)
library(ggplot2)
library(shinyWidgets)
library(kableExtra)
library(knitr)
library(gt)
library(gtsummary)
library(dplyr)
library(shinycustomloader)

ui <- navbarPage(
  useShinyjs(),
  title = "Safety Database",
  # create summary page for CSV upload and display
  tabPanel(title = "Summary",
           # Sidebar with a slider input for number of bins
           sidebarLayout(
             sidebarPanel(
               id = "sidebar",
               width = 2,
               # select a file to be analyzed
               fileInput(
                 inputId = "file1",
                 label = "Choose a CSV file",
                 multiple = FALSE,
                 accept = c("text/csv", ".csv")
               ),
               # check if file has header
               checkboxInput(inputId = "header",
                             label = "Contains Header",
                             TRUE),
               radioButtons(
                 inputId = "sep",
                 label = "Type of Separator",
                 choices = c(
                   Comma = ",",
                   Semicolon = ";",
                   Tab = "\t",
                   None = ""
                 ),
                 selected = ","
               ),
               actionButton("choice", "Update"),
               
               #selectInput(inputId = "columns",
               #label = "Columns to show",
               #choices = NULL, multiple = TRUE),
               
               selectInput(
                 inputId = "rows",
                 label = "Rows to show",
                 choices = NULL,
                 multiple = TRUE
               )
               
               
             ),
             mainPanel(
               width = 10,
               tabsetPanel(
                 id = "tabs",
                 tabPanel(
                   "original_data",
                   fluidRow(
                     column(
                       3,
                       pickerInput(
                         inputId = "site",
                         label = "Site",
                         choices = NULL,
                         multiple = TRUE,
                         options = list("actions-box" = TRUE)
                       )
                     ),
                     column(
                       3,
                       pickerInput(
                         inputId = "subject",
                         label = "Subject",
                         choices = NULL,
                         multiple = TRUE,
                         options = list("actions-box" = TRUE)
                       )
                     ),
                     column(
                       3,
                       pickerInput(
                         inputId = "organ_class",
                         label = "System Organ Class",
                         choices = NULL,
                         multiple = TRUE,
                         options = list("actions-box" = TRUE)
                       )
                     ),
                     column(
                       3,
                       pickerInput(
                         inputId = "preferred_term",
                         label = "Preferred Term",
                         choices = NULL,
                         multiple = TRUE,
                         options = list("actions-box" = TRUE)
                       )
                     )
                     # column(
                     #   3,
                     #   pickerInput(
                     #     inputId = "organ_class",
                     #     label = "System Organ Class",
                     #     choices = NULL,
                     #     multiple = TRUE,
                     #     options = list("actions-box" = TRUE)
                     #   )
                     # )
                   ),
                   DT::dataTableOutput("contents")
                 ),
                 tabPanel("Summary Table", tableOutput("summary_table")),
                 # tabPanel("Crude Incidence Rate and Risk Ratio Estimation",
                 #          fluidRow(
                 #                         column(4,
                 #                                pickerInput(
                 #                                  inputId = "treatment1",
                 #                                  label = "Control Group",
                 #                                  choices = NULL,
                 #                                  multiple = FALSE
                 #                                )),
                 #                         column(4,
                 #                                pickerInput(
                 #                                  inputId = "treatment2",
                 #                                  label = "Treatment Group",
                 #                                  choices = "",
                 #                                  multiple = FALSE
                 #                                ))
                 #
                 #                         ),
                 #          fluidRow(column(5, plotOutput("crude_incidence", height = "790px")),
                 #                   column(7, plotOutput("crude_wald", height = "750px"))),
                 #          fluidRow(plotOutput("volcano"))
                 #          #column(2, tableOutput("crude_wald_table"))
                 #
                 # ),
                 
                 
                 #   tabPanel("Exposure Adjusted Incidence Rate",
                 #            fluidRow(
                 #              column(6,
                 #                     pickerInput(
                 #                       inputId = "treatment1",
                 #                       label = "Treatment Group 1",
                 #                       choices = NULL,
                 #                       multiple = FALSE
                 #                     )),
                 #              column(6,
                 #                     pickerInput(
                 #                       inputId = "interest_ae",
                 #                       label = "Adverse Event of Interest",
                 #                       choices = "",
                 #                       multiple = FALSE
                 #                     ))
                 #              ),
                 #            fluidRow(tableOutput("eair_table"), textOutput("txt"))
                 # ),
                 
                 # tabPanel("Exposure-Adjusted Incidence Rate and Wald's CI",
                 #           fluidRow(
                 # column(2,
                 #        pickerInput(
                 #          inputId = "interest_ae",
                 #                       label = "Adverse Event of Interest",
                 #                       choices = "",
                 #                       multiple = FALSE)),
                 #   column(4,
                 #          pickerInput(
                 #            inputId = "treatment_1",
                 #            label = "Control Group",
                 #            choices = NULL,
                 #            multiple = FALSE
                 #          )),
                 #   column(4, pickerInput(
                 #     inputId = "treatment_2",
                 #     label = "Treatment Group",
                 #     choices = NULL,
                 #     multiple = FALSE
                 #   )),
                 #   column(4, numericInput(inputId = "effect_days", label = "Duration of Drug Action (days)", value = 0, min = 0, max = 1000))
                 # ),
                 # fluidRow(column(5, plotOutput("eair_incidence", height = "790px")),
                 #          column(7, plotOutput("eair_wald", height = "790px")),
                 #          textOutput("txt")),
                 # fluidRow("***Most appropriate when the hazard rate of the specific event is relatively constant over the duration of the study.
                 #          Not approriate for events that usually occur early in the study."),
                 
                 # fluidRow(
                 #   column(2, "Duration of Treatment (years) Median (outside of dummy data)"),
                 #   column(5,
                 #          textInput(
                 #            inputId = "trt1_year",
                 #            label = NULL,
                 #            value = 0.0)),
                 #   column(5,
                 #          textInput(
                 #            inputId = "trt2_year",
                 #            label = NULL,
                 #            value = 0.0))
                 # )
                 #),
                 # tabPanel("Fisher Exact vs Relative Risk",
                 #
                 #          fluidRow(plotOutput("volcano"))),
                 tabPanel(
                   "SOC vs. Treatment",
                   fluidRow(
                     #column(7, tableOutput("soc_tr")),
                   column(6, plotOutput("heatmap", height = "700px")),
                   column(6, plotOutput("soc_bar", height = "700px"))),
                   fluidRow(
                     column(12, DT::dataTableOutput("soc_tr")))
                     #column(6, plotOutput("pie_soc")))
                   
                   
                   
                 )
               )
               
             )
           )),
  # Second navbar page
  tabPanel(
    title = "Statistical Visualization",
    fluidRow(column(
      3,
      pickerInput(
        inputId = "treatment1",
        label = "Control Group",
        choices = NULL,
        multiple = FALSE
      )
    ),
    column(
      3,
      pickerInput(
        inputId = "treatment2",
        label = "Treatment Group",
        choices = "",
        multiple = FALSE
      )
    ),
    column(
      2,
      numericInput(
        inputId = "effect_days",
        label = "Duration of Drug Action (days)",
        value = 0,
        min = 0,
        max = 1000
      )
    ),
    column(
      2,
      actionButton("confirm", "Update")
    ),
    column(
      2, 
      actionButton("report", "Generate Report")
    )),
    tabsetPanel(
      tabPanel(
        "Crude Incidence Rate and Risk Ratio Estimation",
        fluidRow(column(
          5, plotOutput("crude_incidence", height = "760px")
        ),
        column(
          7, plotOutput("crude_wald", height = "720px")
        ))
      ),
      tabPanel("Fisher Exact vs. Relative Risk",
               fluidRow(plotOutput("volcano", height = "720px"))),
      tabPanel(
        "Exposure-Adjusted Incidence Rate and Risk Ratio Estimation",
        # fluidRow(column(
        #   4,
        #   numericInput(
        #     inputId = "effect_days",
        #     label = "Duration of Drug Action (days)",
        #     value = 0,
        #     min = 0,
        #     max = 1000
        #   )
        # )),
        fluidRow(
          column(5, plotOutput("eair_incidence", height = "760px")),
          column(7, plotOutput("eair_wald", height = "720px")),
          textOutput("txt")
        ),
        fluidRow(
          "***Most appropriate when the hazard rate of the specific event is relatively constant over the duration of the study.
                                       Not approriate for events that usually occur early in the study."
        )
      )
    )
  ),
  
  tabPanel(title = "Univariate Bayesian Meta-Analysis of OR",
           fluidRow(
             column(3, fileInput(
               inputId = "file2",
               label = "Upload",
               multiple = FALSE,
               accept = c("text/csv", ".csv")
             )),
             
             column(2, numericInput(inputId = "burnin", label = "Burnin", value = 50000)),
             column(2, numericInput(inputId = "iteration", label = "Number of Iteration", value = 200000)),
             column(2, numericInput(inputId = "chains", label = "Number of Chains", value = 3)),
             column(1, numericInput(inputId = "alpha",
                                    label = "Alpha",
                                    value = 0.001)),
             column(1, numericInput(inputId = "beta",
                                    label = "Beta",
                                    value = 0.001))
             
           ),
           tabsetPanel(
             tabPanel("Model Consideration", 
                      withMathJax(),
                      helpText("The theoretical formula behind this Bayesian model:"),
                      uiOutput("equation"),
                      hr(),
                      fluidRow(grVizOutput("flowchart"))),
             tabPanel("Check model convergence",
                      fluidRow(
             column(2, pickerInput(inputId = "prior_type",
                                   label = "Prior Distribution",
                                   choices = c("Inverse-Gamma", 
                                              "Uniform", 
                                              "Half-Normal",
                                              "Log-Normal"),
                                   multiple = FALSE)),
             column(4, pickerInput(inputId = "study_type",
                                          label = "Type of Comparison",
                                          choices = c("Pharmacological vs. placebo/control comparison",
                                                      "Pharmacological vs. pharmacological comparison",
                                                      "Non-pharmacological comparison"),
                                          multiple = FALSE)),
             column(2, pickerInput(inputId = "outcomes", 
                                   label = "Outcomes",
                                   choices = c("all-cause mortality",
                                               "semi-objective outcome",
                                               "subjective outcome"))),
             column(2),
             column(2, actionButton("refresh", "Update"))
             
             
           ),
           
           fluidRow(withLoader(
             DT::dataTableOutput("meta_table"), type="image", loader="loading-cat.gif"
           )),
           hr(),
           fluidRow(
             column(6,
             withLoader(
             plotOutput("meta_trace"), type="text", loader=list(marquee("Traceplot on the way"))
           )),
           column(6, 
                  withLoader(plotOutput("meta_den"), type = "text", loader = list(marquee("Density plot coming soon")))
                  )
           )
           )))
)

