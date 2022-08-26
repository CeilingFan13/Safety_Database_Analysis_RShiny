library(shiny)
library(shinyjs)
library(ggplot2)
library(shinyWidgets)
library(kableExtra)
library(knitr)
library(gt)
library(gtsummary)
library(dplyr)
library(DiagrammeR)
library(shinycustomloader)
library(bayesmeta)

ui <- navbarPage(
  useShinyjs(),
  title = "Safety Database",
  # create summary page for CSV upload and display-------------------------------
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
                   ),
                   DT::dataTableOutput("contents")
                 ),
                 tabPanel("Summary Table", tableOutput("summary_table")),
                 tabPanel(
                   "Basic Statistical Display",
                   fluidRow(
                     #column(7, tableOutput("soc_tr")),
                   column(6, plotOutput("heatmap", height = "700px")),
                   column(6, plotOutput("soc_bar", height = "700px"))),
                   fluidRow(
                     column(12, DT::dataTableOutput("soc_tr")))
                     #column(6, plotOutput("pie_soc"))) 
                 ),
                 tabPanel("Statistical Visualization",
                          fluidRow(column(
                            2,
                            pickerInput(
                              inputId = "treatment1",
                              label = "Control Group",
                              choices = NULL,
                              multiple = FALSE
                            )
                          ),
                          column(
                            2,
                            pickerInput(
                              inputId = "treatment2",
                              label = "Treatment Group",
                              choices = "",
                              multiple = FALSE
                            )
                          ),
                          column(
                            4,
                            numericInput(
                              inputId = "effect_days",
                              label = "Drug Action Duration (days)",
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
                          )
               )
               
             )
           )),
  # Second navbar page ---------------------------------------------------------
  # tabPanel(
    # title = "Statistical Visualization",
    # fluidRow(column(
    #   3,
    #   pickerInput(
    #     inputId = "treatment1",
    #     label = "Control Group",
    #     choices = NULL,
    #     multiple = FALSE
    #   )
    # ),
    # column(
    #   3,
    #   pickerInput(
    #     inputId = "treatment2",
    #     label = "Treatment Group",
    #     choices = "",
    #     multiple = FALSE
    #   )
    # ),
    # column(
    #   2,
    #   numericInput(
    #     inputId = "effect_days",
    #     label = "Duration of Drug Action (days)",
    #     value = 0,
    #     min = 0,
    #     max = 1000
    #   )
    # ),
    # column(
    #   2,
    #   actionButton("confirm", "Update")
    # ),
    # column(
    #   2, 
    #   actionButton("report", "Generate Report")
    # )),
    # tabsetPanel(
    #   tabPanel(
    #     "Crude Incidence Rate and Risk Ratio Estimation",
    #     fluidRow(column(
    #       5, plotOutput("crude_incidence", height = "760px")
    #     ),
    #     column(
    #       7, plotOutput("crude_wald", height = "720px")
    #     ))
    #   ),
    #   tabPanel("Fisher Exact vs. Relative Risk",
    #            fluidRow(plotOutput("volcano", height = "720px"))),
    #   tabPanel(
    #     "Exposure-Adjusted Incidence Rate and Risk Ratio Estimation",
    #     fluidRow(
    #       column(5, plotOutput("eair_incidence", height = "760px")),
    #       column(7, plotOutput("eair_wald", height = "720px")),
    #       textOutput("txt")
    #     ),
    #     fluidRow(
    #       "***Most appropriate when the hazard rate of the specific event is relatively constant over the duration of the study.
    #                                    Not approriate for events that usually occur early in the study."
    #     )
    #   )
    # )
  # ),
 #------------------------------------------------------------------------------
 tabPanel(
   title = "Hierarchical Beta-Binomial Modeling",
   tabsetPanel(
     tabPanel("Model Consideration",
              "The objective is to learn about the adverse event probability of the group of interest (country, sites, treatment groups). This modeling setup provides posterior estimates that partially pool information among groups.",
              withMathJax(),
              helpText("The theoretical formula behind this Bayesian model:"),
              uiOutput("equation0")),
     tabPanel("Evaluation and Result",
     fluidRow(
       column(3, pickerInput(inputId = "group", 
                             label = "Group to Compare by:", 
                             choices = c("Country", "Site..", "Treatment", "SOC"))),
       column(3,pickerInput(inputId = "event", 
                            label = "Event", 
                            choices = NULL, 
                            multiple = TRUE,
                            options = list("actions-box" = TRUE))),
       column(3, actionButton("yep", "Update"))
       
     ),
     fluidRow("Compare the posterior densities of the different event rate:"),
     fluidRow(plotOutput("p")),
     plotOutput("mu"),
     plotOutput("logeta"),
     fluidRow(column(6, "Shrinkage plot showing how the sample proportions are shrunk towards the overall event rate:"),
              column(6, "Draw 5000 MCMC iterations, and identifies the group with the highest number of smallest simulated value:")),
     fluidRow(
       column(6, plotOutput("beta_binom")),
       column(6, plotOutput("compare"))
       
       )
   )
   )
 ),
 #------------------------------------------------------------------------------ 
 tabPanel(title = "Logistic Regression Model with Mixture Prior on Log-OR",
          fluidRow(
            column(3, fileInput(
              inputId = "file3",
              label = "Upload",
              multiple = FALSE,
              accept = c("text/csv", ".csv")
            )),
            column(2, numericInput(
              inputId = "Nc",
              label = "# Subjects in Control",
              value = 50,
              min = 0,
              max = 1000
            )),
            column(2, numericInput(
              inputId = "Nt",
              label = "# Subjects in Treatment",
              value = 60,
              min = 0,
              max = 1000
            )),
            column(2, actionButton("refresh3", "Update")),
            column(2, actionButton("report2", "Generate Report"))
            
          ),
          tabsetPanel(
            tabPanel("Model Consideration",
                     helpText("The theoretical formula behind this Bayesian model:"),
                     uiOutput("equation2"),
                     helpText("**Point mass: 0-dimensional point that may be assigned a finite mass. It is often a useful simplification in real problems to consider bodies point masses, especially when the dimensions of the bodies are much less than the distances among them.")),
            tabPanel("Result Table",
                     h4("Top 10 AEs with highest risk difference", align = "center"),
                     fluidRow(DT::dataTableOutput("Xia_table1")),
                     h4("Top 10 AEs with highest odds ratio", align = "center"),
                     fluidRow(DT::dataTableOutput("Xia_table2"))
                     ),
            tabPanel("Result Plot",
                     column(6, plotOutput("Xia_plot1", height = "700px")),
                     column(6, plotOutput("Xia_plot2", height = "700px")))
          )), 
 tabPanel(title = "Univariate Bayesian Meta-Analysis of OR",
           fluidRow(
             column(3, fileInput(
               inputId = "file2",
               label = "Upload",
               multiple = FALSE,
               accept = c("text/csv", ".csv")
             )),
             column(2, numericInput(inputId = "alpha",
                                    label = "Alpha",
                                    value = 0.001)),
             column(2, numericInput(inputId = "beta",
                                    label = "Beta",
                                    value = 0.001)),
             column(1),
             column(2, actionButton("refresh", "Update")),
             column(2, actionButton("report1", "Generate Report"))
             
           ),
           tabsetPanel(
             tabPanel("Model Consideration", 
                      withMathJax(),
                      helpText("The theoretical formula behind this Bayesian model:"),
                      uiOutput("equation"),
                      hr(),
                      fluidRow(grVizOutput("flowchart"))),
             tabPanel("Result and Check Model Convergence",
                      fluidRow(column(2, numericInput(inputId = "burnin", label = "Burnin", value = 50000)),
                               column(2, numericInput(inputId = "iteration", label = "Number of Iteration", value = 200000)),
                               column(2, numericInput(inputId = "chains", label = "Number of Chains", value = 3)),
                               # column(2, numericInput(inputId = "alpha",
                               #                        label = "Alpha",
                               #                        value = 0.001)),
                               # column(2, numericInput(inputId = "beta",
                               #                        label = "Beta",
                               #                        value = 0.001))
                               
                               ),
                      fluidRow(
             column(4, pickerInput(inputId = "prior_type",
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
             column(4, pickerInput(inputId = "outcomes", 
                                   label = "Outcomes",
                                   choices = c("all-cause mortality",
                                               "semi-objective outcome",
                                               "subjective outcome")))
             
             
             
           ),
           
           fluidRow(
             column(6,withLoader(
             DT::dataTableOutput("meta_table"), type="image", loader="loading-cat.gif")
           ),
           column(6, plotOutput("logor"))
           ),
           hr(),
           fluidRow(
             column(6,
             withLoader(
             plotOutput("meta_trace"), type="text", loader=list(marquee("Traceplot on the way"))
           )),
           column(6, 
                  withLoader(plotOutput("meta_den"), type = "text", loader = list(marquee("Density plot coming soon")))
                  )
           ),
           #tableOutput("mu_plot")
           ),
           # tabPanel("Compare to Frequentist Method",
           #          plotOutput("freq_meta")
           # ),
           
           tabPanel("Random-effects Meta-regression Model", 
                    fluidRow(column(3, pickerInput(inputId = "tau_prior", 
                                                   label = "Type of Tau Prior", 
                                                   choices = c("uniform", 
                                                               "sqrt", 
                                                               "Jeffreys", 
                                                               "BergerDeely", 
                                                               "conventional", 
                                                               "DuMouchel", 
                                                               "shrinkage", 
                                                               "I2")))),
                    plotOutput("bmr_bayesian")),
           tabPanel("Pairwise Bayesian Model",
                    fluidRow(plotOutput("bmr_pairwise")))
           )),
 # tabPanel(title = "Logistic Regression Model with Mixture Prior on Log-OR",
 #          tabsetPanel(
 #            tabPanel("Model Consideration",
 #                     helpText("The theoretical formula behind this Bayesian model:"),
 #                     uiOutput("equation2"),
 #                     helpText("**Point mass: 0-dimensional point that may be assigned a finite mass. It is often a useful simplification in real problems to consider bodies point masses, especially when the dimensions of the bodies are much less than the distances among them.")),
 #            tabPanel("Result")
 #          ))
)

