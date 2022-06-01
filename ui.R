library(shiny)
library(shinyjs)
library(ggplot2)
library(shinyWidgets)
library(kableExtra)
library(knitr)
library(gt)
library(gtsummary)
library(dplyr)

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
                         inputId = "preferred_term",
                         label = "Preferred Term",
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
                     )
                   ),
                   DT::dataTableOutput("contents")
                 ),
                 tabPanel("Summary Table", tableOutput("summary_table")),
                 # tabPanel("Crude Incidence Rate and Wald's CI",
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
                   fluidRow(column(6, plotOutput("heatmap")),
                            column(6, plotOutput("pie_soc"))),
                   hr(),
                   fluidRow(tableOutput("soc_tr"))
                   
                 )
               )
               
             )
           )),
  # Second page
  tabPanel(
    title = "Statistical Visualization",
    fluidRow(column(
      5,
      pickerInput(
        inputId = "treatment1",
        label = "Control Group",
        choices = NULL,
        multiple = FALSE
      )
    ),
    column(
      5,
      pickerInput(
        inputId = "treatment2",
        label = "Treatment Group",
        choices = "",
        multiple = FALSE
      )
    ),
    column(
      2,
      actionButton("confirm", "Update")
    )),
    tabsetPanel(
      tabPanel(
        "Crude Incidence Rate and Wald's CI",
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
        "Exposure-Adjusted Incidence Rate and Wald's CI",
        fluidRow(column(
          4,
          numericInput(
            inputId = "effect_days",
            label = "Duration of Drug Action (days)",
            value = 0,
            min = 0,
            max = 1000
          )
        )),
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

