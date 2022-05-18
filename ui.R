library(shiny)
library(ggplot2)
library(shinyWidgets)
library(kableExtra)
library(knitr)
library(gt)
library(gtsummary)
library(dplyr)

ui <- navbarPage(title = "Safety Database",
                 # create summary page for CSV upload and display
                 tabPanel(
                   title = "Summary",
                   
                   # Sidebar with a slider input for number of bins
                   sidebarLayout(
                     sidebarPanel(
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
                                     label = "Check if the file contains header",
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
                       tabPanel("original_data",
                                fluidRow(
                                  column(3, 
                                         pickerInput(
                                           inputId = "site",
                                           label = "Site",
                                           choices = NULL,
                                           multiple = TRUE,
                                           options = list("actions-box" = TRUE)
                                         )),
                                  column(3, 
                                         pickerInput(
                                           inputId = "subject",
                                           label = "Subject",
                                           choices = NULL,
                                           multiple = TRUE,
                                           options = list("actions-box" = TRUE)
                                         )),
                                  column(3, 
                                         pickerInput(
                                           inputId = "preferred_term",
                                           label = "Preferred Term",
                                           choices = NULL,
                                           multiple = TRUE, 
                                           options = list("actions-box" = TRUE)
                                         )),
                                  column(3, 
                                         pickerInput(
                                           inputId = "organ_class",
                                           label = "System Organ Class",
                                           choices = NULL,
                                           multiple = TRUE,
                                           options = list("actions-box" = TRUE)
                                         ))
                                ),
                                DT::dataTableOutput("contents")),
                       tabPanel("Summary Table", tableOutput("summary_table")),
                       tabPanel("Forest Plot and Confidence Interval", 
                                fluidRow(
                                               column(6,
                                                      pickerInput(
                                                        inputId = "treatment1",
                                                        label = "Treatment Group 1",
                                                        choices = NULL,
                                                        multiple = FALSE
                                                      )),
                                               column(6,
                                                      pickerInput(
                                                        inputId = "treatment2",
                                                        label = "Treatment Group 2",
                                                        choices = "",
                                                        multiple = FALSE
                                                      ))
                                               ),
                                column(6, plotOutput("crude_incidence", height = "750px")),
                                column(6, tableOutput("crude_wald"))
                                
                       ),
                       tabPanel("SOC vs. Treatment", 
                                fluidRow(column(6, plotOutput("heatmap")),
                                         column(6, plotOutput("pie_soc"))),
                                hr(),
                                fluidRow(tableOutput("soc_tr"))
                                
                                ),

                     #   tabPanel("Exposure Adjusted Incidence Rate",
                     #            # fluidRow(
                     #            #   column(6,
                     #            #          pickerInput(
                     #            #            inputId = "treatment1",
                     #            #            label = "Treatment Group 1",
                     #            #            choices = NULL,
                     #            #            multiple = FALSE
                     #            #          )),
                     #            #   column(6,
                     #            #          pickerInput(
                     #            #            inputId = "interest_ae",
                     #            #            label = "Adverse Event of Interest",
                     #            #            choices = "",
                     #            #            multiple = FALSE
                     #            #          ))
                     #            #   ),
                     #            fluidRow(tableOutput("eair_table"), textOutput("txt"))
                     # ),
                     tabPanel("EAIR", 
                                fluidRow("Most appropriate when the hazard rate of the specific event is relatively constant over the duration of the study. Not approriate for events that usually occur early in the study."),
                              fluidRow(
                                column(2, 
                                       pickerInput(
                                         inputId = "interest_ae",
                                                      label = "Adverse Event of Interest",
                                                      choices = "",
                                                      multiple = FALSE)),
                                column(5,
                                       pickerInput(
                                         inputId = "treatment1",
                                         label = "Treatment Group 1",
                                         choices = NULL,
                                         multiple = FALSE
                                       )),
                                column(5, pickerInput(
                                  inputId = "treatment2",
                                  label = "Treatment Group 2",
                                  choices = NULL,
                                  multiple = FALSE
                                ))
                              ),
                              fluidRow(
                                column(2, "Total Number of Group Population"),
                                column(5, 
                                       textInput(
                                         inputId = "trt1_pop",
                                         label = NULL,
                                         value = 30)),
                                column(5,
                                       textInput(
                                         inputId = "trt2_pop",
                                         label = NULL,
                                         value = 30))
                              ),
                              fluidRow(
                                column(2, "Duration of Treatment (years) Median (outside of dummy data)"),
                                column(5, 
                                       textInput(
                                         inputId = "trt1_year",
                                         label = NULL,
                                         value = 0.0)),
                                column(5,
                                       textInput(
                                         inputId = "trt2_year",
                                         label = NULL,
                                         value = 0.0))
                              ))
                     )
                   )
                   
                 ),
                 
                 # Second page
                 tabPanel(title = "Statistical Model")
                 
                 ))

# ui <- fluidPage(
#   # Application title
#   titlePanel("Safety Database Analysis"),
#   
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       # select a file to be analyzed
#       fileInput(
#         inputId = "file1",
#         label = "Choose a CSV file",
#         multiple = FALSE,
#         accept = c("text/csv",
#                    ".csv")
#       ),
#       
#       
#       # check if file has header
#       checkboxInput(inputId = "header",
#                     label = "Check if the file contains header",
#                     TRUE),
#       
#       radioButtons(
#         inputId = "sep",
#         label = "Type of Separator",
#         choices = c(
#           Comma = ",",
#           Semicolon = ";",
#           Tab = "\t",
#           None = ""
#         ),
#         selected = ","
#       ),
#       actionButton("choice", "incorporate external information"),
#       
#       #selectInput(inputId = "columns",
#       #label = "Columns to show",
#       #choices = NULL, multiple = TRUE),
#       
#       selectInput(
#         inputId = "rows",
#         label = "Rows to show",
#         choices = NULL,
#         multiple = TRUE
#       ),
#       tabsetPanel(
#         id = "tabs",
#         tabPanel("original_data",
#                  DT::dataTableOutput("contents")),
#         tabPanel("page2"),
#         tabPanel("page3")
#       )
#       
#     ),
#     # Show a plot of the generated distribution
#     mainPanel()
#     
#   ))


