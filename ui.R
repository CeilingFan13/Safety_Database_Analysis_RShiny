library(shiny)
library(ggplot2)
library(shinyWidgets)
library(kableExtra)
library(knitr)
library(gt)
library(gtsummary)

ui <- navbarPage(title = "Safety Database",
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
                                DT::dataTableOutput("contents")),
                       tabPanel("Summary Table", tableOutput("summary_table")),
                       tabPanel("page3", plotOutput("pie_soc"))
                     ))
                   )
                   
                 ),
                 
                 # Second page
                 tabPanel(title = "Page2")
                 
                 )

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


