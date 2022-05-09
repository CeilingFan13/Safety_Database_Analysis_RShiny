library(readxl)
library(shiny)
library(ggplot2)

server <- function(input, output, session) {
  # enable column and row selection
  info <- eventReactive(input$choice, {
    inFile <- input$file1
    req(inFile)
    df <- read.table(input$file1$datapath,
                     header = input$header,
                     sep = input$sep)
    c_vars <- names(df)
    r_vars <- row.names(df)
    updateSelectInput(session, "columns", 
                      "Columns to show", choices = c_vars)
    updateSelectInput(session, "rows", 
                      "Rows to show", choices = r_vars)
    return(df)
  })
   output$contents <- DT::renderDataTable({
     data <- info()
     data <- subset(data[input$rows,], select = input$columns)
     return(data)
  })
}




