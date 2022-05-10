library(readxl)
library(shiny)
library(ggplot2)
library(Dict)
library(stringr)
ctcae <- 
server <- function(input, output, session) {
  # enable column and row selection
  info <- eventReactive(input$choice, {
    inFile <- input$file1
    req(inFile)
    df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep, blank.lines.skip = TRUE)
    #names(df) <- str_replace_all(names(df), c(" " = "_"))
    # change all PT and SOC to lowercase
    df$PT <- tolower(df$PT)
    df$SOC <- tolower(df$SOC)
    # get list of unique items to filter
    subject_list <- c(unique(df$Subject.ID))
    site_list <- c(unique(df$Site))
    pt_list <- c(unique(df$PT))
    soc_list <- c(unique(df$SOC))
    #c_vars <- names(df)
    r_vars <- row.names(df)
    #updateSelectInput(session, "columns", 
                      #"Columns to show", choices = c_vars)
    updateSelectInput(session, "rows", 
                      "Rows to show", choices = r_vars,
                      selected = r_vars)
    updatePickerInput(session, "subject", 
                      "Subject", choices = subject_list,
                      selected = subject_list)
    updatePickerInput(session, "site", 
                      "Site", choices = site_list,
                      selected = site_list)
    updatePickerInput(session, "preferred_term", 
                      "Preferred Term", choices = pt_list,
                      selected = pt_list)
    updatePickerInput(session, "organ_class", 
                      "System Organ Class", choices = soc_list,
                      selected = soc_list)
    return(df)
  })
   output$contents <- DT::renderDataTable({
     data <- info()
     data <- subset(data[input$rows,])#, select = input$columns)
     return(data)
  })
}




