# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Safety Database Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # select a file to be analyzed
      fileInput(inputId = "file1",
                label = "Choose Excel or CSV file",
                multiple = FALSE,
                accept = c("text/csv", 
                          ".csv", ".xlsx")),
    
      # horizontal line
    #tag$hr(),
    
#    fluidRow(
#      column(4, selectInput(c("All", unique(as.character(df$AE))))
#    ),
    
    # check if file has header
    radioButtons(
      inputId = "format",
      label = "Check the format of input file",
      choices = c("CSV", "Excel"),
      selected = "Excel"),
    
    # check if file has header
    checkboxInput(
      inputId = "header",
      label = "Check if the file contains header",
      TRUE),
    
    radioButtons(inputId = "sep", label = "Type of Separator",
      choices = c(Comma = ",", Semicolon = ";", Tab = "\t", None = ""),
      selected = "")
    
  ),
  # Show a plot of the generated distribution
  mainPanel(tableOutput("contents"))
  
))


