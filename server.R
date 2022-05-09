library(readxl)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$contents <- renderTable({
    # make sure there is input file
    req(input$file1)
    # determine method to open file according to file type
    if (input$format == "Excel") {
      df <- read_excel(input$file1$datapath)
      return(df)
    }
    if (input$format == "CSV") {
      df <-
        read.table(input$file1$datapath,
                   header = input$header,
                   sep = input$sep)
      return(df)
    }
  })
}




