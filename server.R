
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(flowCore)
library(flowViz)

shinyServer(function(input, output) {
   
  output$FL1 <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.FCS("test_1.FCS", column.pattern = "^[FS0-9][S0-9]")
    
    p1 <- colnames(data)[3]
    p2 <- colnames(data)[4]
    
    n2Filter <- norm2Filter(p1, p2, scale.factor = 3, filterId = "km")
    fres <- filter(data, n2Filter)
    s1 <- split(data, fres)
    plot(data, c("FSC", "SSC"))
  })  
})
