
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Upload QC Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose FCS File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.FCS', '.fcs')),
      tags$hr()
    ),
    mainPanel(
      plotOutput("FL1")
    )
  )
))
