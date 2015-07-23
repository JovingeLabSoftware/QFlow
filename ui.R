
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Upload QC Files"),
  tagList(
    singleton(tags$head(tags$script(src='//cdn.datatables.net/1.10.2/js/jquery.dataTables.js',type='text/javascript'))),
    singleton(tags$head(tags$script(src='//cdn.datatables.net/1.10.2/css/jquery.dataTables.min.css',type='text/css')))
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose FCS File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.FCS', '.fcs')),
      tags$hr()
    ),
    mainPanel(
      plotOutput("FL1"),
      dataTableOutput("results")
      #dataTableOutput('mytable')
    )
  )
))
