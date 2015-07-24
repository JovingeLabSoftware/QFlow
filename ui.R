
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)


shinyUI(fixedPage(
  fixedRow(
    column(3,
           fileInput('file1', 'Choose FCS File',
                     accept=c('text/csv', 
                              'text/comma-separated-values,text/plain', 
                              '.FCS', '.fcs')),
           tags$hr()
    ),
    column(9,
           plotOutput("history", width='700px')
    )
  ),    
  fixedRow(
    column(3,
             ""
    ),
    column(9,
           plotOutput("FL1", width='700px'),
           div(dataTableOutput("results"), style = "font-size: 9px; width: 300px;")           
    )
  )
))