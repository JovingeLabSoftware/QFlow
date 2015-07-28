
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)


shinyUI(fixedPage(
    
  # header wrapper for lab site
  includeHTML('www/header.html'),
  HTML('<link href="shared/font-awesome/css/font-awesome.min.css" rel="stylesheet">'),


  fixedRow(
    column(12, tag('br', ""))),
  fixedRow(
    column(4,
           fileInput('file1', 'FCS Files (w/ &w/o UV)', multiple=TRUE,
                     accept=c('text/csv', 
                              'text/comma-separated-values,text/plain', 
                              '.FCS', '.fcs')),
           tags$hr()
    ),
    column(8,
           h4("Historical data (select FCS file at left to add data)"),
           plotOutput("history", width='700px')
    )
  ),    
  fixedRow(
    column(4,
             ""
    ),
    column(8,
           h4("Diagnostic plots (select FCS file at left):"),
           plotOutput("FL1", width='700px'),
           div(dataTableOutput("results"), style = "font-size: 9px; width: 300px;")           
    )
  ),
  includeHTML('www/footer.html')
))