
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(flowCore)
library(flowViz)
library(hexbin)

shinyServer(function(input, output) {
  data <- NULL

  output$FL1 <- renderPlot({
    inFile <- input$file1    
    if (is.null(inFile))
      return(NULL)
    data <- read.FCS(inFile$datapath, column.pattern = "^[FS0-9][S0-9]")
    par(mfrow=c(2,3), cex=0.7, mar=c(5,5,0,0))
    plotPair('SSC', 'FSC', data)    
    plotPair('530/40', '580/30', data)    
    plotPair('460/50', '670/30', data)    
    #Or
    #379/34 (355), 670/30 (355)
    plotPair('610/20', '585/29', data)    
    plotPair('710/50', '670/30', data)    
    plotPair('750', '710/50', data)    
  })  

  output$results <- renderDataTable({
    inFile <- input$file1    
    if (is.null(inFile))
      return(NULL)
    data <- read.FCS(inFile$datapath, column.pattern = "^[FS0-9][S0-9]")
    stats <- statsPair('530/40', '580/30', data)
    as.data.frame(t(round(stats, 3)))
  }, options = list(paging = FALSE, searching=FALSE))
  
})

plotPair <- function(a, b, data) {
  # extract column name and construct informative axis lael
  ix.a <- grep(a, colnames(data))[1]
  a <- colnames(data)[ix.a]
  a.l <- paste(a, pData(data@parameters)$desc[ix.a])
  
  ix.b <- grep(b, colnames(data))[1]
  b <- colnames(data)[ix.b]
  b.l <- paste(b, pData(data@parameters)$desc[ix.b])


  # filter
  n2Filter <- norm2Filter(a, b, scale.factor = 3, filterId = "km")
  fres <- filter(data, n2Filter)
  s1 <- split(data, fres)
  xy <- exprs(s1[[1]])
  
  # plot
  cr <- colorRampPalette(rev(c(rainbow(5, end = 4/6), 'white')))
  lim <- max(c(ceiling(max(xy[,a]/10000)),ceiling(max(xy[,b]/10000))))*10000  
  smoothScatter(xy[, a], xy[, b], colramp=cr, xlim=c(0, lim), ylim=c(0,lim), xlab=a.l, ylab=b.l)  
}

statsPair <- function(a, b, data) {
  # extract column name and construct informative axis lael
  ix.a <- grep(a, colnames(data))[1]
  a <- colnames(data)[ix.a]
  a.l <- paste(a, pData(data@parameters)$desc[ix.a])
  
  ix.b <- grep(b, colnames(data))[1]
  b <- colnames(data)[ix.b]
  b.l <- paste(b, pData(data@parameters)$desc[ix.b])
  
  
  # filter
  n2Filter <- norm2Filter(a, b, scale.factor = 3, filterId = "km")
  fres <- filter(data, n2Filter)
  s1 <- split(data, fres)
  xy <- exprs(s1[[1]])
    
  # stats
  ssc.rcv <- abs(IQR(xy[,'SSC'])) * 0.7413 / median(xy[,'SSC'])
  fsc.rcv <- abs(IQR(xy[,'FSC'])) * 0.7413 / median(xy[,'FSC'])
  a.rcv <- abs(IQR(xy[,a])) * 0.7413 / median(xy[,a])
  b.rcv <- abs(IQR(xy[,b])) * 0.7413 / median(xy[,b])
  ssc.med <- median(xy[,'SSC'])
  fsc.med <- median(xy[,'FSC'])
  a.med <- median(xy[,a])
  b.med <- median(xy[,b])
  
  res <- c(ssc.med, fsc.med, a.med, b.med, ssc.rcv, fsc.rcv, a.rcv, b.rcv)
  names(res) <- c('SSC med', 'FSC med', paste(a.l, 'med'), paste('b.l', 'med'), 
                  'SSC RCV', 'FSC RCV', paste(a.l, 'RCV'), paste('b.l', 'RCV'))
  return(res)
}
