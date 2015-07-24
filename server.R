
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
    par(mfrow=c(2,3), cex=0.7, mar=c(5,5,1,0))
    plotPair('SSC', 'FSC', data)    
    plotPair('530/40', '580/30', data)    
    plotPair('460/50', '670/30', data)    
    #Or
    #379/34 (355), 670/30 (355)
    plotPair('610/20', '585/29', data)    
    plotPair('710/50', '670/30', data)    
    plotPair('750', '710/50', data)    
  })  

  output$history <- renderPlot({
    load("data.rda")
    matplot(x=matrix(1:ncol(rcv)), y=t(rcv), type='l', col=rainbow(12), 
          lty=1, ylim=c(0,10), 
          xlab="date", 
          ylab="RCV",
          cex.lab = 0.7, font.lab = 2, 
          axes=FALSE)
    legend(x="topleft", legend = rownames(rcv), lty=1, col=rainbow(12), box.lty = 0, cex=0.5, ncol = 2)
    axis(2, cex.axis=0.5, las=1)
    axis(1, labels = colnames(rcv), at=1:ncol(rcv), cex.axis=0.5, las=1)
    box()    
  })

  output$results <- renderDataTable({
    inFile <- input$file1    
    if (is.null(inFile))
      return(NULL)
    data <- read.FCS(inFile$datapath, column.pattern = "^[FS0-9][S0-9]")
    stats <- statsPair('SSC', 'FSC', data)
    stats <- rbind(stats, statsPair('530/40', '580/30', data))
    stats <- rbind(stats, statsPair('460/50', '670/30', data))
    stats <- rbind(stats, statsPair('610/20', '585/29', data))
    stats <- rbind(stats, statsPair('710/50', '670/30 \\(561', data))
    stats <- rbind(stats, statsPair('750', '710/50', data))
    stats <- stats[-12,] # duplicate 710/50
    load("data.rda")
    rcv <- cbind(rcv, stats[,-1])
    colnames(rcv)[ncol(rcv)] <- gsub(".*?\\s(.*?\\s.*?)\\s.*", "\\1", date())
    save(rcv, file="data.rda")
    stats <- cbind(rownames(stats), round(stats, 3))
    colnames(stats)[1] <- "Parameter"

    as.data.frame(stats)
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
  a.rcv <- 100 * abs(IQR(xy[,a])) * 0.7413 / median(xy[,a])
  b.rcv <- 100 * abs(IQR(xy[,b])) * 0.7413 / median(xy[,b])
  a.med <- median(xy[,a])
  b.med <- median(xy[,b])
  res <- matrix(c(a.med, b.med, a.rcv, b.rcv), ncol=2)
  colnames(res) <- c("median", "RCV") 
  rownames(res) <- c(a.l, b.l)
#  res <- c(a.med, b.med, a.rcv, b.rcv)
#  names(res) <- c(paste(a.l, 'med'), paste(b.l, 'med'), 
#                  paste(a.l, 'RCV'), paste(b.l, 'RCV'))
  return(res)
}
