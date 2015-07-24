
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(flowCore)
library(flowViz)
library(hexbin)



shinyServer(function(input, output, session) {
  data <- NULL
  historical <- reactiveFileReader(1000, session, 'rcv.csv', read.csv,
                                   header = TRUE, row.names=1, 
                                   check.names=FALSE, as.is=TRUE)
  
  output$FL1 <- renderPlot({
    UV <- FALSE
    inFile <- input$file1    
    if (is.null(inFile))
      return(NULL)

    for(i in 1:nrow(inFile)) {
      # QC is performed with and without UV laser (which blows everything else away).
      # so find the file that contains the UV data and extract that separately
      d <- read.FCS(inFile$datapath[i], column.pattern = "^[FS0-9][S0-9]")      
      if(median(exprs(d)[,grep("355", names(d))]) > 5000) {
        dataUV <- d
        UV <- TRUE
      } else {
        data <- d
      }
    }
    par(mfrow=c(3,3), cex=0.7, mar=c(5,5,1,0))
    plotPair('SSC', 'FSC', data)    
    
    #if UV dataset present, verify which filter set in use
    if(UV && length(grep('460/50 \\(355', names(dataUV)))) {
      plotPair('460/50 \\(355', '670/30 \\(355', dataUV)          
    } else if (UV) {
      plotPair('379/34 \\(355', '670/30 \\(355', dataUV)          
    }

    plotPair('520/35 \\(405', '460/50 \\(405', data)    
    
    plotPair('530/40 \\(488', '580/30 \\(488', data)    
    
    plotPair('610/20 \\(561', '585/29 \\(561', data)    
    plotPair('710/50 \\(561', '670/30 \\(561', data)    
    plotPair('750.* \\(561', '710/50 \\(561', data)    
    
    plotPair('720/40 \\(640', '670/30 \\(640', data)    
    plotPair('750.* \\(640', '720/40 \\(640', data)    
    
    })  

  output$history <- renderPlot({
    rcv <- historical()
    matplot(x=matrix(1:ncol(rcv)), y=t(rcv), type='l', col=rainbow(12), 
          lty=1, ylim=c(0,15), 
          xlab="date", 
          ylab="RCV",
          cex.lab = 0.7, font.lab = 2, 
          axes=FALSE)
    legend(x="topleft", legend = rownames(rcv), lty=1, col=rainbow(16), box.lty = 0, cex=0.7, ncol = 3)
    axis(2, cex.axis=0.5, las=1)
    #axis(1, labels = colnames(rcv), at=1:ncol(rcv), cex.axis=0.7, las=1)
    text(x=c(1:ncol(rcv)), par("usr")[3] - 0.2, labels = gsub("[a-zA-Z_]", "", colnames(rcv)), srt = 45, cex=0.7,  pos = 2, xpd = TRUE)
    box()    
  })

  output$results <- renderDataTable({
    UV <- FALSE
    inFile <- input$file1    
    if (is.null(inFile))
      return(NULL)
    
    for(i in 1:nrow(inFile)) {
      # QC is performed with and without UV laser (which blows everything else away).
      # so find the file that contains the UV data and extract that separately
      d <- read.FCS(inFile$datapath[i], column.pattern = "^[FS0-9][S0-9]")      
      if(median(exprs(d)[,grep("355", names(d))]) > 5000) {
        dataUV <- d
        UV <- TRUE
      } else {
        data <- d
      }
    }
    
    stats <- statsPair('SSC', 'FSC', data)
    #if UV dataset present, verify which filter set in use
    if(UV && length(grep('460/50 \\(355', names(dataUV)))) {
      stats <- rbind(stats, statsPair('460/50 \\(355', '670/30 \\(355', dataUV))
    } else if (UV) {
      stats <- rbind(stats, statsPair('379/34 \\(355', '670/30 \\(355', dataUV))
    }
    stats <- rbind(stats, statsPair('520/35 \\(405', '460/50 \\(405', data))

    stats <- rbind(stats, statsPair('530/40 \\(488', '580/30 \\(488', data))

    stats <- rbind(stats, statsPair('610/20 \\(561', '585/29 \\(561', data))
    stats <- rbind(stats, statsPair('710/50 \\(561', '670/30 \\(561', data))
    stats <- rbind(stats, statsPair('750.* \\(561', '710/50 \\(561', data))

    stats <- rbind(stats, statsPair('720/40 \\(640', '670/30 \\(640', data))
    stats <- rbind(stats, statsPair('750.*\\(640', '720/40 \\(640', data))    
    stats <- stats[-which(duplicated(row.names(stats))),]
    
    rcv <- read.csv(file = "rcv.csv", header = TRUE, row.names=1, check.names=FALSE, as.is=TRUE)
    cn <- gsub(".*?([0123456789_]+).*", "\\1", input$file1[1,1])     
    if(!(cn %in% colnames(rcv))) {
      rcv <- cbind(rcv, stats[,-1])
      colnames(rcv)[ncol(rcv)] <- cn
      write.csv(rcv, file="rcv.csv")
    }
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
