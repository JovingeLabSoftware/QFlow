
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
   
  output$FL1 <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.FCS(inFile$datapath, column.pattern = "^[FS0-9][S0-9]")
    data <- read.FCS("../test_1.fcs", column.pattern = "^[FS0-9][S0-9]")
    
    ix.530 <- grep("530/40", colnames(data))[1]
    x530 <- colnames(data)[ix.530]
    x530.l <- paste(x530, pData(data@parameters)$desc[ix.530])

    ix.580 <- grep("580/30", colnames(data))[1]
    x580 <- colnames(data)[ix.580]
    x580.l <- paste(x580, pData(data@parameters)$desc[ix.580])
    
    n2Filter <- norm2Filter(x580, x530, scale.factor = 3, filterId = "km")
    fres <- filter(data, n2Filter)
    s1 <- split(data, fres)
    xy <- exprs(s1[[1]])

    cr <- colorRampPalette(rev(c(rainbow(5, end = 4/6), 'white')))
    par(mfrow=c(1,2))
    
    smoothScatter(xy[, x580], xy[,x530], colramp=cr, xlim=c(0, 50000), ylim=c(0,50000), xlab=x580.l, ylab=x530.l)
    eval(parse(text=paste("xyplot(`", x580, "` ~ `", x530, "`, smooth=FALSE, bg='red', data=s1[[1]], checkName=FALSE)", sep="")))    
    eval(parse(text=paste("xyplot(`", 'SSC', "` ~ `", 'FSC', "`, smooth=FALSE, bg='red', data=s1[[1]], checkName=FALSE)", sep="")))    
    
    xy <- exprs(s1[[1]])
    abs(IQR(xy[,x530])) * 0.7413 / median(xy[,x530])
    #plot(hexbin(xy[,'530/40 (488)'], xy[,'580/30 (488)'], xbins=200), colramp=cr)        
    #abs(IQR(tt)) * 0.7413 / median(tt)
    
  })  
})

processPair <- function(a, b, data) {
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
  smoothScatter(xy[, 'FSC'], xy[, 'SSC'], colramp=cr, xlim=c(0, lim), ylim=c(0,lim), xlab="FSC", ylab="SSC")  
 
  # stats
  ssc.rcv <- abs(IQR(xy[,'SSC'])) * 0.7413 / median(xy[,'SSC'])
  fsc.rcv <- abs(IQR(xy[,'FSC'])) * 0.7413 / median(xy[,'FSC'])
  a.rcv <- abs(IQR(xy[,a])) * 0.7413 / median(xy[,a])
  b.rcv <- abs(IQR(xy[,b])) * 0.7413 / median(xy[,b])
  return(list(rcv=list(ssc=ssc.rcv, fsc=rsc.rcv, a=a.rcv, b=b.rcv)))
}
