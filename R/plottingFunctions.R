


#' Plot the current shape list object
#' @param shape shape list object containing x, y and color values
#' @param xOffset current x offset modifying shape object
#' @param yOffset current y offset modifying shape object
#' @return none (objects plotted to current device)
#' @author Jason Waddell
#' @export 
plotShape <- function(shape, xOffset, yOffset){
  for(i in seq_along(shape$y))
    rect(xleft = shape$x[i]+xOffset,
        xright = shape$x[i]+1+xOffset, 
        ybottom = shape$y[i]+yOffset,
        ytop = shape$y[i]+1+yOffset, 
        col = shape$col[i], 
        border = "black", lwd = 2)
}


#' Plot the background list object
#' @param bg list containing x, y and color values
#' @return none (objects plotted to current device)
#' @author Jason Waddell
#' @export 
plotBg <- function(bg){
  for(i in seq_along(bg$y))
    rect(xleft = bg$x[i],
        xright = bg$x[i]+1, 
        ybottom = bg$y[i],
        ytop = bg$y[i]+1, 
        col = bg$col[i], 
        border = "black", lwd = 2)
  
}

#' Make a plot displaying number of blocks in piece versus lambda
#' @param record the record reactive value object
#' @return none (objects plotted to current device)
#' @author Jason Waddell
#' @export 
plotDotplot <- function(record) { 
  
  if(length(record$lambda) > 0) {
    
    par(mar = c(0, 0, 0, 0))
    
    xRange <- diff(range(record$lambda)) + 0.5
    yMax <- max(record$value)
    
    xlim <- c(3.25 - 0.24 * xRange,
        max(record$lambda) + 0.26)
    ylim <- c(-0.5 - 0.22 * yMax, 
        max(3, max(record$value)*1.11))
    blankPlot(xlim = xlim, 
        ylim = ylim)
    rect(xlim[1], ylim[1], xlim[2], ylim[2], lwd = 4)
    
    text(x = mean(range(record$lambda)), y =  -0.5 - 0.135 * yMax, 
        labels = "Lambda")
    text(x = 3.25 - 0.15 * xRange, y =  max(max(record$value)*0.5, 1), 
        labels = "Piece \nSize")
    
    # xAxis
    segments(x0 = min(record$lambda) - 0.25, y0 = -0.5, 
        x1 = max(record$lambda) + 0.25,
        lwd = 2, col = gray(0.2))
    ticks <- unique(record$lambda)
    cexX <- 0.9
    if(max(record$lambda) > 6.5)
      cexX <- 0.7
    if(max(record$lambda) > 7)
      cexX <- 0.6
    for(i in 1:length(ticks)) {
      segments(x0 = ticks[i], 
          y0 = -0.5, y1 = -0.5 - 0.015 * yMax,
          lwd = 2)
      text(x = ticks[i], y = -0.5 - 0.025 * yMax,
          adj = c(0.5, 1), labels = ticks[i], 
          font = 2, cex = cexX)
    }
    
    # yAxis
    segments(x0 = 3.25,
        y0 = -0.5, y1 = max(2.8, max(record$value) * 1.05), col = gray(0.2))
    ticks <- seq(0, 20, by = 2)[which(seq(0, 20, by = 2) 
                <= max(2.8, max(record$value)))]
    for(i in 1:length(ticks)) {
      segments(x0 = 3.25 - 0.02 * xRange, 
          y0 = ticks[i], x1 = 3.25,
          lwd = 2)
      text(x = 3.25 - 0.04 * xRange,
          y = ticks[i], adj = c(1, 0.5), labels = ticks[i], 
          font = 2)
    }
    
    segments(x0 = min(record$lambda), x1 = max(record$lambda), 
        y0 = min(record$lambda), y1 = max(record$lambda), 
        col = grey(0.8), lwd = 2)
    
    x <- record$lambda
    y <- record$value
    
    xVals <- sort(unique(x))
    totalSpread <- 0.08
    
    globalX <- numeric(length = length(y))
    meanVec <- rep(0, length(xVals))
    
    for(i in 1:length(xVals)){
      
      index <- which(x == xVals[i])
      vec <- y[index]; 
      meanVec[i] <- mean(vec)
      
      maxRep <- max(table(vec))	
      xLeft = if(maxRep == 2) xVals[i] - totalSpread/4 else xVals[i] - totalSpread/2
      xRight = if(maxRep == 2) xVals[i] + totalSpread/4 else xVals[i] + totalSpread/2
      center <- (xLeft+xRight)/2 
      span <- abs(xRight - xLeft)
      space <- span/(maxRep-1)	
      
      xLocations <- numeric(length = length(vec))
      
      for(k in seq_along(table(vec))){
        numRep <- table(vec)[k]
        tempLoc <- findLocations(n = numRep, space = space, center = center)
        
        tempIndex <- which(vec == sort(unique(vec))[k])
        
        for(j in seq_along(tempIndex))
          xLocations[tempIndex[j]] <- sort(tempLoc)[j]	
        
      }
      
      globalX[index] <- xLocations
    }
    
    points(x = xVals, y = meanVec, type = "l", lwd = 2)
    points(x = globalX, y = y, pch = 19, col = record$color, cex = 1.5)
  } else {
    par(mar = c(0, 0, 0, 0))
    blankPlot(xlim = c(0, 1), 
        ylim = c(0, 1))
    rect(0, 0, 1, 1, lwd = 4)
  }
}



##' (defunct) plot the record of sampled ratio values (no longer works)
##' @param record 
##' @return 
##' @author Jason
##' @export
#plotRecord <- function(record) {
#  
#  par(mar = c(0, 0, 0, 0))
#  if(length(record$lambda) > 0) {
#    maxY <- max(3, max(record$ratio, na.rm=TRUE))
#    blankPlot(xlim = c(-0.07, 1), ylim = c(0, maxY + 0.5))
#    segments(x0 = 0, y0 = 1, x1 = 1, lwd = 2, col = "grey")
#    segments(x0 = 0, y0 = 0, y1 = 3, lwd = 3)
#    ticks <- c(0, 0.5, 1, 2, 3)
#    
#    text(x = 0.5, y = maxY + 0.4, labels = "x_i / E(x)", font = 2, 
#        cex = 2)
#    
#    for(i in 1:length(ticks)) {
#      segments(x0 = -0.02, y0 = ticks[i], x1 = 0, lwd = 2)
#      text(x = -0.04, y = ticks[i], adj = c(1, 0.5), labels = ticks[i], 
#          font = 2)
#    }
#    
#    record$xVal <- record$id / (max(record$id) + 1)
#    points(x = record$xVal, y = record$runningMean, pch = 19,
#        lwd = 2, type = "l")
#    points(x = record$xVal, y = record$ratio, pch = 19, 
#        col = as.character(record$color), 
#        cex = 2)
#  }
#  
#}

