#' Detects whether a proposed change in the active piece will cause a collision in 
#' the background
#' @param values reactive values object with current piece and bg information
#' @param xChange proposed x location change
#' @param yChange proposed y location change
#' @param rotateClockwise (logical) whether to rotate the piece clockwise
#' @param rotateCounter  (logical) whether to rotate the piece counter clockwise
#' @return (logical) whether a collision will occur
#' @author Jason Waddell
#' @export 
detectCollision <- function(values, xChange = 0, yChange = 0, 
    rotateClockwise = FALSE, rotateCounter = FALSE) {
  
  tmpShape <- values$shape
  if(rotateClockwise)
    tmpShape <- rotateClockwise(tmpShape)
  if(rotateCounter)
    tmpShape <- rotateCounter(tmpShape)
  
  tmpShape$x <- tmpShape$x + values$xOffset + xChange
  tmpShape$y <- tmpShape$y + values$yOffset + yChange
  
  if(min(tmpShape$y) < 0) 
    return(TRUE)
  
  if(max(tmpShape$x) > 9)
    return(TRUE)
  
  if(min(tmpShape$x) < 0)
    return(TRUE)
  
  if(length(values$bg$y) > 0) 
    if((min(tmpShape$y) <= max(values$bg$y))) {
      
      idx <-which(values$bg$y >= min(tmpShape$y))
      
      z1 <- paste(tmpShape$x, tmpShape$y, sep = ",")
      z2 <- paste(values$bg$x[idx], values$bg$y[idx], sep = ",")
      if(any(z1 %in% z2))
        return(TRUE)
    }
  
  return(FALSE)
  
}


#' Check if any lines need to be cleared, and update objects accordingly
#' @param values reactive values object with current piece and bg information
#' @return (numeric) number of lines cleared
#' @author Jason Waddell
#' @export 
clearLines <- function(values) {
  
  yTable <- table(values$bg$y)  
  if(max(yTable) == 10) {
    remLines <- as.numeric(names(which(yTable == 10)))
    if(length(values$bg$y) == (length(remLines) * 10)) {
      values$bg <- makeBg()
    } else {
      idx <- which(values$bg$y %in% remLines)
      values$bg$y <- values$bg$y[-idx]
      values$bg$x <- values$bg$x[-idx]
      values$bg$col <- values$bg$col[-idx]
      
      for(i in 1:length(values$bg$y)) {
        values$bg$y[i] <- 
            values$bg$y[i] - sum(remLines < values$bg$y[i])
      }

    }
      
    return(length(remLines))
  }
  return(0)  # return TRUE for altering fal intervall
}



#' Create a blank background list
#' @return the background list
#' @author Jason Waddell
#' @export 
makeBg <- function() {  
  bg <- list(x=numeric(), y=numeric(), col=character())
  return(bg)  
}


#' When generating a new piece, find candidate block locations
#' @param pieceMat current piece data.frame
#' @return list of candidate locations
#' @author Jason Waddell
#' @export 
findOpenLocations <- function(pieceMat){
  ll <- list()
  for(i in 1:nrow(pieceMat))
    ll[[i]] <- adjacentLocations(pieceMat[i,])
  
  ll <- do.call("rbind.data.frame", ll)
  ll <- ll[!duplicated(ll),] # to find the unique rows
  
  ll <- ll[!duplicated(rbind.data.frame(ll, pieceMat), 
          fromLast = TRUE)[1:nrow(ll)],]
  
  return(ll)
}

#' Find adjacent locations to a given block
#' @param pieceRow One row of the current piece data.frame
#' @return matrix of adjacent locations
#' @author Jason Waddell
#' @export 
adjacentLocations <- function(pieceRow){
  x <- as.numeric(pieceRow[,1]); y <- as.numeric(pieceRow[,2])
  xOut <- c(x, x+1, x, x-1)
  yOut <- c(y+1, y, y-1, y)
  out <- cbind(xOut, yOut); colnames(out) <- c("x", "y")
  return(out)
}

#' Make a randomly generated piece, sampled from the current lambda value
#' @param lambda value of lambda (to be handed to rpois)
#' @return a piece list object
#' @author Jason Waddell
#' @export 
makeShape <- function(lambda=3.5){
  
  colVec <- oaPalette(9)
  myCol <- sample(colVec, 1)
  
  value <- rpois(1, lambda)
  n <- max(value, 1)
  
  x <- 1; y <- 1; pieceMat <- cbind.data.frame(x, y)
  
  if(n > 1)
    for(i in 2:n){
      openSpaces <- findOpenLocations(pieceMat)
      nextSpace <- openSpaces[sample(1:nrow(openSpaces), size = 1),]
      pieceMat <- rbind.data.frame(pieceMat, nextSpace)
    }		
  
  pieceMat$x <- pieceMat$x - round(median(pieceMat$x))
  pieceMat$y <- pieceMat$y - round(median(pieceMat$y))
  pieceMat <- cbind.data.frame(pieceMat, col = rep(myCol, n), 
      value = rep(value, n), 
      lambda = rep(lambda, n))
  pieceMat$col <- as.character(pieceMat$col)
  pieceMat <- as.list(pieceMat)
  
  return(pieceMat)
}




#' Add the current piece to the background (because of a collision)
#' @param values reactive values object with current piece and bg information 
#' @param lambda  current lambda value
#' @param nextPiece the reactive value object for the next piece (to become the active piece)
#' @return values reactive values object with current piece and bg information
#' @author Jason Waddell
#' @export 
addPieceToBg <- function(values, lambda, nextPiece) {
  values$shape$x <- values$shape$x + values$xOffset
  values$shape$y <- values$shape$y + values$yOffset
  
  values$bg$x <- c(values$bg$x, values$shape$x)
  values$bg$y <- c(values$bg$y, values$shape$y)
  values$bg$col <- c(values$bg$col, values$shape$col)
  values$shape <- nextPiece$shape
  values$xOffset <- 4
  values$yOffset <- 20
  return(values)
}



#' Rotate a piece clockwise
#' @param shape the shape list object
#' @return updated shape list object
#' @author Jason Waddell
#' @export 
rotateClockwise <- function(shape) {
  
  tmpX <- shape$x
  shape$x <- shape$y
  shape$y <- -1 * tmpX
  return(shape)
}

#' Rotate a piece counterclockwise
#' @param shape the shape list object
#' @return updated shape list object
#' @author Jason Waddell
#' @export 
rotateCounter <- function(shape) {
  
  tmpY <- shape$y
  shape$y <- shape$x
  shape$x <- -1 * tmpY
  return(shape)
}
