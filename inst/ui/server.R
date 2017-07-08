

function(input, output, session) {
  
  # game states
  gameOver <- FALSE
  gameActive <- FALSE
  
  # how many frames to hold a button before it fires again
  holdDuration <- 4
  
  # can the down key be pressed (sets to false on line clear, true on keyup)
  canPressDown <- TRUE
  
  # duration of button press (how many frames has it been held)
  leftDuration <- 0
  rightDuration <- 0
  downDuration <- 0
  clockDuration <- 0
  counterDuration <- 0
  noDownCounter <- 0
  
  # old counter values
  leftCounterOld <- 0
  downCounterOld <- 0
  rightCounterOld <- 0
  clockCounterOld <- 0
  counterCounterOld <- 0
  
  # game data
  lambda <- 3.5
  score <- 0
  pieceMoved <<- TRUE
  
  # plot reactive value
  rv <- reactiveValues()
  
  # debugging
#  tickTocks <- 0
#  oldTime <- Sys.time()
  
  ##########################
  # define reactive values #
  nextPiece <- reactiveValues(shape = makeShape(lambda))
  observeEvent(values$numShapes, {
        nextPiece$shape <- makeShape(lambda)
      })
  
  values <- reactiveValues(yOffset = 20, 
      shape = makeShape(lambda),
      xOffset = 5, 
      bg = makeBg(), 
      numShapes = 0,
      highScore = if (file.exists("highScore.rds")) readRDS("highScore.rds") else 0)
  
  
  ######################
  #    baseline plot   #
  s1 <- svgstring(standalone=FALSE, height=600/72, width=297/72)
  par(mar = c(0, 0, 0, 0))
  blankPlot(xlim = c(-0.5, 10.5), 
      ylim = c(-0.5, 22.5))   # , asp = 1
  rect(xleft = 0, xright = 10,
      ybottom = 20, ytop = 22, 
      lwd = 4, border = NA, col = grey(0.95))
  for(i in 1:9)        # TODO replace 
    segments(x0 = i, y0 = 0, y1 = 22, 
        col = gray(0.7))
  segments(x0 = 0, x1 = 10, y0 = 0, lwd = 4)
  segments(x0 = 0, y0 = 0, y1 = 22, lwd = 4)
  segments(x0 = 10, y0 = 0, y1 = 22, lwd = 4)
  dev.off()
  output[["s1"]] <- renderUI({ HTML(s1()) })
  
  
  
  ############################
  # plots and value updating #
  ms <- 33
  tickTock30 <- reactiveTimer(ms)
  observeEvent(tickTock30(), {
        
#        tickTocks <<- tickTocks + 1
#        if(tickTocks %% 30 == 0) {
#          newTime <- Sys.time()
#          print(as.numeric(difftime(newTime, oldTime)))
#          oldTime <<- newTime
#        }

        # TODO think about using the %% mod functions again
        
        if(gameActive) {
          
          
          # left movement
          if(input$leftCounter == leftCounterOld & input$left == 50) {
            leftDuration <<- leftDuration + 1
          } else 
            leftDuration <<- 0
          
          if(input$leftCounter > leftCounterOld | 
              (leftDuration > holdDuration)) {  
            if(!detectCollision(values, xChange = -1)) {
              values$xOffset <- values$xOffset - 1
              pieceMoved <<- TRUE
            }
            leftCounterOld <<- input$leftCounter
          }
          
          # right movement
          if(input$rightCounter == rightCounterOld & input$right == 50) {
            rightDuration <<- rightDuration + 1
          } else 
            rightDuration <<- 0
          
          if(input$rightCounter > rightCounterOld | 
              (rightDuration > holdDuration)) {  
            if(!detectCollision(values, xChange = +1)){
              values$xOffset <- values$xOffset + 1
              pieceMoved <<- TRUE
            }
            rightCounterOld <<- input$rightCounter
            
          }
          
          
          # clockwise rotation
          if(input$clock > clockCounterOld) {
            out <- detectCollision(values, rotateClockwise = TRUE)
            if(!out[[1]]) {
              if(length(out) > 1)
                values$xOffset <- values$xOffset + out$xChange
              values$shape <- rotateClockwise(values$shape)
              
              clockCounterOld <<- input$clock
              pieceMoved <<- TRUE
            }
          }
          
          
          # counter rotation
          if(input$counter > counterCounterOld){
            out <- detectCollision(values, rotateCounter = TRUE)
            if(!out[[1]]) {
              if(length(out) > 1)
                values$xOffset <- values$xOffset + out$xChange
              values$shape <- rotateCounter(values$shape) 
              
              counterCounterOld <<- input$counter
              pieceMoved <<- TRUE
            }
          }
          
          # the down key is not pressed
          if(input$down == 30)
            canPressDown <<- TRUE
          
          # down movement
          noDownCounter <<- noDownCounter + 1
          if(input$downCounter == downCounterOld & input$down == 50) {
            downDuration <<- downDuration + 1
          } else 
            downDuration <<- 0
          
          if(noDownCounter >= 450/ms | (canPressDown & (input$downCounter > downCounterOld)) | 
              (canPressDown & (downDuration > holdDuration))) {   #& downDuration %% holdDuration == 1
            pieceMoved <<- TRUE
            
            downCounterOld <<- input$downCounter
            if(detectCollision(values, yChange = -1)) { 
              
              canPressDown <<- FALSE
              
              # update values          
              values <- addPieceToBg(values, lambda, nextPiece)
              swapAvailable <<- 1
              
              # clear lines and change score
              linesCleared <- clearLines(values)
              if(linesCleared > 0) {
                score <<- score + lambda + (linesCleared-1)*2*lambda
                lambda <<- lambda + 0.25 * linesCleared
              }
              values$numShapes <- values$numShapes + 1
              
              # plot the blocks that have already fallen
              s2 <- svgstring(standalone=FALSE, height=600/72, width=297/72, 
                  bg = "transparent")
              par(mar = c(0, 0, 0, 0))
              blankPlot(xlim = c(-0.5, 10.5), 
                  ylim = c(-0.5, 22.5))   # , asp = 1
              plotBg(values$bg)
              dev.off()
              rv$s2 <- s2() 
              
              # check for game end
              if(any(values$bg$y > 19)) {
                gameOver <<- TRUE
                gameActive <<- FALSE
              }
              
              
              
              
            } else {
              # move the piece if no collision
              values$yOffset <- values$yOffset - 1
            }
            downWasPressed <<- FALSE
            noDownCounter <<- 0
            
          }
        }
        
        if(pieceMoved) {
          # plot the active block
          s3 <- svgstring(standalone=FALSE, height=600/72, width=297/72, 
              bg = "transparent")
          par(mar = c(0, 0, 0, 0))
          blankPlot(xlim = c(-0.5, 10.5), 
              ylim = c(-0.5, 22.5))   # , asp = 1
          plotShape(values$shape, xOffset = values$xOffset,
              yOffset = values$yOffset)
          dev.off()
          rv$s3 <- s3() 
          pieceMoved <<- FALSE
        }
        
        if(!gameActive) {
          s4 <- svgstring(standalone=FALSE, height=600/72, width=297/72, 
              bg = "transparent")
          par(mar = c(0, 0, 0, 0))
          blankPlot(xlim = c(-0.5, 10.5), 
              ylim = c(-0.5, 22.5))   # , asp = 1
          rect(xleft = 1.5, xright = 8.5,
              ybottom = 10, ytop = 14, 
              lwd = 4, col = "white")
          text(x = 5, y = 13, "Press Start to Begin")
          if(gameOver) {
            text(x = 5, y = 11, paste0("Score: ", score))
            if (score >= values$highScore) {
              saveRDS(score, "highScore.rds")
              values$highScore <- score
              text(x = 5, y = 12, "High Score!", col = "red")
            }
          }
          
          dev.off()
          rv$s4 <- s4() 
        } 
        
      })
  
  
  # ----------- other layered plots ------------- #
  
  output[["s2"]] <- renderUI({ HTML(rv$s2) })
  output[["s3"]] <- renderUI({ HTML(rv$s3) })
  output[["s4"]] <- renderUI({ HTML(rv$s4) })
  
  
  
  observeEvent(nextPiece$shape, {
        s <- svgstring(standalone=FALSE, height=170/72, width=170/72)
        par(mar = c(0, 0, 0, 0))
        valVec <- c(nextPiece$shape$x, nextPiece$shape$y)
        valRange <- c(min(-3.5, -1*max(abs(valVec))-0.5),
            max(max(abs(valVec))+1.5, 4.5) )
        
        blankPlot(xlim = valRange, ylim = valRange)   # , asp = 1
        rect(valRange[1], valRange[1],
            valRange[2], valRange[2], lwd = 4, col = NA, bg = NA)
        plotShape(nextPiece$shape, xOffset = 0,
            yOffset = 0)
        dev.off()
        rv$nextPiece <- s() 
      })
  output[["nextPiece"]] <- renderUI({ HTML(rv$nextPiece) })
  
  observeEvent(values$shape, {
        s <- svgstring(standalone=FALSE, height=300/72, width=450/72)
        par(mar = c(0, 0, 0, 0))
        blankPlot(xlim = c(0, 11), ylim = c(-0.07, 0.25))
        rect(0, -0.07, 11, 0.25, lwd = 4)
        numBlocks <- length(values$shape$y)
        
        for(i in 2:9) {
          rectCol <- ifelse(i == numBlocks, values$shape$col, "white")
          rect(xleft = i-0.5, ybottom = 0, xright=i+0.5, ytop = dpois(i, lambda), 
              lwd = 3, col = rectCol)
        }
        
        rectCol <- ifelse(numBlocks %in% c(0, 1), values$shape$col, "white")
        rect(xleft = 0.5, ybottom = 0, xright=1.5, 
            ytop = ppois(1, lambda), 
            lwd = 3, col = rectCol)
        
        rectCol <- ifelse(numBlocks > 9, values$shape$col, "white")
        rect(xleft = 10-0.5, ybottom = 0, xright=10+0.5, 
            ytop = ppois(10, lambda, lower.tail = FALSE), 
            lwd = 3, col = rectCol)
        
        text(x = c(1:9, 10.1), y = rep(-0.01, 10), labels = c("0-1", 2:9, "10+"), 
            font = 2, adj = c(0.5, 1), cex = 1.2)
        text(x = 5.5, y = -0.04, labels = "Piece Size (# Blocks)", 
            font = 1, adj = c(0.5, 1), cex = 1.4)
        dev.off()
        rv$pdf <- s() 
      })
  output[["pdf"]] <- renderUI({ HTML(rv$pdf) })
  
  
  sControl <- svgstring(standalone=FALSE, height=170/72, width=170/72)
  par(mar = c(0, 0, 0, 0))
  blankPlot(xlim = c(-0.6, 1), ylim = c(-0.3, 1.3))
  points(x = c(0.5, 0.75, 0.5, 0.25), y = c(0.25, 0.5, 0.75, 0.5), 
      pch = 21, bg = oaColors(c("green", "red", "yellow", "cyan")), 
      cex = 4)
  
  text(x = 0.5, y = 0.95, adj = c(0.5, 0), "Swap Pieces", font = 2, 
      cex = 0.8)
  text(x = 0, y = 0.5, adj = c(1, 0.5), "Rotate \nCounter-\nClockwise",
      font = 2, cex = 0.8)
  text(x = 0.5, y = 0.06, adj = c(0.5, 1), 
      "Rotate \nClockwise", font = 2, cex = 0.8)
  rect(-0.6, -0.3, 1, 1.3, lwd = 4)
  dev.off()
  output[["controls"]] <- renderUI({ HTML(sControl()) })
  
  
  # ------------  game functions --------------- #
  
  observeEvent(input$startGame, {
        gameActive <<- TRUE
        
        # remove the start game box
        s4 <- svgstring(standalone=FALSE, height=600/72, width=297/72, 
            bg = "transparent")
        par(mar = c(0, 0, 0, 0))
        blankPlot(xlim = c(-0.5, 10.5), 
            ylim = c(-0.5, 22.5))   # , asp = 1
        dev.off()
        rv$s4 <- s4() 
        
        if(gameOver) {     
          
          lambda <<- 3.5
          score <<- 0
          
          values$yOffset <- 20
          values$shape <- makeShape(lambda)
          values$xOffset <- 5
          values$bg = makeBg() 
          values$numShapes = 0
          
          nextPiece$shape <- makeShape(lambda)
          
          # reset the fallen blocks
          s2 <- svgstring(standalone=FALSE, height=600/72, width=297/72, 
              bg = "transparent")
          par(mar = c(0, 0, 0, 0))
          blankPlot(xlim = c(-0.5, 10.5), 
              ylim = c(-0.5, 22.5))   # , asp = 1
          plotBg(values$bg)
          dev.off()
          rv$s2 <- s2()
          
          # draw the new active piece
          s3 <- svgstring(standalone=FALSE, height=600/72, width=297/72, 
              bg = "transparent")
          par(mar = c(0, 0, 0, 0))
          blankPlot(xlim = c(-0.5, 10.5), 
              ylim = c(-0.5, 22.5))   # , asp = 1
          plotShape(values$shape, xOffset = values$xOffset,
              yOffset = values$yOffset)
          dev.off()
          rv$s3 <- s3() 
          
          values$highScore <- if (file.exists("highScore.rds")) readRDS("highScore.rds") else 0
          gameOver <<- FALSE
          
        }
        
      })
  
  swapAvailable <- 1
  observeEvent(input$swap, {
        if(swapAvailable) {
          swapAvailable <<- 0
          tmpShp <- nextPiece$shape
          nextPiece$shape <- values$shape
          values$shape <- tmpShp
          values$xOffset <- 4
          values$yOffset <- 20
        }
        
      })
  
  
  
  # ----------- score output ----------------- #
  
  observeEvent(values$numShapes, {
        output$lambda <- renderText(lambda)
        output$score <- renderText(score)
      })
  
  output$highScore <- renderText(values$highScore)
  
  output$pdfTitle <- renderText(paste0("Poisson PDF (&lambda; = ",
          values$shape$lambda[1], ")"))
  
  
}