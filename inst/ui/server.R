

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
  
  # was the button pressed this frame
  leftWasPressed <- FALSE
  rightWasPressed <- FALSE
  downWasPressed <- FALSE
  clockWasPressed <- FALSE
  counterWasPressed <- FALSE
  switchWasPressed <- FALSE
  
  # game data
  lambda <- 3.5
  score <- 0
  
  
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
  # Observe input keys #
  
  # left
  observeEvent(input$left, {    
        # keydown
        if(input$left == 50)
          leftWasPressed <<- TRUE
        
        # keyup
        if(input$left == 30)
          leftDuration <<- 0
      })
  
  # right
  observeEvent(input$right, { 
        # keydown
        if(input$right == 50)
          rightWasPressed <<- TRUE
        
        # keyup
        if(input$right == 30)
          rightDuration <<- 0
      })
  
  # down
  observeEvent(input$down,{ 
        # keydown
        if(input$down == 50)
          downWasPressed <<- TRUE
        
        # keyup
        if(input$down == 30) {
          downDuration <<- 0
          canPressDown <<- TRUE
        }
        
      })
  
  # clockwise rotation
  observeEvent(input$clock,{ 
        # keydown
        if(input$clock == 50)
          clockWasPressed <<- TRUE
      })
  
  # counter clockwise rotation
  observeEvent(input$counter,{ 
        # keydown
        if(input$counter == 50)
          counterWasPressed <<- TRUE
      })
  
  #########
  # plots #
  rv <- reactiveValues()
  ms <- 25
  tickTock30 <- reactiveTimer(ms)
  observeEvent(tickTock30(), {
        
        
        if(gameActive) {
          # left movement
          if(input$left == 50)
            leftDuration <<- leftDuration + 1
          
          if(leftWasPressed | 
              (leftDuration > holdDuration)) {  
            if(!detectCollision(values, xChange = -1))
              values$xOffset <- values$xOffset - 1
            leftWasPressed <<- FALSE
          }
          
          # right movement
          if(input$right == 50){
            rightDuration <<- rightDuration + 1
          }
          
          if(rightWasPressed | 
              (rightDuration > holdDuration)) {  
            if(!detectCollision(values, xChange = +1))
              values$xOffset <- values$xOffset + 1
            rightWasPressed <<- FALSE
          }
          
          
          # clockwise rotation
          if(clockWasPressed)
            if(!detectCollision(values, rotateClockwise = TRUE)) {
              values$shape <- rotateClockwise(values$shape)
              clockWasPressed <<- FALSE
            }
          
          # counter rotation
          if(counterWasPressed)
            if(!detectCollision(values, rotateCounter = TRUE)) {
              values$shape <- rotateCounter(values$shape) 
              counterWasPressed <<- FALSE
            }
          
          
          # down movement
          noDownCounter <<- noDownCounter + 1
          if(input$down == 50){
            downDuration <<- downDuration + 1
          }
          
          if(noDownCounter >= 450/ms | downWasPressed | 
              (canPressDown & downDuration > holdDuration)) {   #& downDuration %% holdDuration == 1
            
            if(detectCollision(values, yChange = -1)) { 
              
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
              
              # check for game end
              if(any(values$bg$y > 19)) {
                gameOver <<- TRUE
                gameActive <<- FALSE
              }
              
              if(input$down == 50)
                canPressDown <<- FALSE
              
            } else {
              # move the piece if no collision
              values$yOffset <- values$yOffset - 1
            }
            downWasPressed <<- FALSE
            noDownCounter <<- 0
          }
        }
        
        # plot code
        s <- svgstring(standalone=FALSE, height=600/72, width=297/72)
        par(mar = c(0, 0, 0, 0))
        blankPlot(xlim = c(-0.5, 10.5), 
            ylim = c(-0.5, 22.5))   # , asp = 1
        rect(xleft = 0, xright = 10,
            ybottom = 20, ytop = 22, 
            lwd = 4, border = NA, col = grey(0.95))
        for(i in 1:9)
          segments(x0 = i, y0 = 0, y1 = 22, 
              col = gray(0.7))
        segments(x0 = 0, x1 = 10, y0 = 0, lwd = 4)
        segments(x0 = 0, y0 = 0, y1 = 22, lwd = 4)
        segments(x0 = 10, y0 = 0, y1 = 22, lwd = 4)
        
        plotBg(values$bg)
        plotShape(values$shape, xOffset = values$xOffset,
            yOffset = values$yOffset)
        
        if(!gameActive) {
          rect(xleft = 1.5, xright = 8.5,
              ybottom = 10, ytop = 14, 
              lwd = 4, col = "white")
          text(x = 5, y = 13, "Press Start to Begin")
        }
        
        if(gameOver) {
          text(x = 5, y = 11, paste0("Score: ", score))
          if (score >= values$highScore) {
            saveRDS(score, "highScore.rds")
            values$highScore <- score
            text(x = 5, y = 12, "High Score!", col = "red")
          }
        }
        dev.off()
        rv$mainPlot <- s() 
      })
  output[["mainPlot"]] <- renderUI({ HTML(rv$mainPlot) })
  
  
  
  
  observe({
        s <- svgstring(standalone=FALSE, height=170/72, width=170/72)
        par(mar = c(0, 0, 0, 0))
        valVec <- c(nextPiece$shape$x, nextPiece$shape$y)
        valRange <- c(min(-3.5, -1*max(abs(valVec))-0.5),
            max(max(abs(valVec))+1.5, 4.5) )
        
        blankPlot(xlim = valRange, 
            ylim = valRange)   # , asp = 1
        
        rect(valRange[1], valRange[1],
            valRange[2], valRange[2], lwd = 4)
        plotShape(nextPiece$shape, xOffset = 0,
            yOffset = 0)
        dev.off()
        rv$nextPiece <- s() 
      })
  output[["nextPiece"]] <- renderUI({ HTML(rv$nextPiece) })
  
  observe({
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
  
  
  s <- svgstring(standalone=FALSE, height=170/72, width=170/72)
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
  rv$controls <- s() 
  output[["controls"]] <- renderUI({ HTML(rv$controls) })
  
  
  # ------------  game functions --------------- #
  
  observeEvent(input$startGame, {
        gameActive <<- TRUE
        
        if(gameOver) {
          
          lambda <<- 3.5
          score <<- 0
          
          values$yOffset <- 20
          values$shape <- makeShape(lambda)
          values$xOffset <- 5
          values$bg = makeBg() 
          values$numShapes = 0
          
          nextPiece$shape <- makeShape(lambda)
          
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