fluidPage(
    
    includeCSS(file.path("css", "styles.css")),
    
    tags$head(tags$title("Poissontris")), # window header
    
    h1("poissontris - a shiny app", span("(poisson-sampled block sizes)"),
        img(src = "logo_text.png", style = "float:right; width:240px;")        
    ),
    
    
    # keyboard inputs
    tags$script('
            var leftCounter = 0;
            var downCounter = 0;
            var rightCounter = 0;
            var clockCounter = 0;
            var counterCounter = 0;
            $(document).on("keydown", function (e) {
            switch (e.which) {
            case 40:
            Shiny.onInputChange("down", 50);
            Shiny.onInputChange("downCounter", ++downCounter);
            break;
            case 39:
            Shiny.onInputChange("right", 50);
            Shiny.onInputChange("rightCounter", ++rightCounter);
            break;
            case 37:
            Shiny.onInputChange("left", 50);
            Shiny.onInputChange("leftCounter", ++leftCounter);
            break;
            case 83:
            Shiny.onInputChange("counter", ++counterCounter);
            break;
            case 68:
            Shiny.onInputChange("clock", ++clockCounter);
            break;
            case 38:
            Shiny.onInputChange("clock", ++clockCounter);
            break;
            case 70:
            Shiny.onInputChange("swap", Math.random(1));
            break;
            case 13:
            Shiny.onInputChange("startGame", Math.random(1));
            break;
            }});
            '), 
    tags$script('
            $(document).on("keyup", function (e) {
            switch (e.which) {
            case 40:
            Shiny.onInputChange("down", 30);
            break;
            case 39:
            Shiny.onInputChange("right", 30);
            break;
            case 37:
            Shiny.onInputChange("left", 30);
            break;
            }});
            '), 
    
    # TODO column around this
    fluidRow(
        column(3, offset = 2,
            fluidRow(
                column(12, 
                    uiOutput("s1", style="max-height:80%;"), style="position:absolute;"),
                column(12, 
                    uiOutput("s2", style="max-height:80%;"), style="position:absolute;"),
                column(12, 
                    uiOutput("s3", style="max-height:80%;"), style="position:absolute;"), 
                column(12, 
                    uiOutput("s4", style="max-height:80%;"), style="position:absolute;")     
            )), 
        
        column(2,  
            
            h3(HTML("&lambda;:"), textOutput(outputId = "lambda", inline = TRUE)),
            h3(textOutput(outputId = "leftCounter")),
            h3("Score: ", textOutput(outputId = "score", inline = TRUE)),
            h3(icon("trophy", class = "fa-2x"), textOutput("highScore", inline = TRUE)),
            h4("Next Piece: ", style = "margin-top: 50px;"), 
            uiOutput("nextPiece", style="width: 170px; height: 170px;"),
            h4(icon("gamepad", class = "fa-2x"), "Controls: "),
            uiOutput("controls", style="width: 170px; height: 170px;"),   
            
            
            conditionalPanel("input.showButtons % 2 == 1", 
                
                actionButton(inputId = "left", label = ""),
                actionButton(inputId = "leftCounter", label = ""),
                actionButton(inputId = "down", label = ""),
                actionButton(inputId = "downCounter", label = ""),
                actionButton(inputId = "right", label = ""), 
                actionButton(inputId = "rightCounter", label = ""), 
                actionButton(inputId = "counter", label = ""), 
                actionButton(inputId = "clock", label = ""), 
                actionButton(inputId = "clicks", label = ""),
                actionButton(inputId = "swap", label = ""), 
                actionButton(inputId = "startGame", label = "")
            
            )
        ), 
        column(4, 
            htmlOutput(outputId = "pdfTitle", container = h4),
            uiOutput("pdf", style="height: 300px; width: 450px;"), 
            actionButton(inputId = "showPc", label = "Show PC Controls"),
            conditionalPanel("input.showPc % 2 == 1", 
                
                h3(icon("arrow-left", class = "fa-2x"), icon("arrow-down", class = "fa-2x"),
                    icon("arrow-right", class = "fa-2x"),
                    ": move left, down, right"),
                h3(icon("arrow-up", class = "fa-2x"), ": rotate clockwise"),
                h3("s : rotate counterclockwise"),
                h3("f : swap pieces")
            
            )
        
        )
    
    )



)

