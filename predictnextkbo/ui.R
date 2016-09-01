# library(shiny)

modelChoices <- list("Blogs"=1, "News"=2, "Twitter"=3)
probBarChoices <- list("1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                       "6"=6, "7"=7, "8"=8, "9"=9, "10"=10)

fluidPage(
    titlePanel(title="Predict Next Word"),
    
    sidebarPanel(
        # Let user know app is computing:
        # stackoverflow.com/questions/17325521#22475216
        tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
        
        textInput('phrase', h4("Enter phrase:")),
        
        radioButtons('corpusToUse', h4("Corpus"), modelChoices,
                     selected=1, inline=TRUE),
        
        sliderInput("bigDiscount",
                    label = h4("Bigram Discount"),
                    min = 0.1, max = 1.9, value = 0.5, step = 0.1),
        
        sliderInput("trigDiscount",
                    label = h4("Trigram Discount"),
                    min = 0.1, max = 1.9, value = 0.5, step = 0.1),
        
        selectInput("histBars", label=h4("Number of Probability Bars"),
                    probBarChoices, selected=3, width="250px"),
        
        actionButton("predictButton", "Predict next word of phrase"),
        
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div("Computing prediction...",id="loadmessage"))
        
    ),
    mainPanel(
        h4('Prediction Settings:'),
        verbatimTextOutput("sPredictionSettings"),
        h4('Last two words of input phrase:'),
        verbatimTextOutput("sInputBigram"),
        # h4('Algorithm Path:'),
        # verbatimTextOutput("sAlgoPath"),
        h4('Predicted Word:'),
        verbatimTextOutput("sPredictedWord"),
        plotOutput("pTop3Probs", width = 500, height = 350)
        # verbatimTextOutput("pTop3Probs")
    )
)