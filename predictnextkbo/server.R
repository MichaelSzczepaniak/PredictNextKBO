source("PredictNextWord.R")

shinyServer(
    function(input, output) {
        
        inBigram <- reactive({
            getInputBigram(input$phrase)
        })
        
        output$sPredictionSettings <-
            renderPrint({getSettings(input$corpusToUSe, input$bigDiscount,
                                     input$trigDiscount)})
        
        output$sInputBigram <- renderPrint(inBigram())
        
        output$sPredictedWord <- renderPrint({getPrediction(inBigram())})
        
        output$pTop3Probs <- renderPlot({
            getPlot()
        })
    }
)