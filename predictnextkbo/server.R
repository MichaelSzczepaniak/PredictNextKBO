source("PredictNextWord.R")

shinyServer(
    function(input, output) {
        
        inBigram <- reactive({
            getInputBigram(input$phrase)
        })
        
        topPreds <- reactive({
            getTopNPredictions(inBigram())
        })
        
        output$sPredictionSettings <-
            renderPrint({getSettings(input$corpusToUSe, input$bigDiscount,
                                     input$trigDiscount)})
        
        output$sInputBigram <- renderPrint(inBigram())
        
        output$sPredictedWord <- renderPrint({getPrediction(topPreds())})
        
        output$pTop3Probs <- renderPlot({
            getPlot(topPreds()$ngram, topPreds()$prob)
        })
    }
)