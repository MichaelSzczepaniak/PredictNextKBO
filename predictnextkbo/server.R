source("PredictNextWord.R")

shinyServer(
    function(input, output) {
        # get bigram and discount rates once and use them in multiple
        # reactive functions to build multiple objects/observers
        inBigram <- eventReactive(input$predictButton,
                                  {getInputBigram(input$phrase)})
        bigDisc <- eventReactive(input$predictButton,
                            {as.numeric(input$bigDiscount)})
        trigDisc <- eventReactive(input$predictButton,
                                  {as.numeric(input$trigDiscount)})
        
        topPreds <- eventReactive(input$predictButton,
            {getTopNPredictions(inBigram(), 3, bigDisc(), trigDisc())}
        )
        
        output$sPredictionSettings <-
            renderPrint({getSettings(input$corpusToUSe, bigDisc(), trigDisc())})
        
        output$sInputBigram <- renderPrint(inBigram())
        
        output$sPredictedWord <- renderPrint({getPrediction(topPreds())})
        
        output$pTop3Probs <- renderPlot({
            getPlot(topPreds()$ngram, topPreds()$prob)
            # getPlot()
        })
        
        # output$pTop3Probs <- renderPrint({topPreds()})
        
    }
)