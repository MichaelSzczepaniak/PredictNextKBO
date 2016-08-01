# source("PredictNextWord.R")
source('PredictNextWordDevOnly.R')  ## DELETE THIS LINE AND USE ONE ABOVE WHEN READY!!!!

shinyServer(
    function(input, output) {
        output$sPredictionSettings <-
            renderPrint({getSettings(input$corpusToUSe, input$bigDiscount,
                                     input$trigDiscount)})
        output$sInputBigram <- renderPrint({getInputBigram(input$phrase)})
        output$sAlgoPath <- renderPrint({getPrediction(input$phrase)[2]})
        output$sPredictedWord <- renderPrint({getPrediction(input$phrase)[1]})
        output$pTop3Probs <- renderPlot({
            getPlot()
        })
    }
)