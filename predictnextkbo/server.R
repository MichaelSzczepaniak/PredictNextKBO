source("PredictNextWord.R")

shinyServer(
    function(input, output) {
        output$sPredictedWord <- renderPrint({getPrediction(input$phrase)[1]})
        output$sAlgoPath <- renderPrint({getPrediction(input$phrase)[2]})
        output$sInputBigram <- renderPrint({getInputBigram(input$phrase)})
        output$sPredModel <- renderPrint({input$predModel})
        output$sDiscount <- renderPrint({input$discount})
    }
)