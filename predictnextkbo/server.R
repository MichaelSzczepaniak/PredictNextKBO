source("PredictNextWord.R")

# load the n-gram tables once because this is expensive
uniPaths <- c("./data/en_US.blogs.train.12unigrams.nosins.csv",
              "./data/en_US.news.train.12unigrams.nosins.csv",
              "./data/en_US.twitter.train.12unigrams.nosins.csv")
bigPaths <- c("./data/en_US.blogs.train.13bigrams.nosins.csv",
              "./data/en_US.news.train.13bigrams.nosins.csv",
              "./data/en_US.twitter.train.13bigrams.nosins.csv")
triPaths <- c("./data/en_US.blogs.train.14trigrams.nosins.csv",
              "./data/en_US.news.train.14trigrams.nosins.csv",
              "./data/en_US.twitter.train.14trigrams.nosins.csv")

# Why getting: Error: invalid subscript type 'closure'  ???
unigs <- list()
bigrs <- list()
trigs <- list()
for(i in 1:length(uniPaths)) {
    unigs[[i]] <- read.csv(uniPaths[i])
    bigrs[[i]] <- read.csv(bigPaths[i])
    trigs[[i]] <- read.csv(triPaths[i])
}

shinyServer(
    function(input, output) {
        
        # get bigram and discount rates once and use them in multiple
        # reactive functions to build multiple objects/observers
        useCorpus <- eventReactive(input$predictButton,
                                   {as.integer(input$corpusToUse)})
        inBigram <- eventReactive(input$predictButton,
                                  {getInputBigram(input$phrase)})
        bigDisc <- eventReactive(input$predictButton,
                            {as.numeric(input$bigDiscount)})
        trigDisc <- eventReactive(input$predictButton,
                                  {as.numeric(input$trigDiscount)})
        probBars <- eventReactive(input$predictButton,
                                  {as.integer(input$histBars)})
        allowEOSPredictions <- eventReactive(input$predictButton,
                                             {as.logical(input$eosCheckbox)})
        
        # fire prediction algorithm when "Predict..." button clicked
        topPreds <- eventReactive(input$predictButton,
                                  {getTopNPredictions(inBigram(), probBars(), 
                                                      useCorpus(),
                                                      bigDisc(), trigDisc(),
                                                      allowEOSPredictions(),
                                                      unigs[[useCorpus]],
                                                      bigrs[[useCorpus]],
                                                      trigs[[useCorpus]])})
        
        output$sPredictionSettings <-
            renderPrint({getSettings(useCorpus(), bigDisc(), trigDisc())})
        
        output$sInputBigram <- renderPrint(inBigram())
        
        output$sPredictedWord <- renderPrint({getPrediction(topPreds())})
        
        output$sPredictedFrom <- renderPrint({getPredictFrom(topPreds())})
        
        output$pTopNProbs <- renderPlot({
            getPlot(topPreds()$ngram, topPreds()$prob)
        })
        
    }
)