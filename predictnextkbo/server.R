source("PredictNextWord.R")

# http://stackoverflow.com/questions/35599470#35665217
load_data <- function() {
    Sys.sleep(2)
    hide("loading_page")
    show("main_content")
}

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
corpra_count <- length(uniPaths)

unigs <- vector("list", corpra_count)
bigrs <- vector("list", corpra_count)
trigs <- vector("list", corpra_count)

for(i in 1:corpra_count) {
    unigs[[i]] <- read.csv(uniPaths[i])
    bigrs[[i]] <- read.csv(bigPaths[i])
    trigs[[i]] <- read.csv(triPaths[i])
}

shinyServer(
    function(input, output) {
        
        # show user loading message http://stackoverflow.com/questions/35599470
        load_data()
        
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
                                                      unigs[[useCorpus()]],
                                                      bigrs[[useCorpus()]],
                                                      trigs[[useCorpus()]])})
        
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