
rm(list = ls())
setwd('D:/dev/PredictNextKBO/predictnextkbo')
source('../predictnextkbo/PredictNextWord.R')


timePredict <- function(phrase="i_love",
                        functions=c()) {
    predict <- getTopNPredictions(phrase, 3, 1, 0.5, 0.5, FALSE)
    cat(sprintf("%s%s%s", "Prediction: ", predict$ngram[1], "\n"))
}