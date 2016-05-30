rm(list = ls())
source("Katz.R")

predictNextWord <- function() {
    gamma2=0.7; gamma3=0.7
    obsTrigrams <- calc.qBO.trigramsA(gamma3, bigramPrefix=bigPre, trigrams=trigs)
    alphaBig <- getAlphaBigram(gamma2, bigrams=bigrs, unigram=unig)
    qboBigs <- calc.qBO.bigramsB(bigDiscount=gamma2, bigramPrefix=bigPre,
                                 trigrams=trigs, bigrams=bigrs, unigrams=unigs)
    alphaTrig <- getAlphaTrigram(gamma3, trigs, bigr)
    qBO.trigs.B <- calc.qBO.trigramB(gamma3, bigPre, trigs, bigrs, unigs, alphaTrig)
    all_trigrams <- rbind(obsTrigrams, qBO.trigs.B)
    all_trigrams
    predict_trigram <- all_trigrams[which.max(all_trigrams$prob),]
    predict_trigram
}


