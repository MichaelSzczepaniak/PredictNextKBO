rm(list = ls())
source("Katz.R")

trigs <- getNgramTables(3)  # for little test corpus
unigrams <- getNgramTables(1)
# getTrigramWinA('fry_the', trigs) # empty char vector
wInA <- getOTTWinA('sell_the', trigs)
wInB <- getUTTWinB('sell_the', trigs)
big <- getNgramTables(2, prefixFilter = 'sell_the')
alphaTrig <- getAlphaTrigram(0.5, trigs, big)

unigram <- getNgramTables(1, prefixFilter = 'the')
alphaBig <- getAlphaBigram(0.5, bigs, unig)  # 1/8 = 0.125

bigDiscount=0.5; bigramPrefix='sell_the'
trigs=getNgramTables(3)
unobsTrigs=getUnobsTrigs('sell_the', trigs)
bigs=getNgramTables(2)
unigs=getNgramTables(1)
unig=getNgramTables(1, prefixFilter = 'the')

x <- calc.qBO.bigramsB(bigDiscount=bigDiscount, bigramPrefix=bigramPrefix,
                       trigrams=trigs, bigrams=bigs, unigrams=unigs,
                       unigram=unig)
