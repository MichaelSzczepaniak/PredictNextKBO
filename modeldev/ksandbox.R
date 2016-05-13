source("Katz.R")

trigs <- getNgramTables(3)  # for little test corpus
# getTrigramWinA('fry_the', trigs) # empty char vector
wInA <- getOTTWinA('sell_the', trigs)
wInB <- getUTTWinB('sell_the', trigs)
big <- getNgramTables(2, prefixFilter = 'sell_the')
alphaTrig <- getAlphaTrigram(0.5, trigs, big)
bigs <- getNgramTables(2)
unig <- getNgramTables(1, prefixFilter = 'the')
alphaBig <- getAlphaBigram(0.5, bigs, unig)  # 1/8 = 0.125
