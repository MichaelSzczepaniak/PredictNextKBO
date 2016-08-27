# COMMENT THE NEXT 4 LINES BEFORE DEPLOYMENT BECAUSE SHINY IMPORTS WHAT IT NEEDS
libs <- c("dplyr", "readr", "stringr", "quanteda", "data.table")
new.packages <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
lapply(libs, require, character.only=TRUE)
options(stringsAsFactors = FALSE)  # strings are what we are operating on...

## Returns a named vector of n-grams and their associated frequencies
## extracted from the character vector dat.
##
## ng - Defines the type of n-gram to be extracted: unigram if ng=1,
##      bigram if ng=2, trigram if n=3, etc.
## dat - Character vector from which we want to get n-gram counts.
## igfs - Character vector of words (features) to ignore from frequency table
## sort.by.ngram - sorts the return vector by the names
## sort.by.freq - sorts the return vector by frequency/count
getNgramFreqs <- function(ng, dat, igfs=NULL,
                          sort.by.ngram=TRUE, sort.by.freq=FALSE) {
    # http://stackoverflow.com/questions/36629329/
    # how-do-i-keep-intra-word-periods-in-unigrams-r-quanteda
    if(is.null(igfs)) {
        dat.dfm <- dfm(dat, ngrams=ng, toLower = FALSE, removePunct = FALSE,
                       what = "fasterword", verbose = FALSE)
    } else {
        dat.dfm <- dfm(dat, ngrams=ng, toLower = FALSE, ignoredFeatures=igfs,
                       removePunct = FALSE, what = "fasterword", verbose = FALSE)
    }
    rm(dat)
    # quanteda docfreq will get the document frequency of terms in the dfm
    ngram.freq <- docfreq(dat.dfm)
    if(sort.by.freq) { ngram.freq <- sort(ngram.freq, decreasing=TRUE) }
    if(sort.by.ngram) { ngram.freq <- ngram.freq[sort(names(ngram.freq))] }
    rm(dat.dfm)
    
    return(ngram.freq)
}

## Returns a 2 column data.table. The first column: ngram, contains all the
## unigrams, bigrams, or trigrams in the corpus depending on whether
## ng = 1, 2, or 3 respectively. The second column: freq, contains the
## frequency or count of the ngram found in linesCorpus.
##
## ng - Defines the type of n-gram to be extracted: unigram if ng=1,
##      bigram if ng=2, trigram if n=3, etc.
## linesCorpus - character vector which is a line from a corpus file
## prefixFilter - character vector: If not NULL, tells the function to return
##                only rows where the ngram column starts with prefixFilter.
##                If NULL, returns all the ngram and count rows.
getNgramTables <- function(ng, linesCorpus, prefixFilter=NULL) {
    ngrams <- getNgramFreqs(ng, linesCorpus)
    ngrams.dt <- data.table(ngram=names(ngrams), freq=ngrams)
    if(length(grep('^SOS', ngrams.dt$ngram)) > 0) {
        ngrams.dt <- ngrams.dt[-grep('^SOS', ngrams.dt$ngram),]
    }
    if(!is.null(prefixFilter)) {
        regex <- sprintf('%s%s', '^', prefixFilter)
        ngrams.dt <- ngrams.dt[grep(regex, ngrams.dt$ngram),]
    }
    
    return(ngrams.dt)
}

## Returns a two column data.table of observed trigrams that start with
## bigramPrefix in the first column named ngram and frequencies/counts in the 
## second column named freq. If no observed trigrams with bigramPrefix exist,
## an empty data.table is returned.
##
## bigramPrefix - single element character vector which is a bigram of the
##                form: word1_word2 e.g. sell_the
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
getObsTrigs <- function(bigramPrefix, trigrams) {
    regex <- sprintf("%s%s", "^", bigramPrefix)
    trigs.winA <- trigrams[grep(regex, trigrams$ngram),]
    return(trigs.winA)
}

## Returns the probability estimate for observed trigrams with bigramPrefix
## calculated from equation 12.
##
## The first column of the datatable are the trigrams corresponding to the
## probability estimate that are in the second column.  If no observed trigrams
## exist, returns NULL.
##
## bigramPrefix - single-element char array of the form word1_word2
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
## triDiscount - amount to discount observed trigrams
calc.qBO.trigramsA <- function(bigramPrefix, trigrams, triDiscount=0.5) {
    obsTrigsA <- getObsTrigs(bigramPrefix, trigrams)
    if(nrow(obsTrigsA) < 1) return(NULL)
    obsCount <- sum(obsTrigsA$freq)
    probs <- (obsTrigsA$freq - triDiscount) / obsCount
    qBO.A <- data.table(ngram=obsTrigsA$ngram, prob=probs)
    return(qBO.A)
}

## Returns a character vector that are the OBSERVED trigram tail words (OTTW)
## that start with bigramPrefix: wi in A(w_i-2, w_i-1) as shown in
## eqn. 9
##
## Precondition: bigramPrefix is of the format wi-2_wi-1 where w1-2 is the
## 1st word of an observered trigram and wi-1 is the 2nd/middle word of the
## trigram.
##
## If no trigrams start with bigramPrefix an empty character vector is returned.
##
## bigramPrefix - single-element char array of the form word1_word2
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
getOTTWinA <- function(bigramPrefix, trigrams) {
    regex <- sprintf("%s%s", "^", bigramPrefix)
    trigs.winA <- trigrams[grep(regex, trigrams$ngram), ]
    patToReplace <- sprintf("%s%s", bigramPrefix, "_")
    wInA <- str_replace(trigs.winA$ngram, patToReplace, "")
    return(wInA)
}

## Returns a character vector containing the UNOBSERVED trigram tail words
## (UTTW) that start with bigramPrefix: wi in B(w_i-2, w_i-1) as shown in
## eqn. 17
##
## Precondition: bigramPrefix is of the format wi-2_wi-1 where w1-2 is the
## 1st word of the trigram and wi-1 is the 2nd/middle word of the trigram.
##
## bigramPrefix - single-element char array of the form word1_word2
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
## unigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the unigrams in the corpus. The second column:
##            freq, contains the frequency or count of each unigram.
getUTTWinB <- function(bigramPrefix, trigrams, unigrams) {
    allUnigrams <- unigrams$ngram
    wInA <- getOTTWinA(bigramPrefix, trigrams)
    if(length(wInA) < 1) {
        wInB <- allUnigrams
    } else {
        wInB <- setdiff(allUnigrams, wInA)
    }
    
    return(wInB)
}

## Returns a vector of unobserved trigrams that start with bigramPrefix
##
## bigramPrefix - first two words of unobserved trigrams we want to estimate
##                probabilities of
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
## unigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the unigrams in the corpus. The second column:
##            freq, contains the frequency or count of each unigram.
getUnobsTrigs <- function(bigramPrefix, trigrams, unigrams) {
    unobsTriTails <- getUTTWinB(bigramPrefix, trigrams, unigrams)
    unobsTrigs <- vector(mode="character", length=length(unobsTriTails))
    for(i in 1:length(unobsTriTails)) {
        unobsTrigs[i] <- sprintf('%s%s%s', bigramPrefix, '_', unobsTriTails[i])
    }
    return(unobsTrigs)
}

## Returns the total probability mass discounted from all observed bigrams
## calculated from equation 14.  This is the amount of probability mass which
## is redistributed to UNOBSERVED bigrams. If no bigrams starting with
## unigram$ngram[1] exist, NULL is returned.
##
## unigram - 2 column, single row frequency table. The first column: ngram,
##           contains the w_i-1 unigram. The second column: freq, contains the
##           frequency or count of this unigram.
## bigrams - 2 column data.frame or data.table. The first column: ngram,
##           contains all the bigrams in the corpus. The second column:
##           freq, contains the frequency or count of each bigram.
## bigDiscount - amount to discount observed bigrams
getAlphaBigram <- function(unigram, bigrams, bigDiscount=0.5) {
    # get all bigrams that start with unigram
    regex <- sprintf("%s%s", "^", unigram$ngram[1])
    bigsThatStartWithUnig <- bigrams[grep(regex, bigrams$ngram),]
    if(nrow(bigsThatStartWithUnig) < 1) return(NULL)
    alphaBi <- 1 - (sum(bigsThatStartWithUnig$freq - bigDiscount) /
                    unigram$freq)
    return(alphaBi)
}


## Returns a 3 column data.table. First column (ngram) = bigrams that are the
## last two words of unobserved trigrams that start with bigramPrefix.
## Second column (btfreq) = frequency/count of the bigram tail of unobserved
##                          trigram
## Third column (utfreq) = frequency/count of the unigram tail word of the
##                         bigram defined in the 2nd column (btfreq)
##
## bigramPrefix - single-element char array of the form word1_word2
## unobsTrigs - character vector of unobserved trigrams of the form:
##              word1_word2_word3, e.g. sell_the_buy
## bigrams - 2 column data.frame or data.table. The first column: ngram,
##           contains all the bigrams in the corpus. The second column:
##           freq, contains the frequency or count of each bigram.
## unigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the unigrams in the corpus. The second column:
##            freq, contains the frequency or count of each unigram.
getUnobsBigramsTable <- function(bigramPrefix, unobsTrigs, bigrams, unigrams) {
    cat("getUnobsBigramsTable: START building table at",
        as.character(Sys.time()),"...\n")
    bigramTails <- vector(mode='character', length = length(unobsTrigs))
    bigramTailCounts <- rep(-1, length(unobsTrigs))
    unigramTailCounts <- rep(-1, length(unobsTrigs))
    for(i in 1:length(unobsTrigs)) {
        bigramTail <- str_split(unobsTrigs[i], '_')[[1]]
        unigramTail <- bigramTail[3]
        bigramTail <- sprintf('%s%s%s', bigramTail[2], '_', bigramTail[3])
        bigramTails[i] <- bigramTail
        bigramIndex <- which(bigrams$ngram == bigramTail)
        if(length(bigramIndex) > 0) {
            bigramTailCounts[i] <- bigrams$freq[bigramIndex]
        } else {
            bigramTailCounts[i] <- 0
        }
        unigramTailIndex <- which(unigrams$ngram == unigramTail)
        unigramTailCounts[i] <- unigrams$freq[unigramTailIndex]
    }
    dt <- data.table(ngram=bigramTails, btfreq=bigramTailCounts,
                     utfreq=unigramTailCounts)
    cat("getUnobsBigramsTable: FINISH building table at",
        as.character(Sys.time()),"...\n")
    
    return(dt)
}

## TODO THIS IS AN EXTEMELY COMPUTATIONALLY EXPENSIVE CALCULATION WHEN DONE
## ON a LARGE CORPUS. NEED TO REWRITE THIS SO THAT IT'S MORE EFFICIENT!!!
################################################################################
## Returns a data.table with the first column (ngram) containing the bigram
## tails of unobserved trigrams that start with bigramPrefix. The second column
## (probs) holds the conditional probability estimate for the last word of the
## bigram tail given the last word of the bigramPrefix (middle word of the 
## unobserved trigram).
##
## bigDiscount - amount to discount observed bigrams
## bigramPrefix - first two words of unobserved trigrams we want to estimate
##                probabilities of
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
## bigrams - 2 column data.frame or data.table. The first column: ngram,
##           contains all the bigrams in the corpus. The second column:
##           freq, contains the frequency or count of each bigram.
## unigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the unigrams in the corpus. The second column:
##            freq, contains the frequency or count of each unigram.
calc.qBO.bigramsB <- function(bigDiscount=0.5, bigramPrefix,
                              trigrams, bigrams, unigrams) {
    unobsTrigs <- getUnobsTrigs(bigramPrefix, trigrams=trigrams,
                                unigrams=unigrams)
    unobBis <- getUnobsBigramsTable(bigramPrefix, unobsTrigs, bigrams, unigrams)
    unobBiProbs <- rep(-1, length(unobBis$ngram))
    # calc discounted prob. mass at bigram level
    unig <- str_split(bigramPrefix, '_')[[1]][2]
    unigram <- filter(unigrams, ngram == unig)
    alphaBig <- getAlphaBigram(bigDiscount, bigrams, unigram)
    uniSumUnobs <- sum(filter(unobBis, btfreq == 0)$utfreq)
    for(i in 1:length(unobBis$ngram)) {
        if(unobBis$btfreq[i] > 0) {
            # bigram tail observed: calc qBO from eqn. 10.
            unobBiProbs[i] <- (unobBis$btfreq[i]-bigDiscount)/unigram$freq[1]
        } else {
            # bigram tail NOT observed: calc qBO w/ bigram from eqn. 16.
            unobBiProbs[i] <- alphaBig * unobBis$utfreq[i] / uniSumUnobs
        }
    }
    dt <- data.table(ngram=unobBis$ngram, prob=unobBiProbs)
    
    return(dt)
}

## Returns the total probability mass discounted from all observed trigrams.
## This is the amount of probability mass which is redistributed to
## UNOBSERVED trigrams. If no trigrams starting with bigram$ngram[1] exist,
## NULL is returned.
##
## triDiscount - amount to discount observed trigrams
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
## bigram - single row frequency table where the first col: ngram, is the bigram
##          which are the first two words of unobserved trigrams we want to
##          estimate probabilities of (same as bigramPrefix in previous functions
##          listed above) delimited with '_'. The second column: freq, is the
##          frequency/count of the bigram listed in the 1st column.
getAlphaTrigram <- function(triDiscount=0.5, trigrams, bigram) {
    # get all trigrams that start with bigram
    regex <- sprintf("%s%s", "^", bigram$ngram[1])
    trigsThatStartWithBig <- trigrams[grep(regex, trigrams$ngram),]
    if(nrow(trigsThatStartWithBig) < 1) return(NULL)
    alphaTri <- 1 - (sum(trigsThatStartWithBig$freq - triDiscount) / bigram$freq)
    
    return(alphaTri)
}

## Returns a two column data table with column names ngram and prob.  The first
## column are trigrams formatted as w1_w2_w3 meaning that the values in the
## prob column are probability estimates of q_BO(w3 | w1, w2) for unobserved
## trigrams that start with bigramPrefix (w1, w2) calculated from equation 17.
##
## bigDiscount - amount to discount observed bigrams
## bigramPrefix - first two words of unobserved trigrams we want to estimate
##                probabilities of
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
## bigrams - 2 column data.frame or data.table. The first column: ngram,
##           contains all the bigrams in the corpus. The second column:
##           freq, contains the frequency or count of each bigram.
## unigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the unigrams in the corpus. The second column:
##            freq, contains the frequency or count of each unigram.
## alphaTrigr - total amount of discounted probability mass at the trigram level
calc.qBO.trigramB <- function(bigDiscount=0.5, bigramPrefix, trigrams, bigrams,
                              unigrams, alphaTrigr) {
    qBoUnobsBigs <- calc.qBO.bigramsB(bigDiscount=0.5, bigramPrefix=bigramPrefix,
                                      trigrams=trigrams, bigrams=bigrams,
                                      unigrams=unigrams)
    sum.qBoUnobsBigs <- sum(qBoUnobsBigs$prob)
    qBoUnobsTrigProbs <- alphaTrigr * qBoUnobsBigs$prob / sum.qBoUnobsBigs
    qBoUnobsTrigrams <- paste0(str_split(bigramPrefix, '_')[[1]][1],
                               '_', qBoUnobsBigs$ngram)
    dt <- data.table(ngram=qBoUnobsTrigrams, prob=qBoUnobsTrigProbs)
    
    return(dt)
}
