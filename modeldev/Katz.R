# install packages if needed, load libraries
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
getNgramFreqs <- function(ng, dat, igfs=NULL, sent.parse=FALSE,
                          sort.by.ngram=TRUE, sort.by.freq=FALSE) {
    if(sent.parse) { dat <- breakOutSentences(dat) }
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

## Returns a 2 column data.table. The first column (ngram) contains the
## unigram (if n=1), the bigram (if n=2), or trigram (if n=3). The second column
## (freq) contains the frequency or count of the ngram found in inputFile if
## isFreqTable=FALSE.
##
## If isFreqTable is TRUE, input file is assumed to be a 2 column csv file with
## first column header = "ngram" and second column header = "freq". Data in 
## these 2 columns are assumed to be as is described above for returned table.
##
## prefixFilter - If not NULL, tells the function to return only rows where
##                ngram column starts with prefixFilter.
getNgramTables <- function(n, inputFile="../data/little_test_corpus1.txt",
                           isFreqTable=FALSE, prefixFilter=NULL) {
    ngrams.dt <- NULL
    if(isFreqTable) {
        ngrams.dt <- read.csv(inputFile, stringsAsFactors=FALSE)
    } else {
        lines <- read_lines(inputFile)
        ngrams <- getNgramFreqs(n, lines)
        ngrams.dt <- data.table(ngram=names(ngrams), freq=ngrams)
    }
    if(length(grep('^SOS', ngrams.dt$ngram)) > 0) {
        ngrams.dt <- ngrams.dt[-grep('^SOS', ngrams.dt$ngram),]
    }
    if(!is.null(prefixFilter)) {
        regex <- sprintf('%s%s', '^', prefixFilter)
        ngrams.dt <- ngrams.dt[grep(regex, ngrams.dt$ngram),]
    }
    
    return(ngrams.dt)
}



## Returns the total probability mass discounted from all observed TRIGRAMS.
## This is the amount of probability mass which is redistributed to
## UNOBSERVED trigrams. If no trigrams starting with bigram$ngram[1] exist,
## NULL is returned.
## trigrams - data.frame or data.table of trigrams (1st column) and
##            frequencies (2nd column)
## bigram - single row frequency table with bigram in first col and frequency
##          in the second.
getAlphaTrigram <- function(discount=0.5, trigrams, bigram) {
    # get all trigrams that start with bigram
    regex <- sprintf("%s%s", "^", bigram$ngram[1])
    trigsThatStartWithBig <- trigrams[grep(regex, trigrams$ngram),]
    if(nrow(trigsThatStartWithBig) < 1) return(NULL)
    alphaTri <- 1 - (sum(trigsThatStartWithBig$freq - discount) / bigram$freq)
    return(alphaTri)
}

## Returns the total probability mass discounted from all observed BIGRAMS.
## This is the amount of probability mass which is redistributed to
## UNOBSERVED bigrams. If no bigrams starting with unigram$ngram[1] exist,
## NULL is returned.
## bigrams - data.frame or data.table of bigrams (1st column) and
##           frequencies (2nd column)
## unigram - single row frequency table with unigram in first col and frequency
##           in the second.
getAlphaBigram <- function(discount=0.5, bigrams, unigram) {
    # get all bigrams that start with unigram
    regex <- sprintf("%s%s", "^", unigram$ngram[1])
    bigsThatStartWithUnig <- bigrams[grep(regex, bigrams$ngram),]
    if(nrow(bigsThatStartWithUnig) < 1) return(NULL)
    alphaBi <- 1 - (sum(bigsThatStartWithUnig$freq - discount) / unigram$freq)
    return(alphaBi)
}

## Returns a two column data.table of observed trigrams that start with
## bigramPrefix in the first column named ngram and frequencies/counts in the 
## second column named freq. If no observed trigrams with bigramPrefix exist,
## and empty data.table is returned.
getObsTrigs <- function(bigramPrefix, trigrams) {
    regex <- sprintf("%s%s", "^", bigramPrefix)
    trigs.winA <- trigrams[grep(regex, trigrams$ngram)]
    return(trigs.winA)
}

## Returns the probability estimate for observed trigrams with bigramPrefix
## calculated from equation 11.
## Vector element names are the trigrams corresponding to the probability est.
## If no such trigrams exist, returns NULL.
calc.qBO.trigramA <- function(discount=0.5, bigramPrefix, trigrams) {
    obsTrigsA <- getObsTrigs(bigramPrefix, trigrams)
    if(nrow(obsTrigsA) < 1) return(NULL)
    obsCount <- sum(obsTrigsA$freq)
    qBO.A <- (obsTrigsA$freq - discount) / obsCount
    names(qBO.A) <- obsTrigsA$ngram
    return(qBO.A)
}

## Returns a 3 column data.table. First column (ngram) = bigrams that are the
## last two words of unobserved trigrams that start with bigramPrefix.
## Second column (btfreq) = frequency/count of the bigrams, -1 if unobserved
## Third column (utfreq) = frequency/count of the unigram tail word in bigram
## bigrams - all the bigrams in the corpus
## unigrams - all the unigrams in the corpus
getUnobsBigramsTable <- function(bigramPrefix, unobsTrigs, bigrams, unigrams) {
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
            bigramTailCounts[i] <- -1
        }
        unigramTailIndex <- which(unigrams$ngram == unigramTail)
        unigramTailCounts[i] <- unigrams$freq[unigramTailIndex]
    }
    dt <- data.table(ngram=bigramTails, btfreq=bigramTailCounts,
                     utfreq=unigramTailCounts)
    return(dt)
}

## Returns the OBSERVED trigram tail words (OTTW) that start with bigramPrefix.
## Precondition: bigramPrefix is of the format wi-2_wi-1 where w1-2 is the
## 1st word of the trigram and wi-1 is the 2nd/middle word of the trigram.
##
## If no trigrams start with bigramPrefix an empty character vector is returned.
getOTTWinA <- function(bigramPrefix, trigrams) {
    regex <- sprintf("%s%s", "^", bigramPrefix)
    trigs.winA <- trigrams[grep(regex, trigrams$ngram)]
    patToReplace <- sprintf("%s%s", bigramPrefix, "_")
    wInA <- str_replace(trigs.winA$ngram, patToReplace, "")
    return(wInA)
}

## Returns the UNOBSERVED trigram tail words (UTTW) that start with bigramPrefix.
## Precondition: bigramPrefix is of the format wi-2_wi-1 where w1-2 is the
## 1st word of the trigram and wi-1 is the 2nd/middle word of the trigram.
getUTTWinB <- function(bigramPrefix, trigrams) {
    allUnigrams <- getNgramTables(1)$ngram
    wInA <- getOTTWinA(bigramPrefix, trigrams)
    if(length(wInA) < 1) {
        wInB <- allunigrams
    } else {
        wInB <- setdiff(allUnigrams, wInA)
    }
    return(wInB)
}

## Returns a vector of unobserved trigrams that start with bigramPrefix
getUnobsTrigs <- function(bigramPrefix, trigrams) {
    unobsTriTails <- getUTTWinB(bigramPrefix, trigrams)
    unobsTrigs <- vector(mode="character", length=length(unobsTriTails))
    for(i in 1:length(unobsTriTails)) {
        unobsTrigs[i] <- sprintf('%s%s%s', bigramPrefix, '_', unobsTriTails[i])
    }
    return(unobsTrigs)
}

## Returns a data.table with the first column (ngram) containing the bigram
## tails of unobserved trigrams that start with bigramPrefix. The second column
## (probs) holds the probability estimate for the bigram tail described above.
## bigDiscount - bigram discount
## bigramPrefix - first two words of unobserved trigrams we want to estimate
##                probabilities of
## trigrams - data.table of all trigrams in corpus and their counts/frequencies
## bigrams - data.table of all bigrams in corpus and their counts/frequencies
## unigrams - data.table of all unigrams in corpus and their counts/frequencies
calc.qBO.bigramsB <- function(bigDiscount=0.5, bigramPrefix,
                              trigrams, bigrams, unigrams) {
    unobsTrigs <- getUnobsTrigs(bigramPrefix, trigrams)
    unobBis <- getUnobsBigramsTable(bigramPrefix, unobsTrigs, bigrams, unigrams)
    unobBiProbs <- rep(-1, length(unobBis$ngram))
    # calc discounted prob. mass at bigram level
    unig <- str_split(bigramPrefix, '_')[[1]][2]
    unigram=getNgramTables(1, prefixFilter = unig)
    alphaBig <- getAlphaBigram(bigDiscount, bigrams, unigram)
    uniSumUnobs <- sum(filter(unobBis, btfreq == -1)$utfreq)
    for(i in 1:length(unobBis$ngram)) {
        if(unobBis$btfreq[i] > 0) {
            # bigram tail observed: calc qBO from eqn. 9.
            unobBiProbs[i] <- (unobBis$btfreq[i]-bigDiscount)/unigram$freq[1]
        } else {
            # bigram tail NOT observed: calc qBO w/ bigram from eqn. 15.
            unobBiProbs[i] <- alphaBig * unobBis$utfreq[i] / uniSumUnobs
        }
    }
    dt <- data.table(ngram=unobBis$ngram, probs=unobBiProbs)
    return(dt)
}

calc.qBO.trigramB <- function() {
    qBoUnobsBigs <- calc.qBO.bigramsB(bigDiscount=0.5, bigramPrefix,
                                      trigrams, bigrams, unigrams, unigram)
    sum.qBoUnobsBigs <- sum(qBoUnobsBigs$probs)
}

