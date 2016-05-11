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
getNgramTables <- function(n, inputFile="../data/little_test_corpus1.txt",
                           isFreqTable=FALSE) {
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
    
    return(ngrams.dt)
}

## Returns the tail words of all trigrams that start with bigramPrefix.
## Precondition: bigramPrefix is of the format wi-2_wi-1 where w1-2 is the
## 1st word of the trigram and wi-1 is the 2nd/middle word of the trigram.
getTrigramWinA <- function(bigramPrefix, trigrams) {
    regex <- sprintf("%s%s", "^", bigramPrefix)
    trigs.winA <- trigrams[grep(regex, trigrams$ngram)]
    patToReplace <- sprintf("%s%s", bigramPrefix, "_")
    wInA <- str_replace(trigs.winA$ngram, patToReplace, "")
    return(wInA)
}

getTrigramWInB <- function(bigramPrefix, trigrams) {
    allUnigrams <- getNgramTables(1)$ngram
    wInA <- getTrigramWinA(bigramPrefix, trigrams)
    wInB <- setdiff(allUnigrams, wInA)
}

alphaTrigram <- function(discount=0.5, wInA) {
    
}

getTrigramQbo <- function() {
    
}

getBigramQbo <- function() {
    
}

isTrigramObs <- function(trigram, trigrams) {
    
}

isBigramObs <- function(bigram, bigrams) {
    
}

alphaBigram <- function(discount=0.5, wInA) {
    
}

getUnobservedTrigramTails <- function() {
    
}

getUnobservedBigramTails <- function() {
    
}