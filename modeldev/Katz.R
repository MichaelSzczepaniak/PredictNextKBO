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

getTrigrams <- function(corpFile="../data/little_test_corpus1.txt") {
    lines <- read_lines(corpFile)
    trigs <- getNgramFreqs(3, lines)
    dt <- data.table(ngram=names(trigs), freq=trigs)
    dt <- dt[-grep('^SOS', dt$ngram),]
}

getTrigramWinA <- function(bigramPrefix, trigrams) {
    
}

alphaTrigram <- function(discount=0.5, wInA) {
    
}

getTrigramInB <- function() {
    
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