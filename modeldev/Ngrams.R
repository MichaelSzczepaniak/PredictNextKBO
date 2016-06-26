
ddir <- "../data/en_US/"

loadLibs <- function() {
    libs <- c("dplyr", "readr", "stringr", "dplyr", "quanteda",
              "ggplot2", "data.table")
    lapply(libs, require, character.only=TRUE)  # load libs
    options(stringsAsFactors = FALSE)  # strings are what we are operating on...
}

## Somewhat crude estimate of word count, but is very close to other methods.
## Assumes that words are separated by spaces.
getUniqueWords <- function(fname) {
    fl <- getLocalDataLines(fname)
    fl <- gsub("[.!,:'\\-]", " ", fl, perl=TRUE) # remove remaining punctuation
    return(unique(unlist(strsplit(fl, " "))))
}

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

## Returns a 2 column data.table. The first column (ngram) contains the
## unigram (if n=1), the bigram (if n=2), etc.. The second column
## (freq) contains the frequency or count of the ngram found in linesCorpus.
##
## linesCorpus - character vector
## prefixFilter - string/character vector: If not NULL, tells the function
##                to return only rows where ngram column starts with prefixFilter.
##                If NULL, returns all the ngram and count rows.
getNgramTables <- function(n, linesCorpus, prefixFilter=NULL) {
    cat("start getNgramTables:", as.character(Sys.time()), "\n")
    ngrams <- getNgramFreqs(n, linesCorpus)
    ngrams.dt <- data.table(ngram=names(ngrams), freq=ngrams)
    if(length(grep('^SOS', ngrams.dt$ngram)) > 0) {
        ngrams.dt <- ngrams.dt[-grep('^SOS', ngrams.dt$ngram),]
    }
    if(!is.null(prefixFilter)) {
        regex <- sprintf('%s%s', '^', prefixFilter)
        ngrams.dt <- ngrams.dt[grep(regex, ngrams.dt$ngram),]
    }
    cat("FINISH getNgramTables:", as.character(Sys.time()), "\n")
    return(ngrams.dt)
}