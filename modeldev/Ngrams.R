
ddir <- "D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/"

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

## Returns a data frame that is the union of data frames ftab1 and ftab2.
## Both ftab1 and ftab2 are expected to have 2 columns: ngram and freq.
## The union is performed by matching on the ngram column and summing the freq
## column when matching ngrams are found.
mergeFreqTables <- function(ftab1, ftab2,
                            checkInterval=100, output.check=FALSE) {
    cat("start mergeFreqTables:", as.character(Sys.time()), "\n")
    mergedNgram <- ftab1$ngram
    mergedFreq <- ftab1$freq
    checkNgram <- ftab2$ngram
    checkFreq <- ftab2$freq
    outerIndex <- length(mergedNgram)
    counter <- 0
    for(i in 1:outerIndex) {
        counter <- counter + 1
        word.f1 <- mergedNgram[i]
        otherIndex <- which(word.f1 == checkNgram)
        if(length(otherIndex) > 0) {
            mergedFreq[i] <- mergedFreq[i] + checkFreq[otherIndex]
            checkNgram <- checkNgram[-otherIndex]  # remove found item
            checkFreq <- checkFreq[-otherIndex]
        }
        if(output.check) {
            if(counter == checkInterval) {
                cat("completed merging", i, "ngrams from 1st list\n")
                cat("size of 2nd list =", length(checkNgram), "|",
                    as.character(Sys.time()), "\n")
                counter <- 0
            }
        }
    }
    mergedTable <- data.frame(ngram=mergedNgram, freq=mergedFreq,
                              stringsAsFactors = FALSE)
    df.unmatched <- data.frame(ngram=checkNgram, freq=checkFreq,
                               stringsAsFactors = FALSE)
    # add rows not matched in ftab2
    mergedTable <- rbind(mergedTable, df.unmatched)
    cat("mergeFreqTables sorting merged frequency table:", as.character(Sys.time()), "\n")
    mergedTable <- arrange(mergedTable, desc(freq))
    cat("finish mergeFreqTables:", as.character(Sys.time()), "\n")
    return(mergedTable)
}