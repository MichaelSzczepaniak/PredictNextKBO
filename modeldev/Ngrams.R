
ddir <- "D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/"
ddir.ngram <- sprintf("%s%s", ddir, 'ngrams/')

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
## igfs - Character vector of words (features) to ignore from frequency table
## sort.by.ngram - If TRUE (default), returned table is sorted by ngram
## sort.by.freq - If TRUE, returned table is sorted by frequency, default=FALSE
## prefixFilter - string/character vector: If not NULL, tells the function
##                to return only rows where ngram column starts with prefixFilter.
##                If NULL, returns all the ngram and count rows.
getNgramTables <- function(n, linesCorpus, igfs=NULL, sort.by.ngram=TRUE,
                           sort.by.freq=FALSE, prefixFilter=NULL) {
    cat("start getNgramTables:", as.character(Sys.time()), "\n")
    ngrams <- getNgramFreqs(n, linesCorpus, igfs, sort.by.ngram, sort.by.freq)
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

## Creates and writes out the raw n-gram frequecy tables for each of the 
## corpus data files.  These are the initial n-gram tables that include the
## singletons.  Defaults to unigrams: n=1
## table.dir - string: dir where files to processes reside
## filePrefix - string: prefix of files to process
## inFilePostfix - string: ending/postfix portion of input file name
## outFilePostfix - string: ending/postfix portion of output file name
## n - integer: 1 if unigram table is to be created, 2 if bigram, 3 if trigram
makeRawNgrams <- function(table.dir=ddir, filePrefix="en_US.",
                          inFilePostfix=".train.8posteos.txt",
                          outFilePostfix=".train.9rawunig.csv",
                          fileTypes=c("blogs", "news", "twitter"), n=1) {
    inPaths <- sprintf("%s%s%s%s", table.dir, filePrefix, fileTypes,
                       inFilePostfix)
    outPaths <- sprintf("%s%s%s%s", table.dir, filePrefix, fileTypes,
                        outFilePostfix)
    for(i in 1:length(inPaths)) {
        charvect <- read_lines(inPaths[i])
        ngrams.raw <- getNgramTables(n, charvect)
        write.csv(ngrams.raw, outPaths[i], row.names = FALSE)
    }
}

## Reads in a n-gram frequency csv file and writes two files: 1) a n-gram
## frequency file with the singletons removed and 2) the n-gram singletons
## that were removed from the file. Both output files are sorted by ngram.
## table.dir - string, path to data directory
## filePrefix - string, input and output file prefixes, default="en_US."
## inFilePostfix - string, postfix for the input ngram frequency tables
## out1FilePostfix - string, postfix for the output ngram frequency tables
##                   which have had their singletons removed
## out2FilePostfix - string, postfix for the output files that contain the
##                   removed singletons
removeSingltetons <- function(table.dir=ddir, filePrefix="en_US.",
                              inFilePostfix=".train.9rawunig.csv",
                              out1FilePostfix=".train.11unigrams.nosins.csv",
                              out2FilePostfix=".train.10usingles.txt",
                              fileTypes=c("blogs", "news", "twitter")) {
    inPaths <- sprintf("%s%s%s%s", table.dir, filePrefix, fileTypes,
                       inFilePostfix)
    out1Paths <- sprintf("%s%s%s%s", table.dir, filePrefix, fileTypes,
                         out1FilePostfix)
    out2Paths <- sprintf("%s%s%s%s", table.dir, filePrefix, fileTypes,
                         out2FilePostfix)
    for(i in 1:length(inPaths)) {
        raw.ngrams <- read.csv(inPaths[i])
        nonsingle.ngrams <- filter(raw.ngrams, freq > 1)
        nonsingle.ngrams <- arrange(nonsingle.ngrams, ngram)
        ngram.singletons <- filter(raw.ngrams, freq == 1)
        ngram.singletons <- arrange(ngram.singletons, ngram)$ngram
        write.csv(nonsingle.ngrams, out1Paths[i], row.names = FALSE)
        writeLines(ngram.singletons, out2Paths[i])
    }
}

## Creates and writes out n-gram frequecy tables for each of the corpus data
## files for n = 2 or 3.  These tables do NOT include n-grams built from
## unigram singletons.  To build trigram table use the default:
## n=3, and outFilePostfix=".train.13trigrams.nous.csv"
## to build bigram tables, use:
## n=2, and outFilePostfix=".train.12trigrams.nous.csv"
makeBiTrigrams <- function(n=3, table.dir=ddir, filePrefix="en_US.",
                           inFilePostfix=".train.9ustokens.txt",
                           outFilePostfix=".train.13trigrams.nous.csv",
                           ignored.features=c("USIN"),
                           fileTypes=c("blogs", "news", "twitter")) {
    inPaths <- sprintf("%s%s%s%s", table.dir, filePrefix, fileTypes,
                       inFilePostfix)
    outPaths <- sprintf("%s%s%s%s", table.dir, filePrefix, fileTypes,
                        outFilePostfix)
    for(i in 1:length(inPaths)) {
        cat("building ", n, "-gram frequency table for:\n")
        cat(inPaths[i], "\n")
        charvect <- read_lines(inPaths[i])
        unigrams.raw <- getNgramTables(n, charvect, ignored.features)
        write.csv(unigrams.raw, outPaths[i], row.names = FALSE)
    }
}

## Merges 3 ngram/frequency tables together.  Each table is expected to have
## at least 2 fields: ngram and freq.  The merged table is written to:
## ddir/ngrams/unigramSingletonsAll.csv
writeMergedUnigramSingletons <- function(ngram.freq1, ngram.freq2, ngram.freq3,
                                         ddir=ddir) {
    # sort tables by freq primary, ngram secondary
    ngram.freq1 <- arrange(ngram.freq1, freq, ngram)
    ngram.freq2 <- arrange(ngram.freq2, freq, ngram)
    ngram.freq3 <- arrange(ngram.freq3, freq, ngram)
    # merge blogs and new unigrams and write out result
    merged.1.2 <- mergeFreqTables(ngram.freq1, ngram.freq2, 1000, TRUE)
    merged.1.2 <- arrange(merged.unigrams.raw, frequency, ngram)
    write.csv(merged.1.2,
              sprintf("%s%s", ddir, "ngrams/merged.blogs.news.csv"),
              row.names=FALSE)
    # merge blogs, news and twitter unigrams and write out result
    merged.1.2.3 <- mergeFreqTables(merged.1.2, ngram.freq3, 10000, TRUE)
    merged.1.2.3 <- arrange(merged.1.2.3, freq, ngram)
    write.csv(merged.1.2.3,
              sprintf("%s%s", ddir, "ngrams/merged.all.raw.csv"),
              row.names=FALSE)
    # get unigram singletons and write them out
    unigSingles.all <- merged.1.2.3[merged.1.2.3$freq==1,]
    write.csv(unigSingles.all,
              sprintf("%s%s", ddir, "ngrams/unigramSingletonsAll.csv"),
              row.names=FALSE)
}

##### This section of code replaces unigram singletons with a common token #####

## Returns the index in the alphabetizedSingletons character vector where words
## start with each of the the letters a-z
getStartByLetterIndex <- function(alphabetizedSingletons) {
    letterStarts <- vector(mode="integer")
    for(let in letters) {
        regexpr <- sprintf("%s%s", "^", let)
        startOfLetIndex <- grep(regexpr, alphabetizedSingletons)[1]
        letterStarts <- append(letterStarts, startOfLetIndex)
    }
    names(letterStarts) <- letters
    return(letterStarts)
}

## Returns a list of three vectors. The first vector stores the names
## corresponding to the groups that the file was broken into (a, b,..., z).
##
## The second vector stores the line numbers of the first instance of words
## that start with a non-alpha char or a given letter a-z.
##
## The third vector stores line numbers of the last instance of the word
## that started with a given letter
##
## filePath - path to unigram singleton unigram/frequency file. This file is
##            expected to have ngram and freq columns.
##
## Precondition - the ignore files are named: news.ignore.words.txt and
##                twitter.ignore.words.txt respectively
getBreakingIndices <- function(filePath) {
    singletonWords <- read.csv(filePath)$ngram
    alphaStarts <- getStartByLetterIndex(singletonWords)
    endOfGroups <- alphaStarts[2:length(alphaStarts)] - 1
    endOfGroups <- c(endOfGroups, length(singletonWords))
    names(endOfGroups) <- names(alphaStarts)
    
    return(list(group=letters, startGroup=alphaStarts, endGroup=endOfGroups))
}

## Creates and writes out a series of singleton partition files named:
## unigram.singletons[a-z].txt
## Example of partition file names:
## unigram.singletons.a = unigram singletons starting with a
## unigram.singletons.b = unigram singletons starting with b
## ...
## unigram.singletons.z = unigram singletons starting with z
breakWritePartdFiles <- function(usingsDir=ddir.ngram,
                                 unigramFileName="unigramSingletonsAll.csv") {
    inFilePath <- sprintf("%s%s", usingsDir, unigramFileName)
    unigramSingletons <- read.csv(inFilePath, stringsAsFactors=FALSE)$ngram
    groups <- getBreakingIndices(inFilePath)
    outFilePrefix <- "unigram.singletons/unigram.singletons."
    for(i in 1:length(groups[[1]])) {
        outFilePath <- sprintf("%s%s%s%s", usingsDir, outFilePrefix,
                               groups[[1]][i], ".txt")
        # write unigrams that begin with a to unigrams.singletons.a.txt
        # write unigrams that begin with b to unigrams.singletons.b.txt etc...
        writeLines(unigramSingletons[groups[[2]][i]:groups[[3]][i]], outFilePath)
    }
}

## Loads the partitioned unigram singleton files into a list for easy retrieval
## by functions further down the pipline
loadSingletonParts <- function(usingsDir=ddir.ngram,
                               unigramFileName="unigramSingletonsAll.csv") {
    inFilePath <- sprintf("%s%s", usingsDir, unigramFileName)
    groups <- getBreakingIndices(inFilePath)
    singletons <- list()
    inFilePrefix <- "unigram.singletons/unigram.singletons."
    for(i in 1:length(groups[[1]])) {
        groupName <- groups[[1]][i]
        inFilePath <- sprintf("%s%s%s%s", usingsDir, inFilePrefix,
                              groups[[1]][i], ".txt")
        singletons[[groupName]] <- readLines(inFilePath)
    }
    
    return(singletons)
}

## Replaces all the unigram singletons with the a special token specified by
## the usingleton.token: default value = 'USIN' (Unigram SINgleton)
tokenizeUnigramSingletons <- function(filePrefix="en_US", fileType=".news",
                                      inFilePostfix=".train.8posteos.txt",
                                      data.dir=ddir, outdir=ddir,
                                      outFilePostfix=".train.9ustokens.txt",
                                      status.check=10000,
                                      usingleton.token="USIN") {
    infile <- sprintf("%s%s%s", filePrefix, fileType, inFilePostfix)
    # replace all the ngrams that don't start w/alpha char's w/special token
    inpath <- sprintf("%s%s", data.dir, infile)
    inlines <- readLines(inpath)
    cat("start tokenizeUnigramSingletons operation on", inpath, "at",
        as.character(Sys.time()), "\n")
    cat("processing", length(inlines), "lines...\n")
    singletons <- loadSingletonParts()
    counter <- 0
    inlines.edit <- vector(mode="character")
    for(i in 1:length(inlines)) {
        counter <- counter + 1
        line.tokens <- str_split(inlines[i], " ")[[1]] # split line into words
        for(j in 1:length(line.tokens)) {    # iterate through line words checking if
            line.token <- line.tokens[j]     # they are in the ignore.words list
            sublistIndex <- substr(line.token, 1, 1)  # get first char of word for quick check
            # if(grepl("[^a-z]", line.token)) { sublistIndex <- "0" }  # non-alpha start char
            if(line.token %in% singletons[[sublistIndex]]) {
                line.tokens[j] <- usingleton.token
                # cat(line.token, "replaced with USIN at line", i, "\n")
            }
        }
        inlines.edit <- append(inlines.edit, paste(line.tokens, collapse = " "))
        # cat("i =", i, "line = \n", inlines.edit, "\n")
        if(counter == status.check) {
            cat("    completed processing", i, "lines out of", length(inlines),
                "at", as.character(Sys.time()), "\n")
            percentComplete <- (i / length(inlines)) * 100
            cat(percentComplete, "% complete...\n")
            counter <- 0
        }
    }
    outfile <- sprintf("%s%s%s", filePrefix, fileType, outFilePostfix)
    outpath <- sprintf("%s%s", outdir, outfile)
    writeLines(inlines.edit, outpath)
    cat("finish tokenizeUnigramSingletons at", as.character(Sys.time()), "\n")
    
    return(inlines.edit)
}