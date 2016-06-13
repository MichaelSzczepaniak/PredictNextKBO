# install packages if needed
list.of.packages <- c('dplyr', 'readr', 'stringr', 'quanteda')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
# libs <- c('dplyr', 'readr', 'quanteda')
lapply(list.of.packages, require, character.only=TRUE)  # load libs
options(stringsAsFactors = FALSE)  # strings are what we are operating on...
# set parameters
ddir <- "../data/en_US/" # assumes exec from dir at same level as data
fnames <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
fullpaths <- sprintf("%s%s", ddir, fnames)

## Reads the text corpus data file and returns a character array where every
## element is a line from the file.
## fileId = string, text fragment of file name to be read e.g. 'blogs', 'news',
##          or 'twit'
## dataDir = path to data file to be read
## fnames = file names to be read which have fileId fragments
getFileLines <- function(fileId, dataDir=ddir, fileNames=fnames) {
    if(grep(fileId, fnames) > 0) index <- grep(fileId, fnames)
    else {
        cat('getFileLines could undestand what file to read:', fileId)
        return(NULL)
    }
    fileLines <- read_lines(sprintf("%s%s", dataDir, fnames[index]))
    return(fileLines)
}

## Breaks the en_US.<fileType>.txt into training and test sets and writes out
## these files.
## fileType - string, one of 3 values: 'blogs', 'news', or 'twitter'
## train.fraction - float between 0 and 1, fractional amount of data to be used
##                  in the training set
## dataDir - relative path to the data directory
writeTrainTestFiles <- function(fileType, train.fraction=0.8,
                                dataDir=ddir) {
    set.seed(71198)
    prefix <- "en_US."
    in.postfix <- ".txt"
    train.postfix <- ".train.txt"
    test.postfix <- ".test.txt"
    infile <- sprintf("%s%s%s%s", dataDir, prefix, fileType, in.postfix)
    dat <- getFileLines(fileType)
    line.count <- length(dat)
    train.size <- as.integer(train.fraction * line.count)
    test.size <- line.count - train.size
    train.indices <- sample(1:line.count, train.size, replace=FALSE)
    train.indices <- train.indices[order(train.indices)]
    test.indices <- setdiff(1:line.count, train.indices)
    train.set <- dat[train.indices]
    ofile <- sprintf('%s%s%s%s', dataDir, prefix, fileType, train.postfix)
    writeLines(train.set, ofile)
    test.set <- dat[test.indices]
    ofile <- sprintf('%s%s%s%s', dataDir, prefix, fileType, test.postfix)
    writeLines(test.set, ofile)
    
    # return(list(train=train.indices, test=test.indices))
}

## Returns a character vector where every element is a sentence of text.
## charVect - character vector where every element may contain 1 or more
## sentences of text.
## REQUIRES quanteda package
breakOutSentences <- function(charVect) {
    require(quanteda)
    qTokenizeText <- tokenize(charVect, what="sentence")
    sentNormCharVect <- vector(mode = "character")
    for(i in 1:length(qTokenizeText)) {
        sent.tokenized.line <- qTokenizeText[[i]]
        sentNormCharVect <- append(sentNormCharVect, sent.tokenized.line)
    }
    
    return(sentNormCharVect)
}

## Read inFileName, parses each line into sent
parseSentsToFile <- function(inFileName,
                             outDataDir=ddir,
                             outFilePostfix=".1sents.txt") {
    outFileName <- str_replace(inFileName, '.txt', outFilePostfix)
    outFilePath <- sprintf("%s%s", outDataDir, outFileName)
    cat("start parseSentsToFile:", as.character(Sys.time()), "\n")
    cat("processing file:", inFileName, "\n")
    cat("output will be written to:", outFileName, "\n")
    require(quanteda)
    flines <- getFileLines(inFileName)
    
    flines <- breakOutSentences(flines)
    
    writeLines(flines, con = outFilePath)
    cat("wrote output to file:", outFileName, "\n")
    cat("finish parseSentsToFile:", as.character(Sys.time()), "\n")
}

preProcessCorpus <- function() {
    parseSentsToFile(fnames[1])
}