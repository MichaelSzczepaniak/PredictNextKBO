# install packages if needed
list.of.packages <- c("dplyr", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
libs <- c("dplyr", "readr")
lapply(libs, require, character.only=TRUE)  # load libs
options(stringsAsFactors = FALSE)  # strings are what we are operating on...
# set parameters
dataDir <- "../data/en_US/"
filenames <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
fullpaths <- sprintf("%s%s", dataDir, filenames)

## Read data
getLocalDataLines <- function(fileName, file.is.path=TRUE, dataDir=NULL) {
    fileLines <- NULL
    filePath <- fileName
    if(!file.is.path) { filePath <- sprintf("%s%s", dataDir, fileName) }
    fileLines <- read_lines(filePath)
    
    return(fileLines)
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
parseSentsToFile <- function(inDataDir="../data/en_US/",
                             outDataDir="../data/en_US/final cleaning/",
                             inFileName="en_US.twitter.train.txt",
                             outFileName="en_US.twitter.1sents.txt") {
    outFileName <- sprintf("%s%s", outDataDir, outFileName)
    cat("start parseSentsToFile:", as.character(Sys.time()), "\n")
    cat("processing file:", inFileName, "\n")
    cat("output will be written to:", outFileName, "\n")
    require(quanteda)
    flines <- getLocalDataLines(inFileName, FALSE, inDataDir)
    flines <- breakOutSentences(flines)
    
    writeLines(flines, con = outFileName)
    cat("wrote output to file:", outFileName, "\n")
    cat("finish parseSentsToFile:", as.character(Sys.time()), "\n")
}