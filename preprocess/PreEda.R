# Install required packages only if they are needed.
list.of.packages <- c('dplyr', 'readr', 'stringr', 'quanteda')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
lapply(list.of.packages, require, character.only=TRUE)  # load libs
options(stringsAsFactors = FALSE)  # strings are what we are operating on...
# set parameters
ddir <- "../data/en_US/" # assumes exec from dir at same level as data
fnames <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
fnames.train <- c("en_US.blogs.train.txt", "en_US.news.train.txt",
                  "en_US.twitter.train.txt")

fullpaths <- sprintf("%s%s", ddir, fnames)

## Reads the text corpus data file and returns a character array where every
## element is a line from the file.
## fileId = string - text fragment of file name to be read. Must be one of 3
##          values: 'blogs', 'news', or 'twitter'
## dataDir = string - path to data file to be read
## fileNames = character vector - File names to be read which have fileId
##             fragments
getFileLines <- function(fileId, dataDir=ddir, fileNames=fnames) {
    if(grep(fileId, fnames) > 0) index <- grep(fileId, fileNames)
    else {
        cat('getFileLines could undestand what file to read:', fileId)
        return(NULL)
    }
    fileLines <- read_lines(sprintf("%s%s", dataDir, fileNames[index]))
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
##
## NOTE1: This function will improperly parse "St. Something" into 2 sentences.
##        It makes other mistakes which one could spend a crazy amount of time
##        fixing, but these others errors are ignored in the interest of time.
##
##        To fix the "Saint" issue, the char vector returned by this function
##        needs to be passing to the annealSaintErrors function to fix most
##        (> 90% based on a manual analysis of the 1st 150k lines of the news
##        file) of these errors.
##
## NOTE2: This function over 22 hrs to run on my quad-core Xeon with 16Gb RAM
##        RAM on the twitter 80% training set.
##
## charVect - character vector where every element may contain 1 or more
## sentences of text.
## Preconditions: This function requires the quanteda package.
breakOutSentences <- function(charVect, check.status=10000) {
    sentenceTokens <- tokenize(charVect, what="sentence")
    sentNormCharVect <- vector(mode = "character")
    counter <- 0
    for(i in 1:length(sentenceTokens)) {
        counter <- counter + 1
        sent.tokenized.line <- sentenceTokens[[i]]
        sentNormCharVect <- append(sentNormCharVect, sent.tokenized.line)
        if(counter == check.status) {
            completed <- (100*i) / length(sentenceTokens)
            cat(i, "breakOutSentences: lines parsed to sentences ",
                completed, "% completed", as.character(Sys.time()), "\n")
            counter <- 0
        }
    }
    
    return(sentNormCharVect)
}

## Repairs (anneals) sentences that were initially parsed improperly across
## the pattern "St. SomeSaintsName".  NOTE: This function took many hours
## to complete on the training data sets.
annealSaintErrors <- function(charVect, status.check=10000) {
    annealedSents <- vector(mode='character')
    next.sent <- ""
    i <- 1
    counter <- 0
    while(i < length(charVect)) {
        counter <- counter + 1
        curr.sent <- charVect[i]
        next.sent <- charVect[i+1]
        hasTerminalSt <- length(grep('(St[.])$', curr.sent)) > 0
        if(hasTerminalSt) {
            # sentence ends with St.: concat w/ following sentence
            annealedSents <- append(annealedSents,
                                    paste(curr.sent, next.sent))
            i <- i + 1
        } else {
            annealedSents <- append(annealedSents, curr.sent)
        }
        i <- i + 1
        if(counter == status.check) {
            completed <- (100*i) / length(charVect)
            cat(i, "annealSaintErrors: lines annealed ",
                completed, "% completed", as.character(Sys.time()), "\n")
            counter <- 0
        }
    }
    annealedSents <- append(annealedSents, next.sent) # add last sentence
    
    return(annealedSents)
}

## Returns the file name of the training set data given fileId which can be
## on of the 3 values: 'blogs', 'news', or 'twitter'. Returns an empty string
## (char vector), if fileId is not one of the 3 expected string values.
getInputDataFileName <- function(fileId) {
    isBlogs <- length(grep(fileId, 'blogs')) > 0
    isNews <- length(grep(fileId, 'news')) > 0
    isTwitter <- length(grep(fileId, 'twitter')) > 0
    if(isBlogs) return(fnames.train[1])
    if(isNews) return(fnames.train[2])
    if(isTwitter) return(fnames.train[3])
    
    return("")
}

## Read inFileName, parses each line into sentences, fixes most of the "Saint"
## parsing errors and writes the results to a file names:
## [original file name].1sents.txt after initial sentence parsing and
## [original file name].2sents.txt after fixing improper sentence breaks across
## the "St. SomeSaintName" tokens.
parseSentsToFile <- function(inFileType,
                             outDataDir=ddir,
                             outFilePostfix1=".1sents.txt",
                             outFilePostfix2=".2sents.txt") {
    
    inFileName <- getInputDataFileName(inFileType)
    outFileName1 <- str_replace(inFileName, '.txt', outFilePostfix1)
    outFileName2 <- str_replace(inFileName, '.txt', outFilePostfix2)
    outFilePath1 <- sprintf("%s%s", outDataDir, outFileName1)
    outFilePath2 <- sprintf("%s%s", outDataDir, outFileName2)
    cat("start parseSentsToFile:", as.character(Sys.time()), "\n")
    cat("processing file:", inFileName, "\n")
    cat("output will be written to:", outFilePath1, "\n")
    
    flines <- getFileLines(fileId=inFileType, dataDir=ddir,
                           fileNames=fnames.train)
    
    flines <- breakOutSentences(flines)
    cat("parseSentsToFile breakOutSentences completed.", "\n")
    writeLines(flines, con = outFilePath1)
    cat("output written to:", outFilePath1, "\n")
    cat("parseSentsToFile annealSaintErrors started...:", as.character(Sys.time()), "\n")
    flines <- annealSaintErrors(flines)
    
    writeLines(flines, con = outFilePath2)
    cat("St. annealed file written to:", outFilePath2, "\n")
    cat("finish parseSentsToFile:", as.character(Sys.time()), "\n")
}

