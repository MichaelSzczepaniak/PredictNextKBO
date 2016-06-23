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
## NOTE2: This function took over 22 hrs to run on my quad-core Xeon with
##        16Gb RAM on the twitter 80% training set.
##
## charVect - character vector where every element may contain 1 or more
##            sentences of text
## check.status - the number of lines to process before writing a status
##                message to the console
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

## Removes all the non-ASCII characters from charVect and then returns a
## character vector that contains only ASCII characters.  This function is
## intended to be passed to the runFilterAndWrite function.
convertToAscii <- function(charVect) {
    cat("convertToAscii: start UTF-8 to ASCII conversion...\n")
    charVectAscii <- iconv(charVect, from="UTF-8", to="ASCII")
    charVectAscii <- charVect[-which(is.na(charVectAscii))]
    cat("convertToAscii: finished converting UTF-8 to ASCII.\n")
    return(charVectAscii)
}

## Replaces the unicode tag delimiting contractions and plural possesive forms
## with a ASCII single quote character in the character vector charVect, 
## replaces all other unicode tags with spaces, and then returns the updated
## character vector.  This function is intended to be passed to the
## runFilterAndWrite function.
convertUnicodeTags <- function(charVect) {
    cat("convertUnicodeTags: start replacing unicode tags...\n")
    singleQuotePatter <- "([A-Za-z]{1})(<U[+][A-Fa-f0-9]{4}>)(s|d|ve|t|ll|re)"
    unicodePattern <- "<U[+][A-Fa-f0-9]{4}>"
    imFixPattern <- "([Ii])(<U[+][A-Fa-f0-9]{4}>)([mM])"
    charVectContractions <- str_replace_all(charVect, singleQuotePatter,
                                            "\\1'\\3")
    charVectImFix <- str_replace_all(charVectContractions, imFixPattern,
                                     "\\1'\\3")
    # Replace remaining unicode tags with spaces because extra spaces
    # will get cleaned up in a later pre-processing step.
    charVectNoTags <- str_replace_all(charVectImFix, unicodePattern, ' ')
    cat("convertUnicodeTags: FINISHED replacing unicode tags.\n")
    return(charVectNoTags)
}

## Removes anything not a word character or character needed to create
## one of the profanity words/phrases in the profanity list.
preProfFilter <- function(samp, is.news=FALSE, perl.flag=TRUE) {
    # The regex below gets no hits in the updated pre-processing scheme
    samp <- gsub("[^ A-Za-z0-9.!'_*+<>&@#()$\\^\\[\\]\\-]", "", samp, perl=perl.flag)
    return(samp)
}

## Runs the pre-profanity filter on dataDir/inFileName and outputs results to
## the same directory using outFilePostfix as the postfix in output file name.
runPreProfFilter <- function(dataDir=ddir,
                             inFileName='en_US.blogs.train.2sents.txt',
                             outFilePostfix='.3preprof.txt') {
    inPath <- sprintf("%s%s", dataDir, inFileName)
    cat("START runPreProfFilter on:", inPath, as.character(Sys.time()), "\n")
    outFileName <- str_replace(inFileName, '.2sents.txt', outFilePostfix)
    
    outPath <- sprintf("%s%s", dataDir, outFileName)
    fileLines <- read_lines(inPath)
    preProfFiltered <- preProfFilter(charVect=fileLines)
    writeLines(preProfFiltered, outPath)
    cat("FINISH runPreProfFilter. File written to:", outPath,
        as.character(Sys.time()), "\n")
}

## Removes most URL's that start with either http, https, or www. from the
## character vector charVect and returns the resulting character vector.
## This function is intended to be passed to the runFilterAndWrite function.
removeUrls <- function(charVect) {
    cat("removeUrls: start removing urls...\n")
    # Build regex to remove URLs. No shorthand character classes in R,
    # so need to create by hand
    wordChars <- "A-Za-z0-9_\\-"
    # urlRegex <- "(http|https)://[\w\-_]+(\.[\w\-_]+)+[\w\-.,@?^=%&:/~\\+#]*"
    urlRegex1 <- sprintf("%s%s%s", "(http|https)(://)[", wordChars, "]+")
    urlRegex2 <- sprintf("%s%s%s", "(\\.[", wordChars, "]+)+")
    urlRegex2 <- sprintf("%s%s%s%s", urlRegex2, "[", wordChars, ".,@?^=%&:/~\\+#]*")
    urlRegex <- sprintf("%s%s", urlRegex1, urlRegex2)
    charVect <- gsub(urlRegex, "", charVect, perl=TRUE)
    # clean up www.<something> instances that don't start with http(s)
    urlRegexWww <- sprintf("%s%s%s%s", "( www\\.)[", wordChars, "]+", urlRegex2)
    charVect <- gsub(urlRegexWww, "", charVect, perl=TRUE)
    cat("removeUrls: FINISHED removing urls.\n")
    return(charVect)
}

## Consolidates the tasks of reading data in and writing data out as part of
## filtering or cleaning the data.
## FUN - function to run against the input data
## dataDir - directory where the input is read and the output is written
## inFilePostfix - suffix of input data files that are read in and passed to FUN
## outFilePostfix - suffix of output data files that are written after FUN has
##                  has processed the input.
## filePrefixes - prefixes of the files to be read in and written out
runFilterAndWrite <- function(FUN, dataDir=ddir, inFilePostfix, outFilePostfix,
                              filePrefixes=c('en_US.blogs.train',
                                             'en_US.news.train',
                                             'en_US.twitter.train')) {
    infiles <- c(sprintf('%s%s%s', dataDir, filePrefixes[1], inFilePostfix),
                 sprintf('%s%s%s', dataDir, filePrefixes[2], inFilePostfix),
                 sprintf('%s%s%s', dataDir, filePrefixes[3], inFilePostfix))
    names(infiles) <- c('blogs', 'news', 'twitter')
    outfiles <- c(sprintf('%s%s%s', dataDir, filePrefixes[1], outFilePostfix),
                  sprintf('%s%s%s', dataDir, filePrefixes[2], outFilePostfix),
                  sprintf('%s%s%s', dataDir, filePrefixes[3], outFilePostfix))
    names(outfiles) <- names(infiles)
    
    cat("runFilterAndWrite: start running filter...\n")
    
    for(i in names(infiles)) {
        charVect <- read_lines(infiles[i])
        charVectFiltered <- FUN(charVect)
        writeLines(charVectFiltered, outfiles[i])
    }
    cat("convertUnicodeTags: FINISHED replacing unicode tags.\n")
}

## Removes URLs and anything not a word, space, or basic punctuation character
## such as ?.!,:'- in a somewhat intelligent manner.
postProfClean <- function(samp, is.news=TRUE) {
    
    if(!is.news) { samp <- nonNewsPostProfClean(samp) }
    # remove anything that's not an alpha, digit basic punctuation char
    # will replace digits with NUM later
    samp <- gsub("[^A-Za-z0-9?.!,:'\\-]", " ", samp, perl=TRUE)
    samp <- gsub("( ){2,}", " ", samp, perl=TRUE)  # replace >=2 spaces w/single space
    samp <- gsub("^( . )", " ", samp, perl=TRUE)
    samp <- gsub("^( ){1,}", "", samp, perl=TRUE)  # remove leading spaces
    samp <- gsub("[ ]{1,}$", "", samp, perl=TRUE)  # remove trailing spaces
    # remove non-alpha char's that start sentences
    samp <- gsub("^[^A-Za-z]+", "", samp)
    # make lines that don't end in . ! or ? empty so they'll be removed later
    samp <- gsub("^.*[^.!?]$", "", samp)
    # replace non-word-period by just period
    samp <- gsub("([^A-Za-z0-9]+.)$", ".", samp)
    # remove lines that don't have any alpha characters
    samp <- gsub("^[^A-Za-z]+$", "", samp, perl=TRUE)
    # remove empty lines
    samp <- samp[which(samp  != "")]
    # replace 2 or more spaces with a single space
    samp <- gsub("[ ]{2,}", " ", samp, perl=TRUE)
    # normalize text to lower case
    samp <- tolower(samp)
    # replace sequences of digits by NUM token: after lower case to keep
    # this special token UPPER CASE in the processed file
    samp <- gsub("[0-9]+", "NUM", samp)
    
    return(samp)
}

## Merges two contigency tables firstTab and secondTab and returns the merged table
mergeContingencyTables <- function(firstTab, secondTab, char.labels=TRUE) {
    # normalize the first table
    Un <- union(names(firstTab), names(secondTab))
    third <- as.table(setNames(rep(0, length(Un)), Un))
    firstTab1 <- c(firstTab, third)
    firstTab2 <- firstTab1[!duplicated(names(firstTab1))]
    if(char.labels) {
        firstTabFull <- as.table(firstTab2[order(names(firstTab2))])
    } else {
        firstTabFull <- as.table(firstTab2[order(as.numeric(names(firstTab2)))])
    }
    # normalize the second table
    secondTab1 <- c(secondTab, third)
    secondTab2 <- secondTab1[!duplicated(names(secondTab1))]
    if(char.labels) {
        secondTabFull <- as.table(secondTab2[order(names(secondTab2))])
    } else {
        secondTabFull <- as.table(secondTab2[order(as.numeric(names(secondTab2)))])
    }
    
    thirdFull <- firstTabFull + secondTabFull
    
    return(thirdFull)
}