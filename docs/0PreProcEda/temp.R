---
title: "NLP Predicting Next Word Project - Preprocessing & EDA"
author: "Michael Szczepaniak"
date: "May 2016"
output: html_document
url: http://rpubs.com/mszczepaniak/163777
---
## Synopsis  
World wide mobile internet usage is projected to continue its rapid growth over the next few years.  According to a [Statista Fact Sheet](http://www.statista.com/statistics/284202/mobile-phone-internet-user-penetration-worldwide/), the percentage of mobile phone users accessing the internet will rise to 63.4% in 2019 up from 48.8% in 2014.  This increased ownship has resulted in more people spending increasing amounts of time on mobile devices for email, social networking, banking and other activities. Because typing on these devices is an awkward and tedious task, smart keyboard applications based on predictive text analytics have emerged to make typing easier.  

The goal of this application is to develop a predictive web application that suggests the next word in a text based message based on what has been typed in by the user. For example, a user may type *I love Italian* and the the application might suggest: *food*, *shoes*, or *opera*.

## Acquiring and Cleaning the Data
### Acquiring and Reading the Data
The data was downloaded from [this link](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) and stored locally. The file was download to a directory called **data** in a local project and unzipped there.  The the unzipped data contained four subdirectories: **de_DE** (German), **en_US** (US english), **fi_FI** (Finnish), and **ru_RU** (Russian). This project focuses on the English corpora residing in the **en_US** folder. This folder contains three files named **en_US.blogs.txt**, **en_US.news.txt**, and **en_US.twitter.txt**.

```{r message=FALSE, warning=FALSE, results='hide'}
# init
libs <- c("dplyr", "readr", "tm", "RWeka", "SnowballC", "ggplot2")
lapply(libs, require, character.only=TRUE)  # load libs
options(stringsAsFactors = FALSE)  # strings are what we are operating on...
# set parameters
dataDir <- "../data/en_US/"
filenames <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
fullpaths <- sprintf("%s%s", dataDir, filenames)
## Different issues with each data file forces use of different read functions
getFileLines <- function(fileId) {
    fileLines <- NULL
    if(fileId == "blogs") {
        fileLines <- readLines(sprintf("%s%s", dataDir, filenames[1]))
    }
    else if(fileId == "news") {
        fileLines <- read_lines(sprintf("%s%s", dataDir, filenames[2]))
    }
    else if(fileId == "twitter") {
        fileLines <- readLines(sprintf("%s%s", dataDir, filenames[3]))
    }
    else {
        cat("getFilesLines: INVALID fileId parameter!")
    }
    
    return(fileLines)
}
```

### Cleaning the Data
After the data was read in, it was cleaning followed two general steps. First, profanity was removed. Second, all characters that were not either a letter or essential punctuation such as apostrophes, periods, etc. were removed. The profanity removal step required its own three sub-steps:

1. building a list of profanity words/terms
2. doing a "pre-profanity" filtering which left in characters needed for the profanity words, and then
3. removing all the words in the profanity list.

The profanity list was a union of three lists from the following sources: [google](http://fffff.at/googles-official-list-of-bad-words/), [Luis von Ahn of Carnegio Mellon University ](http://www.cs.cmu.edu/~biglou/resources/bad-words.txt), and [a github project of Duncan Robertson from London, UK](https://github.com/whomwah/language-timothy/blob/master/profanity-list.txt). These lists were downloaded and merged together using the **mergeTermLists** function and profanity was removed using the **removeProfanity** function. Both of these functions are listed in the **Appendix** below.

As part of the profanity removal process, all alpha characters were converted to lower case. Following the profanity removal and case folding, a final filtering step was done using the **cleanText** function to remove urls and any non-essential characters.

All the steps described so far were done off-line (not executed in the Rmd file) because they were very time intensive. To speed future processing, the three cleaned en_US files were rewritten back to the local file system so they could be read in again later.

## Summary Statistics
The following functions were written to get the initial file sizes, line counts, vocabulary (count of all words), and word types (count of unique words) respectively. The results of these functions are shown in the table following the code:
```{r cache=TRUE}
## Returns the file size in Mb
getFileSize <- function(dataDir="../data/en_US/", filename) {
    inputFilePath <- paste0(dataDir, filename)
    return(file.size(inputFilePath) / (2^20))  # convert to MB
}

getLineCount <- function(dataDir="../data/en_US/", fileType) {
    return(length(getFileLines(fileType)))
}

## Somewhat crude estimate of word count, but is very close to other methods.
## Assumes that words are separated by spaces.
getWordCount <- function(fileType) {
    f <- getFileLines(fileType)
    return(length(unlist(strsplit(f, " "))))
}

## Returns the number of unique words (tokens) the fileType file.
getTypeCount <- function(fileType) {
    f <- getFileLines(fileType)
    return(length(unique(unlist(strsplit(f, " ")))))
}
```

```{r cache=TRUE, echo=FALSE, results='hide', message=FALSE}
# gather values for table
f1=filenames[1]; f2=filenames[2]; f3=filenames[3]
s1=getFileSize(filename=f1); s2=getFileSize(filename=f2); s3=getFileSize(filename=f3)
lc1 <- suppressWarnings(getLineCount(fileType="blogs"))  # about 15 sec's
lc2 <- suppressWarnings(getLineCount(fileType="news"))  # less than 10 sec's
lc3 <- suppressWarnings(getLineCount(fileType="twitter"))  # about 15 sec's
voc1=getWordCount("blogs"); voc2=getWordCount("news"); voc3=getWordCount("twitter")
wt1=getTypeCount("blogs"); wt2=getTypeCount("news"); wt3=getTypeCount("twitter")
```
<!--note colon trick to right align text in table -->

File Name | File Size (MB) | Line Count | Word Count (all words) | Word Types (unique words)
----------|---------------:|-----------:|----------:|----------:
`r f1`    | `r round(s1, 2)` | `r lc1` | `r voc1` | `r wt1`
`r f2`    | `r round(s2, 2)` | `r lc2` | `r voc2` | `r wt2`
`r f3`    | `r round(s3, 2)` | `r lc3` | `r voc3` | `r wt3`

### Word Coverage
How many unique words from a frequency sorted dictionary are needed to cover 50% of all words?  How many are needed to cover 90%.  These questions were answered with calls to the functions **** and **** listed in the **Appendix** and displayed in the table below.

```{r cache=TRUE, eval=FALSE}
## Returns unigram counts of fileName.
## Precondition: fileName has been cleaned (e.g. punctuation, profanity, etc. has been removed)
getSortedFrequency <- function(fileName, isNews=FALSE, dataDir="../data/en_US/") {
    filePath <- sprintf("%s%s", dataDir, fileName)
    fileLines <- NULL
    if(isNews) { fileLines <- readLines(filePath) }
    else       { fileLines <- read_lines(filePath)}
    
    vocab <- unlist(strsplit(fileLines, " "))  # all the words
    smashedLines <- paste(vocab, collapse = " ")
    corp.tmp <- Corpus(VectorSource(smashedLines))
    outFileName <- sprintf("%s%s", fileType, ".unigram.counts.csv")
    termFreqs <- c(t(as.matrix(DocumentTermMatrix(corp.tmp, list(stemming=TRUE)))))
    write.csv(termFreqs, file=outFileName)
    termFreqs <- sort(termFreqs, decreasing = TRUE)
}


```

## Interesting Findings
### Apostrophes
Because contractions like *can't*, *won't*, *haven't* etc. maybe important in N-gram modeling, it was important to capture the counts of these words properly from the corpora files. When I first attempted to do this, a problem immediately came up while working on the **en_US.blogs.txt** file. Words did not consistently use the same UTF-8 character for the apostrophe.  The normal ASCII apostrophe character is \\U0027 in UTF-8.  In the blog file, this character occured 387,090 times. In the same file, the **right single quote** character is used almost as much (387,317 times) for the same purpose.

To deal with this, the first part of the **preProfFilter** function shown in the Appendix converts all the different kinds of characters used as apostrophes to the standard apostrophe character as part of text normalization.

### Frequently Use Words - Unigrams
A Document Term Matrix (DTM) was generated from a [stemmed](https://en.wikipedia.org/wiki/Stemming) sample of each file using the **DocumentTermMatrix(Corpus)** function in the **tm** package. The DTMs were used to build the unigram (single words) and bigram (two word pairs) counts as shown in the barplots below.

```{r echo=FALSE, cache=TRUE, warning=FALSE}
## Returns a stemmed Corpus Object from a list of files: corpFiles
## The Corpus is built from a samp.frac (default = 1%) sample from
## each corpus file in the corpFiles vector.
## samp.frac - size of sample to take from corpus files, default is 1%
##             NOTE: When I tried 10%, I got a heap space error.
## corpFiles - list of files to build the corpus from
## dataDir - directory where the corpus data files are located
getSmashedCorpus <- function(samp.frac=0.01,
                             corpFiles=c("en_US.blogs.cleaned.txt",
                                         "en_US.news.cleaned.txt",
                                         "en_US.twitter.cleaned.txt"),
                      dataDir="../data/en_US/") {
    infile1 = corpFiles[1]; infile2 = corpFiles[2]; infile3 = corpFiles[3]
    # create samples from corpora
    flines1 <- readLines(sprintf("%s%s", dataDir, infile1))
    s1 <- sample(flines1, samp.frac*length(flines1))
    rm(flines1)
    flines2 <- readLines(sprintf("%s%s", dataDir, infile2))
    s2 <- sample(flines2, samp.frac*length(flines2))
    rm(flines2)
    flines3 <- suppressWarnings(readLines(sprintf("%s%s", dataDir, infile3)))
    s3 <- sample(flines3, samp.frac*length(flines3))
    rm(flines3)
    chsamps <- list(s1=s1, s2=s2, s3=s3)
    
    smashedLines <- vector(mode = "character", length = 3)
    for(i in 1:3) {
        smashedLines[i] <- paste(chsamps[[i]], collapse = " ")
    }
    corp.tmp <- Corpus(VectorSource(smashedLines))
    corp.tmp <- tm_map(corp.tmp, removePunctuation)
    corp.tmp <- tm_map(corp.tmp, stripWhitespace)
    return(corp.tmp)
}

getPreCleanedDTM <- function(corp) {
    dtm <- DocumentTermMatrix(corp, list(stemming=TRUE))
    return(dtm)
}

## Generates that named matrix of terms and their frequencies in the corpora
getUnigramFrequencies <- function() {
    # cat("start getUnigramFrequencies", as.character(Sys.time()), "\n")
    # most frequent terms, could we use termFreq function here?
    corp <- getSmashedCorpus()
    dtm.samp <- getPreCleanedDTM(corp)
    #dtms <- removeSparseTerms(dtm.samp, 0.2) #  10% empty space, max.
    dtm.samp <- as.matrix(dtm.samp)
    write.csv(dtm.samp, file="dtm.csv")
    term.freq <- colSums(dtm.samp)
    term.freq <- sort(term.freq, decreasing = TRUE)
    # cat("finish getUnigramFrequencies", as.character(Sys.time()), "\n")
    return(term.freq)
}
```

```{r echo=FALSE, cache=TRUE}

makeUnigramPlot <- function(tmfrq) {
     wordFreqs <- data.frame(word=names(tmfrq), freq=tmfrq)
     g <- ggplot(wordFreqs[1:22,], aes(x=reorder(word, -freq), y=freq))
     g <- g + geom_bar(stat="identity")
     g <- g + theme(axis.text.x=element_text(angle=45, hjust=1))
     g <- g + ggtitle("Unigram Frequencies from 1% sample of 3 US Corpora")
     g <- g + xlab("Words") + ylab("Frequency")
     g
}

uf <- getUnigramFrequencies()
makeUnigramPlot(uf)
```

### Bigram and Trigram Frequencies
When I first tried to use the **RWeka** package to build bigram and trigram counts, I started seeing this error:
```
Loading required package: RWeka
Error : .onLoad failed in loadNamespace() for 'rJava', details:
  call: inDL(x, as.logical(local), as.logical(now), ...)
  error: unable to load shared object 'C:/apps/R/R-3.2.3/library/rJava/libs/i386/rJava.dll':
  LoadLibrary failure:  %1 is not a valid Win32 application.

In addition: Warning message:
package 'RWeka' was built under R version 3.2.4 
```

After much consternation, turns out I just needed to update my **PATH** variable by adding another level (/server/) where the **jvm.dll** resides.  The bigram and trigram frequency plots created by calls to the **makeNgramFrequencies** and **makeNgramPlot** functions (listed in the Appendix) are shown below.

```{r echo=FALSE, cache=TRUE}
## Return an unsorted TDM of ng-grams from the character array charArr
getNgrams <- function(charArr, ng) {
    # tutorial on rweka - http://tm.r-forge.r-project.org/faq.html
    # http://rstudio-pubs-static.s3.amazonaws.com/39014_76f8487a8fb84ed7849e96846847c295.html
    corpus <- Corpus(VectorSource(charArr)) # create corpus for TM processing
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    options(mc.cores=1) # http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka/20251039#20251039
    XgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ng, max = ng)) # create n-grams
    tdm <- TermDocumentMatrix(corpus,
                              control = list(tokenize = XgramTokenizer,
                                             stemming = TRUE)) # create tdm from n-grams
    tdm
}

## Returns the topx ng-gram frequency counts sampling the dataDir/infiles data files.
## Function samples samp.freq (default is 1%) of the data files.
getNgramFrequencies <- function(ng=2, topx=22, samp.frac=0.01, dataDir="../data/en_US/",
                                infiles=c("en_US.blogs.cleaned.txt",
                                          "en_US.news.cleaned.txt",
                                          "en_US.twitter.cleaned.txt")
                                ) {
    # cat("start word freq cals", as.character(Sys.time()), "\n")
    # create samples from corpora
    flines1 <- readLines(sprintf("%s%s", dataDir, infiles[1]))
    s1 <- sample(flines1, samp.frac*length(flines1))
    rm(flines1)
    flines2 <- readLines(sprintf("%s%s", dataDir, infiles[2]))
    s2 <- sample(flines2, samp.frac*length(flines2))
    rm(flines2)
    flines3 <- suppressWarnings(readLines(sprintf("%s%s", dataDir, infiles[3])))
    s3 <- sample(flines3, samp.frac*length(flines3))
    rm(flines3)
    dat <- append(s1, s2); dat <- append(dat, s3)
    chsamps <- data.frame(dat)
    # chsamps <- data.frame(s1)
    tdmb <- getNgrams(chsamps, ng)
    # bfq <- findFreqTerms(tdmb, lowfreq = 50)
    
    tdm.Ngram.matrix <- as.matrix(tdmb)
    sorted.ngrams <- sort(tdm.Ngram.matrix[,1], decreasing = TRUE)[1:topx]
    
    # cat("finish word freq cals", as.character(Sys.time()), "\n")
    return(sorted.ngrams)
}

makeNgramPlot <- function(tmfrq, plotTitle, topx=22) {
     wordFreqs <- data.frame(word=names(tmfrq), freq=tmfrq)
     g <- ggplot(wordFreqs[1:topx,], aes(x=reorder(word, -freq), y=freq))
     g <- g + geom_bar(stat="identity")
     g <- g + theme(axis.text.x=element_text(angle=45, hjust=1))
     g <- g + ggtitle(plotTitle)
     g <- g + xlab("Words") + ylab("Frequency")
     g
}

txcounts <- 22
n2gramFreqs <- getNgramFrequencies(ng=2, txcounts)
p2title <- sprintf("%s%s%s", "Top ", txcounts, " Bigram Frequencies from 1% sample of 3 US Corpora")
makeNgramPlot(n2gramFreqs, p2title)

n3gramFreqs <- getNgramFrequencies(ng=3, txcounts)
p3title <- sprintf("%s%s%s", "Top ", txcounts, " Trigram Frequencies from 1% sample of 3 US Corpora")
makeNgramPlot(n3gramFreqs, p3title)

```


## Future Work
* To get more realistic N-gram counts, I also need to redo how the DTMs were built. The "smashing lines" into a single element does not honor sentence structure. This is based on the assumption that the application should not be predicting next words based on N-grams that span sentences.
* Need to build trigrams and 4-grams.
* Need to remove some of the profanity words in the current list as I'm finding that there are many words that aren't really profanity words.
* Start building implementation of prediction algorithm (see next section for details).

## Prediction Algorithm Development
The initial iteration of the application will utilize a statistically-based [**N-gram** language model](https://class.coursera.org/nlp/lecture/14) with N=3 (trigram) as the foundation for making its predictions. The following iteration will upgrade this to a [Katz's Backoff Model](https://en.wikipedia.org/wiki/Katz's_back-off_model). Future iterations will then focus on enhancements to improve and balance accuracy and performance.  

After the algorithm is sufficiently mature, it will be integrated with a simple user interface (UI) that provides a text box for the user to enter in text and a button to initiate the prediction of the next word. After UI integration, the code will be deployed as as a [**Shiny** web application](https://www.shinyapps.io/).

## Appendix
```{r eval=FALSE}
# Returns a dataframe of profanity words built as a superset of two sources.
# The default sources used to build the list are:
# 1) http://fffff.at/googles-official-list-of-bad-words/
# 2) http://www.cs.cmu.edu/~biglou/resources/bad-words.txt
# Note: 1) was converted from the JSON format to newline-delimited text
mergeTermLists <- function(dataDir="../data/en_US/",
                           src1=sprintf("%s%s", dataDir, "profanity.google.txt"),
                           src2=sprintf("%s%s", dataDir, "profanity.biglou.txt"),
                           skipLines1=1, skipLines2=1) {
    v1 <- read.csv(src1, sep="\n", header=FALSE, skip=skipLines1,
                   stringsAsFactors=FALSE)
    v1 <- v1[1:nrow(v1), "V1"]
    v2 <- read.csv(src2, sep="\n", header=FALSE, skip=skipLines2,
                   stringsAsFactors=FALSE)
    v2 <- v2[1:nrow(v2), "V1"]
    
    merged.list <- data.frame(badwords = union(v1, v2))
    
    return(merged.list)
}

## Replace chars similar to single quotes with simple ascii single quote chars
## and then removes anything not a word character or character needed to create
## one of the profanity words/phrases in the profanity list.
preProfFilter <- function(samp) {
    # Handle right single quotes: 387317 instances in the blog file
    # http://stackoverflow.com/questions/2477452/%C3%A2%E2%82%AC%E2%84%A2-showing-on-page-instead-of
    samp <- gsub("(\xE2\x80\x99)", "'", samp, perl=TRUE)
    # Handle other chars that are like single quotes:
    samp <- gsub("[\U0027\U00B4\U0092\U0060\U02BB\U02BC\U2018\U2019]",
                 "'", samp, perl=TRUE)
    # Remove chars that can't be used to create profanity
    samp <- gsub("[^A-Za-z0-9.!'_*+<>&@#()$\\^\\[\\]\\-]", " ", samp, perl=TRUE)
    return(samp)
}

## Converts all the text in flines to lower case (because profanity list is in lower case)
## and then removes all profanity in the flines character vector before returning it.
removeProfanity <- function(profList, flines) {
    # plist <- readLines("../data/en_US/profanity.final.txt")
    cat("Start removeProfanity:", as.character(Sys.time()), "\n")
    flines <- getFileLines(fileType)
    for(i in 1:length(flines)) {
        lin <- flines[i]
        lin <- preProfFilter(lin)
        lin <- tolower(lin)  # profanity list is in lower case
        corp.tmp <- Corpus(VectorSource(lin))
        corp.tmp <- tm_map(corp.tmp, removeWords, profList)
        flines[i] <- corp.tmp[[1]]$content
    }
    
    cat("Finish removeProfanity:", as.character(Sys.time()), "\n")
    return(flines)
}

## Removes URLs and anything not a word, space, or basic punctuation character
## Performance: ~20sec on news file
cleanText <- function(samp) {
    # No shorthand character classes in R, so need to create by hand
    wordChars <- "A-Za-z0-9_\\-"
    urlRegex <- sprintf("%s%s%s", "(http|https)://[", wordChars, "]+")
    urlRegex <- sprintf("%s%s%s%s", urlRegex, "(.[", wordChars, "]+)+")
    urlRegex <- sprintf("%s%s%s%s", urlRegex, "[", wordChars, ".,@?^=%&:/~\\+#]*")
    # urlRegex <- "(http|https)://[\w\-_]+(\.[\w\-_]+)+[\w\-.,@?^=%&:/~\\+#]*"
    samp <- gsub(urlRegex, "", samp, perl=TRUE)
    samp <- gsub("[^A-Za-z?.!,:'\\-]", " ", samp, perl=TRUE)
    samp <- gsub("( ){2,}", " ", samp, perl=TRUE)  # replace >=2 spaces w/single space
    samp <- gsub("^( . )", " ", samp, perl=TRUE)
    samp <- gsub("^( ){1,}", "", samp, perl=TRUE)  # remove leading spaces
    samp <- gsub("$( ){1,}", "", samp, perl=TRUE)  # remove trailing spaces
    return(samp)
}

## Returns a stemmed Corpus Object from a list of files: corpFiles
## The Corpus is build from a samp.frac (default = 1%) sample from
## each corpus file in the corpFiles vector.
## samp.frac - size of sample to take from corpus files, default is 1%
##             NOTE: When I tries 10%, I got a heap space error.
## corpFiles - list of files to build the corpus from
## dataDir - directory where the corpus data files are located
getSmashedCorpus <- function(samp.frac=0.01,
                             corpFiles=c("en_US.blogs.cleaned.txt",
                                         "en_US.news.cleaned.txt",
                                         "en_US.twitter.cleaned.txt"),
                      dataDir="../data/en_US/") {
    infile1 = corpFiles[1]; infile2 = corpFiles[2]; infile3 = corpFiles[3]
    # create samples from corpora
    flines1 <- readLines(sprintf("%s%s", dataDir, infile1))
    s1 <- sample(flines1, samp.frac*length(flines1))
    rm(flines1)
    flines2 <- readLines(sprintf("%s%s", dataDir, infile2))
    s2 <- sample(flines2, samp.frac*length(flines2))
    rm(flines2)
    flines3 <- suppressWarnings(readLines(sprintf("%s%s", dataDir, infile3)))
    s3 <- sample(flines3, samp.frac*length(flines3))
    rm(flines3)
    chsamps <- list(s1=s1, s2=s2, s3=s3)
    
    smashedLines <- vector(mode = "character", length = 3)
    for(i in 1:3) {
        smashedLines[i] <- paste(chsamps[[i]], collapse = " ")
    }
    corp.tmp <- Corpus(VectorSource(smashedLines))
    corp.tmp <- tm_map(corp.tmp, removePunctuation)
    corp.tmp <- tm_map(corp.tmp, stripWhitespace)
    return(corp.tmp)
}

getPreCleanedDTM <- function(corp) {
    dtm <- DocumentTermMatrix(corp, list(stemming=TRUE))
    return(dtm)
}

## Generates that named matrix of terms and their frequencies in the corpora
getUnigramFrequencies <- function() {
    # cat("start getUnigramFrequencies", as.character(Sys.time()), "\n")
    # most frequent terms, could we use termFreq function here?
    corp <- getSmashedCorpus()
    dtm.samp <- getPreCleanedDTM(corp)
    #dtms <- removeSparseTerms(dtm.samp, 0.2) #  10% empty space, max.
    dtm.samp <- as.matrix(dtm.samp)
    write.csv(dtm.samp, file="dtm.csv")
    term.freq <- colSums(dtm.samp)
    term.freq <- sort(term.freq, decreasing = TRUE)
    # cat("finish getUnigramFrequencies", as.character(Sys.time()), "\n")
    return(term.freq)
}

makeUnigramPlot <- function(tmfrq) {
     wordFreqs <- data.frame(word=names(tmfrq), freq=tmfrq)
     g <- ggplot(wordFreqs[1:22,], aes(x=reorder(word, -freq), y=freq))
     g <- g + geom_bar(stat="identity")
     g <- g + theme(axis.text.x=element_text(angle=45, hjust=1))
     g <- g + ggtitle("Unigram Frequencies from 1% sample of 3 US Corpora")
     g <- g + xlab("Words") + ylab("Frequency")
     g
}

## Return an unsorted TDM of ng-grams from the character array charArr
getNgrams <- function(charArr, ng) {
    # tutorial on rweka - http://tm.r-forge.r-project.org/faq.html
    # http://rstudio-pubs-static.s3.amazonaws.com/39014_76f8487a8fb84ed7849e96846847c295.html
    corpus <- Corpus(VectorSource(charArr)) # create corpus for TM processing
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    options(mc.cores=1) # http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka/20251039#20251039
    XgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ng, max = ng)) # create n-grams
    tdm <- TermDocumentMatrix(corpus,
                              control = list(tokenize = XgramTokenizer,
                                             stemming = TRUE)) # create tdm from n-grams
    tdm
}

## Returns the topx ng-gram frequency counts sampling the dataDir/infiles data files.
## Function samples samp.freq (default is 1%) of the data files.
getNgramFrequencies <- function(ng=2, topx=22, samp.frac=0.01, dataDir="../data/en_US/",
                                infiles=c("en_US.blogs.cleaned.txt",
                                          "en_US.news.cleaned.txt",
                                          "en_US.twitter.cleaned.txt")
                                ) {
    # cat("start word freq cals", as.character(Sys.time()), "\n")
    # create samples from corpora
    flines1 <- readLines(sprintf("%s%s", dataDir, infiles[1]))
    s1 <- sample(flines1, samp.frac*length(flines1))
    rm(flines1)
    flines2 <- readLines(sprintf("%s%s", dataDir, infiles[2]))
    s2 <- sample(flines2, samp.frac*length(flines2))
    rm(flines2)
    flines3 <- suppressWarnings(readLines(sprintf("%s%s", dataDir, infiles[3])))
    s3 <- sample(flines3, samp.frac*length(flines3))
    rm(flines3)
    dat <- append(s1, s2); dat <- append(dat, s3)
    chsamps <- data.frame(dat)
    # chsamps <- data.frame(s1)
    tdmb <- getNgrams(chsamps, ng)
    # bfq <- findFreqTerms(tdmb, lowfreq = 50)
    
    tdm.Ngram.matrix <- as.matrix(tdmb)
    sorted.ngrams <- sort(tdm.Ngram.matrix[,1], decreasing = TRUE)[1:topx]
    
    # cat("finish word freq cals", as.character(Sys.time()), "\n")
    return(sorted.ngrams)
}

makeNgramPlot <- function(tmfrq, plotTitle, topx=22) {
     wordFreqs <- data.frame(word=names(tmfrq), freq=tmfrq)
     g <- ggplot(wordFreqs[1:topx,], aes(x=reorder(word, -freq), y=freq))
     g <- g + geom_bar(stat="identity")
     g <- g + theme(axis.text.x=element_text(angle=45, hjust=1))
     g <- g + ggtitle(plotTitle)
     g <- g + xlab("Words") + ylab("Frequency")
     g
}


```

