#
source('Katz.R')
options(stringsAsFactors = FALSE)  # strings are what we are operating on...
# Use next 3 lines for development
uniPath <- "./data/unigrams.chars.ltc1.csv"
bigPath <- "./data/bigrams.chars.ltc1.csv"
triPath <- "./data/trigrams.chars.ltc1.csv"
# Use the next 3 lines for live deployement
# uniPath <- "./data/unigrams.chars.csv"
# bigPath <- "./data/bigrams.chars.csv"
# triPath <- "./data/trigrams.chars.csv"

# default parameters for bigram and trigram discount rates
gamma2 <- 0.5
gamma3 <- 0.5

getSettings <- function(corpus, bigDisc, trigDisc) {
    corpusLabels <- c("blogs", "news", "twitter")
    corpus <- as.numeric(corpus)
    result <- sprintf("%s%s%s", "corpus=", corpusLabels[corpus], ", ")
    result <- sprintf("%s%s%s%s", result, "bigram discount=", bigDisc, ", ")
    result <- sprintf("%s%s%s", result, "trigram discount=", trigDisc)
    
    return(result)
}

getInputBigram <- function(inputPhrase) {
    inPh <- filterInput(inputPhrase)
    inputTokens <- str_split(inPh, " ")  # 
    lastIndex <- length(inputTokens[[1]])
    w1 <- inputTokens[[1]][(lastIndex-1)]
    w2 <- inputTokens[[1]][(lastIndex)]
    bigram_tail <- paste(w1, w2, sep="_")
    
    return(bigram_tail)
}

getPrediction <- function(inputPhrase) {
    trigs <- read.csv(triPath)
    bigTail <- getInputBigram(inputPhrase)
    obsTrigs <- calc.qBO.trigramsA(bigramPrefix=bigTail, trigrams=trigs)
    
    return(obsTrigs$ngram[1])
}

filterInput <- function(someText) {
    # st <- ""
    # if(nchar(someText) > 1) {
    #     st <- preProfFilter(someText)
    #     proflist <- readLines("./data/profanity.revised.txt")
    #     st <- removeProfanity(proflist, st)
    #     st <- postProfClean(st)
    # }
    # 
    # return(st)
    
    return(someText)
}

## Replace chars similar to single quotes with simple ascii single quote chars
## and then removes anything not a word character or character needed to create
## one of the profanity words/phrases in the profanity list.
preProfFilter <- function(samp, perl.flag=TRUE, is.news=FALSE) {
    # news file has char's that break gsub, so convert to ASCII and remove NA's
    if(is.news) {
        samp <- iconv(samp, from="UTF-8", to="ASCII")
        samp <- samp[-which(is.na(samp))]
    }
    # Handle right single quotes: 387317 instances in the blog file
    # http://stackoverflow.com/questions/2477452/%C3%A2%E2%82%AC%E2%84%A2-showing-on-page-instead-of
    samp <- gsub("(\xE2\x80\x99)", "'", samp, perl=perl.flag)
    # Handle other chars that are like single quotes:
    samp <- gsub("[\U0027\U00B4\U0092\U0060\U02BB\U02BC\U2018\U2019]",
                 "'", samp, perl=perl.flag)
    # Remove chars that can't be used to create profanity
    samp <- gsub("[^ A-Za-z0-9.!'_*+<>&@#()$\\^\\[\\]\\-]", "", samp, perl=perl.flag)
    return(samp)
}

## Converts all the text in flines to lower case (because profanity list is in lower case)
## and then removes all profanity in the flines character vector before returning it.
## Precondition - preProfFilter has been run on flines
removeProfanity <- function(profList, samp) {
    samp <- tolower(samp)  # profanity list is in lower case
    corp.tmp <- Corpus(VectorSource(samp))
    corp.tmp <- tm_map(corp.tmp, removeWords, profList)
    samp <- corp.tmp[[1]]$content
    # remove most variants of the all-time favorite profanity word which
    # weren't caught above
    samp <- gsub("(f+u+c+k+)", "", samp, ignore.case=TRUE, perl=TRUE)
    return(samp)
}

## Removes URLs and anything not a word, space, or basic punctuation character
## such as ?.!,:'- in a somewhat intelligent manner.
postProfClean <- function(samp, is.news=TRUE) {
    # Build regex to remove URLs. No shorthand character classes in R,
    # so need to create by hand
    wordChars <- "A-Za-z0-9_\\-"
    urlRegex <- sprintf("%s%s%s", "(http|https)(://)?[", wordChars, "]+")
    urlRegex <- sprintf("%s%s%s%s", urlRegex, "(.[", wordChars, "]+)+")
    urlRegex <- sprintf("%s%s%s%s", urlRegex, "[", wordChars, ".,@?^=%&:/~\\+#]*")
    # urlRegex <- "(http|https)://[\w\-_]+(\.[\w\-_]+)+[\w\-.,@?^=%&:/~\\+#]*"
    samp <- gsub(urlRegex, "", samp, perl=TRUE)
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
    # samp <- gsub("^.*[^.!?]$", "", samp)
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
    # remove terminating chars like punctuation etc.
    samp <- gsub("([^a-z]+)$", "", samp, ignore.case=TRUE, perl=TRUE)
    
    return(samp)
}

## Does additional cleaning for twitter and blog files
nonNewsPostProfClean <- function(flines) {
    cat("start nonNewsPostProfClean:", as.character(Sys.time()), "\n")
    # remove all lines that don't contain alpha char's
    flines.edit <- gsub("^[^a-z]$", "", flines, ignore.case=T, perl=T)
    flines.edit <- flines.edit[which(flines.edit != "")]
    # remove most variants of the all-time favorite profanity word which
    # weren't caught by profanity filter
    flines.edit <- gsub("(f+u+c+k+)", "", flines.edit,
                        ignore.case=TRUE, perl=TRUE)
    # remove 'omg' and 'wow'
    flines.edit <- gsub("(omg|o m g)", "", flines.edit, ignore.case=TRUE, perl=TRUE)
    flines.edit <- gsub("(wow|w o w)", "", flines.edit, ignore.case=TRUE, perl=TRUE)
    # remove trailing periods
    flines.edit <- gsub("^[.]+", "", flines.edit, ignore.case=TRUE, perl=TRUE)
    # remove embedded periods
    flines.edit <- gsub("([a-z]+)([.]+)([a-z]+)", "\\1 \\3", flines.edit,
                        ignore.case=TRUE, perl=TRUE)
    # replace space-period-space with just space
    flines.edit <- gsub(" [.] ", " ", flines.edit, ignore.case=TRUE, perl=TRUE)
    # remove periods assoc'd w/ morning and evening time abbrev's
    flines.edit <- gsub("(a m.)", "am", flines.edit,
                        ignore.case=TRUE, perl=TRUE)
    flines.edit <- gsub("(p m.)", "pm", flines.edit,
                        ignore.case=TRUE, perl=TRUE)
    # remove remaining www instances
    flines.edit <- gsub("[a-z][.][a-z]", " ", flines.edit,
                        ignore.case=TRUE, perl=TRUE)
    cat("finish nonNewsPostProfClean:", as.character(Sys.time()), "\n")
    
    return(flines.edit)
}

getBigramsStartingWithChars <- function(wrd1=NULL, wrd2=NULL,
                                        bigPath=bigPath) {
    bdata <- read.csv(bigPath, stringsAsFactors=FALSE)
    if(!is.null(wrd1)) {
        bdata <- filter(bdata, w1==wrd1 & w2 != "EOS" &
                            !grepl("^[^a-z]+$", w2))
    }
    if(!is.null(wrd2)) {
        bdata <- filter(bdata, w2==wrd2 & w2 != "EOS" &
                            !grepl("^[^a-z]+$", w2))
    }
    
    return(arrange(bdata, desc(freq)))
}

getTrigramsStartingWithChars <- function(wrd1=NULL, wrd2=NULL, wrd3=NULL,
                                         triPath=triPath) {
    tdata <- read.csv(triPath, stringsAsFactors=FALSE)
    if(!is.null(wrd1)) {
        tdata <- filter(tdata, w1==wrd1 & w3 != "EOS" &
                            !grepl("^[^a-z]+$", w3))
    }
    if(!is.null(wrd2)) {
        tdata <- filter(tdata, w2==wrd2 & w3 != "EOS" &
                            !grepl("^[^a-z]+$", w3))
    }
    if(!is.null(wrd3)) {
        tdata <- filter(tdata, w3==wrd3)
    }
    
    return(arrange(tdata, desc(freq)))
}

## Creates a horizontal bar plot of the words with the three highest
## probabilities
getPlot <- function(highest3=c('wisdom', 'health', 'pleasure'),
                    probabs=c(0.03, 0.02, 0.01)) {
    df <- data.frame(words=highest3, probs=probabs)
    
    p <- ggplot(df, aes(x=reorder(words, probs), weight=probs))
    p <- p + geom_bar() + coord_flip()
    p <- p + labs(x = 'word', y = 'probability')
    
    return(p)
}