#
source('Katz.R')
options(stringsAsFactors = FALSE)  # strings are what we are operating on...
# Use next 3 lines for development
uniPaths <- "../data/ltc1_unigrams.csv"
bigPaths <- "../data/ltc1_bigrams.csv"
triPaths <- "../data/ltc1_trigrams.csv"

# Use the next 3 lines for live deployement
# uniPaths <- c("./data/en_US.blogs.train.12unigrams.nosins.csv",
#               "./data/en_US.news.train.12unigrams.nosins.csv",
#               "./data/en_US.twitter.train.12unigrams.nosins.csv")
# bigPaths <- c("./data/en_US.blogs.train.13bigrams.nosins.csv",
#              "./data/en_US.news.train.13bigrams.nosins.csv",
#              "./data/en_US.twitter.train.13bigrams.nosins.csv")
# triPaths <- c("./data/en_US.blogs.train.14trigrams.nosins.csv",
#              "./data/en_US.news.train.14trigrams.nosins.csv",
#              "./data/en_US.twitter.train.14trigrams.nosins.csv")

# default parameters for bigram and trigram discount rates
# gamma2 <- 0.5
# gamma3 <- 0.5

# read n-gram tables upfront so they can be passed to the Katz.R functions
unigrams <- read.csv(uniPaths[1])
bigrams <- read.csv(bigPaths[1])
trigrams <- read.csv(triPaths[1])

getSettings <- function(corpus, bigDisc=0.1, trigDisc=0.2) {
    corpusLabels <- c("blogs", "news", "twitter")
    corpus <- as.numeric(corpus)
    result <- sprintf("%s%s%s", "corpus=", corpusLabels[corpus], ", ")
    result <- sprintf("%s%s%s%s", result, "bigram discount=", bigDisc, ", ")
    result <- sprintf("%s%s%s", result, "trigram discount=", trigDisc)
    
    return(result)
}

getInputBigram <- function(inputPhrase) {
    bigram_tail = ""
    inPh <- filterInput(inputPhrase)
    inputTokens <- str_split(inPh, " ")
    if(length(inputTokens[1]) > 0) {
        lastIndex <- length(inputTokens[[1]])
        w1 <- inputTokens[[1]][(lastIndex-1)]
        w2 <- inputTokens[[1]][(lastIndex)]
        bigram_tail <- paste(w1, w2, sep="_")
    }
    
    return(bigram_tail)
}

## Returns a data.frame of 2 columns: ngram and prob with n rows. Value in the 
## ngram column are words that complete the highest probability trigrams using
## the KBO Trigram alogrithm.  The values in the prob column are
## q_bo(w1 | w3, w2) calculated from either eqn 12 if w3_w2_w1 is observed
## or eqn 17 if w3_w2_w1 is not observed.
##
## bigPre - last 2 words of user input separated by an _ e.g. sell_the
##          This is also referred to as the bigram prefix in code futher
##          downstream.
## n - number of predictions to return, default is 3
## gamma2 - bigram discount rate
## gamma3 - trigram discount rate
getTopNPredictions <- function(bigPre, n=3, gamma2=1.0, gamma3=1.0) {
    obs_trigs <- getObsTrigs(bigPre, trigrams)
    unobs_trig_tails <- getUnobsTrigTails(obs_trigs$ngram, unigrams)
    bo_bigrams <- getBoBigrams(bigPre, unobs_trig_tails)
    # separate bigrams which use eqn 10 and those that use 16
    obs_bo_bigrams <- getObsBoBigrams(bigPre, unobs_trig_tails, bigrams)
    unobs_bo_bigrams <- getUnobsBoBigrams(bigPre, unobs_trig_tails,
                                          obs_bo_bigrams)
    # calc obs'd bigram prob's from eqn 10
    qbo_obs_bigrams <- getObsBigProbs(obs_bo_bigrams, unigrams, gamma2)
    # calc alpha_big & unobs'd bigram prob's from eqn 16
    unig <- str_split(bigPre, "_")[[1]][2]
    unig <- unigrams[unigrams$ngram == unig,]
    alpha_big <- getAlphaBigram(unig, bigrams, gamma2)
    qbo_unobs_bigrams <- getQboUnobsBigrams(unobs_bo_bigrams, unigrams, alpha_big)
    # calc trigram probabilities - start with observed trigrams: eqn 12
    qbo_obs_trigrams <- getObsTriProbs(obs_trigs, bigrams, bigPre, gamma3)
    # finally, calc trigram unobserved probabilities: eqn 17
    bigram <- bigrams[bigrams$ngram %in% bigPre, ]
    alpha_trig <- getAlphaTrigram(obs_trigs, bigram, gamma3)
    qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
                                           qbo_unobs_bigrams, alpha_trig)
    qbo_trigrams <- rbind(qbo_obs_trigrams, qbo_unobs_trigrams)
    qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]
    
    return(qbo_trigrams[1:n,])
}

getPrediction <- function(topPreds) {
    prediction <- ""
    if(length(topPreds$ngram) > 0) {
        predict_trigram <- topPreds$ngram[1]  # highest probability trigram
        # Get tail word of the highest probability trigram
        prediction <- str_split(predict_trigram, '_')[[1]][3]
    }
    
    return(prediction)
}

## TODO
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
## trigram tail word probabilities
## topTrigrams - character vector of trigrams delimited by _
##             e.g. tom_loves_sushi
## topProbs - probabilities associated with each of the trigram tail words
getPlot <- function(topTrigrams=c('to_love_wisdom', 'to_cultivate_health',
                                'enjoy_healthy_pleasure'),
                    topProbs=c(0.03, 0.02, 0.01)) {
    require(ggplot2)
    words <- vector(mode = 'character')
    ngramTokens <- str_split(topTrigrams, '_')
    for(i in 1:length(ngramTokens)) {
        words <- append(words, ngramTokens[[i]][3])
    }
    
    df <- data.frame(words, probs=topProbs)
    
    p <- ggplot(df, aes(x=reorder(words, probs), weight=probs))
    p <- p + geom_bar() + coord_flip()
    p <- p + labs(x = 'predicted word', y = 'probability')
    
    return(p)
}