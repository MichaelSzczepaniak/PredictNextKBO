#
source('Katz.R')
options(stringsAsFactors = FALSE)  # strings are what we are operating on...

# Setup paths to the ngram frequency data used by the model
uniPaths <- c("./data/en_US.blogs.train.12unigrams.nosins.csv",
              "./data/en_US.news.train.12unigrams.nosins.csv",
              "./data/en_US.twitter.train.12unigrams.nosins.csv")
bigPaths <- c("./data/en_US.blogs.train.13bigrams.nosins.csv",
             "./data/en_US.news.train.13bigrams.nosins.csv",
             "./data/en_US.twitter.train.13bigrams.nosins.csv")
triPaths <- c("./data/en_US.blogs.train.14trigrams.nosins.csv",
             "./data/en_US.news.train.14trigrams.nosins.csv",
             "./data/en_US.twitter.train.14trigrams.nosins.csv")

## Gets the user settings to use for the model prediction, sets model parameters
## and loads the ngram frequency tables corresponding to the corpus to use
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
    inPh <- inputPhrase #filterInput(inputPhrase)
    inputTokens <- str_split(inPh, " ")
    if(length(inputTokens[1]) > 0) {
        lastIndex <- length(inputTokens[[1]])
        w1 <- inputTokens[[1]][(lastIndex-1)]
        w2 <- inputTokens[[1]][(lastIndex)]
        bigram_tail <- paste(w1, w2, sep="_")
    }
    
    return(bigram_tail)
}

## Returns a data.frame with n rows and 3 columns: ngram, prob, path.  Values
## in the ngram column are words that complete the highest probability trigrams
## using the KBO Trigram alogrithm.  The values in the prob column are
## q_bo(w1 | w3, w2) calculated from either eqn 12 if w3_w2_w1 is observed
## or eqn 17 if w3_w2_w1 is not observed.  Values in the path column describe
## the path through the KBO Trigram algorithm used to compute the probability
## of the predicted word.  These values will only be one of three possible
## strings: "observed trigram", "observed bigram", or "unobserved bigram".
##
## bigPre - last 2 words of user input separated by an _ e.g. sell_the
##          This is also referred to as the bigram prefix in code futher
##          downstream.
## n - number of predictions to return, default is 3
## corp_index - integer corresponding to corpus selected by user:
##              1 for blogs, 2 for news, 3 for twitter
## gamma2 - bigram discount rate
## gamma3 - trigram discount rate
getTopNPredictions <- function(bigPre, n=3, corp_index, gamma2, gamma3,
                               allowEOS) {
    # load unigram, bigram, and trigram tables corresponding to the corpus
    # selected by the user
    unigrams <- read.csv(uniPaths[corp_index])
    bigrams <- read.csv(bigPaths[corp_index])
    trigrams <- read.csv(triPaths[corp_index])
    
    # extracted observed trigrams from trigram table
    obs_trigs <- getObsTrigs(bigPre, trigrams)
    # get character vector of unobserved trigram tail words
    unobs_trig_tails <- getUnobsTrigTails(obs_trigs$ngram, unigrams)
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
    qbo_unobs_bigrams <- getUnobsBigProbs(unobs_bo_bigrams, unigrams, alpha_big)
    # calc trigram probabilities - start with observed trigrams: eqn 12
    qbo_obs_trigrams <- getObsTriProbs(obs_trigs, bigrams, bigPre, gamma3)
    # add column path column for observed trigrams
    # finally, calc trigram unobserved probabilities: eqn 17
    bigram <- bigrams[bigrams$ngram %in% bigPre, ]
    alpha_trig <- getAlphaTrigram(obs_trigs, bigram, gamma3)
    qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
                                           qbo_unobs_bigrams, alpha_trig)
    # add column path column for unobserved trigrams
    qbo_trigrams <- rbind(qbo_obs_trigrams, qbo_unobs_trigrams)
    # sort predictions in descending order
    qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]
    # remove EOS predictions if user wants them removed
    if(!allowEOS) {
        n <- n + 1
        tmp <- str_split_fixed(qbo_trigrams[1:n,]$ngram, '_', 3)
        eos_index <- grep('EOS', tmp[,3])
        if(length(eos_index) > 0) {
            qbo_trigrams <- qbo_trigrams[-eos_index,]
        }
        n <- n - 1
    }
    
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

getPredictFrom <- function(top_preds) {
    return(top_preds$path[1])
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
    
    p <- ggplot(df, aes(x=reorder(words, probs), y=probs))
    p <- p + geom_bar(stat="identity", fill="#996633", colour="black") +
        coord_flip()
    p <- p + labs(x = 'predicted word', y = 'probability')
    
    return(p)
}