# 
# libs <- c("dplyr", "readr", "stringr", "quanteda", "data.table")
# new.packages <- libs[!(libs %in% installed.packages()[,"Package"])]
# if(length(new.packages) > 0) install.packages(new.packages)
# lapply(libs, require, character.only=TRUE)
library(dplyr)
library(readr)
library(stringr)
library(quanteda)
library(data.table)
options(stringsAsFactors = FALSE)  # strings are what we are operating on...

## Returns a named vector of n-grams and their associated frequencies
## extracted from the character vector dat.
##
## ng - Defines the type of n-gram to be extracted: unigram if ng=1,
##      bigram if ng=2, trigram if n=3, etc.
## dat - Character vector from which we want to get n-gram counts.
## ignores - Character vector of words (features) to ignore from frequency table
## sort.by.ngram - sorts the return vector by the names
## sort.by.freq - sorts the return vector by frequency/count
getNgramFreqs <- function(ng, dat, ignores=NULL,
                          sort.by.ngram=TRUE, sort.by.freq=FALSE) {
    # http://stackoverflow.com/questions/36629329/
    # how-do-i-keep-intra-word-periods-in-unigrams-r-quanteda
    if(is.null(ignores)) {
        dat.dfm <- dfm(dat, ngrams=ng, toLower = FALSE, removePunct = FALSE,
                       what = "fasterword", verbose = FALSE)
    } else {
        dat.dfm <- dfm(dat, ngrams=ng, toLower = FALSE, ignoredFeatures=ignores,
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

## Returns a 2 column data.table. The first column: ngram, contains all the
## unigrams, bigrams, or trigrams in the corpus depending on whether
## ng = 1, 2, or 3 respectively. The second column: freq, contains the
## frequency or count of the ngram found in linesCorpus.
##
## ng - Defines the type of n-gram to be extracted: unigram if ng=1,
##      bigram if ng=2, trigram if n=3, etc.
## linesCorpus - character vector: each element is a line from a corpus file
## prefixFilter - character vector: If not NULL, tells the function to return
##                only rows where the ngram column starts with prefixFilter.
##                If NULL, returns all the ngram and count rows.
getNgramTables <- function(ng, linesCorpus, prefixFilter=NULL) {
    cat("START getNgramTables at", as.character(Sys.time()), "\n")
    ngrams <- getNgramFreqs(ng, linesCorpus)
    ngrams_dt <- data.table(ngram=names(ngrams), freq=ngrams)
    if(length(grep('^SOS', ngrams_dt$ngram)) > 0) {
        ngrams_dt <- ngrams_dt[-grep('^SOS', ngrams_dt$ngram),]
    }
    if(!is.null(prefixFilter)) {
        regex <- sprintf('%s%s', '^', prefixFilter)
        ngrams_dt <- ngrams_dt[grep(regex, ngrams_dt$ngram),]
    }
    
    cat("FINISH getNgramTables at", as.character(Sys.time()), "\n")
    return(ngrams_dt)
}

## Returns a two column data.frame of observed trigrams that start with the
## bigram prefix (bigPre) in the first column named ngram and
## frequencies/counts in the second column named freq. If no observed trigrams
## that start with bigPre exist, an empty data.frame is returned.
##
## bigPre -  single-element char array of the form w2_w1 which are the first 
##           two words of the trigram we are predicting the tail word of
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency/count of each trigram.
getObsTrigs <- function(bigPre, trigrams) {
    trigs.winA <- data.frame(ngrams=vector(mode = 'character', length = 0),
                             freq=vector(mode = 'integer', length = 0))
    regex <- sprintf("%s%s%s", "^", bigPre, "_")
    trigram_indices <- grep(regex, trigrams$ngram)
    if(length(trigram_indices) > 0) {
        trigs.winA <- trigrams[trigram_indices, ]
    }
    
    return(trigs.winA)
}

## Returns a character vector which are the tail words of unobserved trigrams
## that start with the first two words of obsTrigs (aka the bigram prefix).
## These are the words w in the set B(w_i-2, w_i-1) as defined in the section
## describing the details of equation 17.
##
## obsTrigs - character vector of observed trigrams delimited by _ of the form:
##            w3_w2_w1 where w3_w2 is the bigram prefix
## unigs - 2 column data.frame of all the unigrams in the corpus:
##         ngram = unigram
##         freq = frequency/count of each unigram
getUnobsTrigTails <- function(obsTrigs, unigs) {
    obs_trig_tails <- str_split_fixed(obsTrigs, "_", 3)[, 3]
    unobs_trig_tails <- unigs[!(unigs$ngram %in% obs_trig_tails), ]$ngram
    return(unobs_trig_tails)
}

## Returns a two column data.frame of observed trigrams that start with bigram
## prefix bigPre in the first column named ngram and the probabilities
## q_bo(w_i | w_i-2, w_i-1) in the second column named prob calculated from
## eqn 12. If no observed trigrams starting with bigPre exist, NULL is returned.
##
## obsTrigs - 2 column data.frame or data.table. The first column: ngram,
##            contains all the observed trigrams that start with the bigram
##            prefix bigPre which we are attempting to the predict the next
##            word of in a give phrase. The second column: freq, contains the
##            frequency/count of each trigram.
## bigrs - 2 column data.frame or data.table. The first column: ngram,
##         contains all the bigrams in the corpus. The second column:
##         freq, contains the frequency/count of each bigram.
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## triDisc - amount to discount observed trigrams
getObsTriProbs <- function(obsTrigs, bigrs, bigPre, triDisc=0.5) {
    if(nrow(obsTrigs) < 1) return(NULL)
    obsCount <- filter(bigrs, ngram==bigPre)$freq[1]
    obsTrigProbs <- mutate(obsTrigs, freq=((freq - triDisc) / obsCount))
    colnames(obsTrigProbs) <- c("ngram", "prob")
    
    return(obsTrigProbs)
}

## Returns a character vector of backed off bigrams of the form w2_w1. These 
## are all the (w_i-1, w) bigrams where w_i-1 is the tail word of the bigram
## prefix bigPre and w are the tail words of unobserved bigrams that start with
## w_i-1.
##
## bigPre - single-element char array of the form w2_w1 which are first two
##          words of the trigram we are predicting the tail word of
## unobsTrigTails - character vector that are tail words of unobserved trigrams
getBoBigrams <- function(bigPre, unobsTrigTails) {
    w_i_minus1 <- str_split(bigPre, "_")[[1]][2]
    boBigrams <- paste(w_i_minus1, unobsTrigTails, sep = "_")
    return(boBigrams)
}

## Returns a two column data.frame of backed-off bigrams in the first column
## named ngram and their frequency/counts in the second column named freq.
## 
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## unobsTrigTails - character vector that are tail words of unobserved trigrams
## bigrs - 2 column data.frame or data.table. The first column: ngram,
##         contains all the bigrams in the corpus. The second column:
##         freq, contains the frequency/count of each bigram.
getObsBoBigrams <- function(bigPre, unobsTrigTails, bigrs) {
    boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
    obs_bo_bigrams <- bigrs[bigrs$ngram %in% boBigrams, ]
    return(obs_bo_bigrams)
}

## Returns a character vector of backed-off bigrams which are unobserved.
##
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## unobsTrigTails - character vector that are tail words of unobserved trigrams
## obsBoBigram - data.frame which contains the observed bigrams in a column
##               named ngram
getUnobsBoBigrams <- function(bigPre, unobsTrigTails, obsBoBigram) {
    boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
    unobs_bigs <- boBigrams[!(boBigrams %in% obsBoBigram$ngram)]
    return(unobs_bigs)
}

## Returns a dataframe of 2 columns: ngram and probs.  Values in the ngram
## column are bigrams of the form: w2_w1 which are observed as the last
## two words in unobserved trigrams.  The values in the prob column are
## q_bo(w1 | w2) calculated from from equation 10.
##
## obsBoBigrams - a dataframe with 2 columns: ngram and freq. The ngram column
##                contains bigrams of the form w1_w2 which are observed bigrams
##                that are the last 2 words of unobserved trigrams (i.e. "backed
##                off" bigrams). The freq column contains integers that are
##                the counts of these observed bigrams in the corpus.
## unigs - 2 column data.frame of all the unigrams in the corpus:
##         ngram = unigram
##         freq = frequency/count of each unigram
## bigDisc - amount to discount observed bigrams
getObsBigProbs <- function(obsBoBigrams, unigs, bigDisc=0.5) {
    first_words <- str_split_fixed(obsBoBigrams$ngram, "_", 2)[, 1]
    first_word_freqs <- unigs[unigs$ngram %in% first_words, ]
    obsBigProbs <- (obsBoBigrams$freq - bigDisc) / first_word_freqs$freq
    obsBigProbs <- data.frame(ngram=obsBoBigrams$ngram, prob=obsBigProbs)
    
    return(obsBigProbs)
}


## Returns the total probability mass discounted from all observed bigrams
## calculated from equation 14.  This is the amount of probability mass which
## is redistributed to UNOBSERVED bigrams. If no bigrams starting with
## unigram$ngram[1] exist, 0 is returned.
##
## unigram - single row, 2 column frequency table. The first column: ngram,
##           contains the w_i-1 unigram (2nd word of the bigram prefix). The
##           second column: freq, contains the frequency/count of this unigram.
## bigrams - 2 column data.frame or data.table. The first column: ngram,
##           contains all the bigrams in the corpus. The second column:
##           freq, contains the frequency or count of each bigram.
## bigDisc - amount to discount observed bigrams
getAlphaBigram <- function(unigram, bigrams, bigDisc=0.5) {
    # get all bigrams that start with unigram
    regex <- sprintf("%s%s%s", "^", unigram$ngram[1], "_")
    bigsThatStartWithUnig <- bigrams[grep(regex, bigrams$ngram),]
    if(nrow(bigsThatStartWithUnig) < 1) return(0)
    alphaBi <- 1 - (sum(bigsThatStartWithUnig$freq - bigDisc) / unigram$freq)
    
    return(alphaBi)
}

## Returns a dataframe of 2 columns: ngram and prob.  Values in the ngram
## column are unobserved bigrams of the form: w2_w1.  The values in the prob
## column are the backed off probability estimates q_bo(w1 | w2) calculated
## from from equation 16.
##
## unobsBoBigrams - character vector of unobserved backed off bigrams
## unigs - 2 column data.frame of all the unigrams in the corpus:
##         ngram = unigram
##         freq = frequency/count of each unigram
## alphaBig - total discounted probability mass at the bigram level
getQboUnobsBigrams <- function(unobsBoBigrams, unigs, alphaBig) {
    # get the unobserved bigram tails
    qboUnobsBigs <- str_split_fixed(unobsBoBigrams, "_", 2)[, 2]
    w_in_Aw_iminus1 <- unigs[!(unigs$ngram %in% qboUnobsBigs), ]
    # convert to data.frame with counts
    qboUnobsBigs <- unigs[unigs$ngram %in% qboUnobsBigs, ]
    denom <- sum(qboUnobsBigs$freq)
    # converts counts to probabilities
    qboUnobsBigs <- data.frame(ngram=unobsBoBigrams,
                               prob=(alphaBig * qboUnobsBigs$freq / denom))
    
    return(qboUnobsBigs)
}

## Returns the total probability mass discounted from all observed trigrams.
## calculated from equation 14. This is the amount of probability mass which is
## redistributed to UNOBSERVED trigrams. If no trigrams starting with
## bigram$ngram[1] exist, 0 is returned.
##
## obsTrigs - 2 column data.frame or data.table. The first column: ngram,
##            contains all the observed trigrams that start with the bigram
##            prefix we are attempting to the predict the next word of. The 
##            second column: freq, contains the frequency/count of each trigram.
## bigram - single row frequency table where the first col: ngram, is the bigram
##          which are the first two words of unobserved trigrams we want to
##          estimate probabilities of (same as bigPre in other functions listed
##          prior) delimited with '_'. The second column: freq, is the
##          frequency/count of the bigram listed in the ngram column.
## triDisc - amount to discount observed trigrams
getAlphaTrigram <- function(obsTrigs, bigram, triDisc=0.5) {
    if(nrow(obsTrigs) < 1) return(0)
    alphaTri <- 1 - sum((obsTrigs$freq - triDisc) / bigram$freq[1])
    
    return(alphaTri)
}

## Returns a dataframe of 2 columns: ngram and prob.  Values in the ngram
## column are unobserved trigrams of the form: w3_w2_w1.  The values in the prob
## column are q_bo(w1 | w3, w2) calculated from equation 17.
##
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## qboObsBigrams - 2 column data.frame with the following columns -
##                 ngram: observed bigrams of the form w2_w1
##                 probs: the probability estimate for observed bigrams:
##                        qbo(w1 | w2) calc'd from equation 10.
##                 trigrams that start with bigPre
## qboUnobsBigrams - 2 column data.frame with the following columns -
##                   ngram: unobserved bigrams of the form w2_w1
##                   probs: the probability estimate for unobserved bigrams
##                          qbo(w1 | w2) calc'd from equation 16.
## alphaTrig - total discounted probability mass at the trigram level
getUnobsTriProbs <- function(bigPre, qboObsBigrams,
                             qboUnobsBigrams, alphaTrig) {
    qboBigrams <- rbind(qboObsBigrams, qboUnobsBigrams)
    qboBigrams <- qboBigrams[order(-qboBigrams$prob), ]
    sumQboBigs <- sum(qboBigrams$prob)
    first_bigPre_word <- str_split(bigPre, "_")[[1]][1]
    unobsTrigNgrams <- paste(first_bigPre_word, qboBigrams$ngram, sep="_")
    unobsTrigProbs <- alphaTrig * qboBigrams$prob / sumQboBigs
    unobsTrigDf <- data.frame(ngram=unobsTrigNgrams, prob=unobsTrigProbs)
    
    return(unobsTrigDf)
}
