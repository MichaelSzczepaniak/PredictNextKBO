# COMMENT THE NEXT 4 LINES BEFORE DEPLOYMENT BECAUSE SHINY IMPORTS WHAT IT NEEDS
libs <- c("dplyr", "readr", "stringr", "quanteda", "data.table")
new.packages <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
lapply(libs, require, character.only=TRUE)
options(stringsAsFactors = FALSE)  # strings are what we are operating on...

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

## Returns a 2 column data.table. The first column: ngram, contains all the
## unigrams, bigrams, or trigrams in the corpus depending on whether
## ng = 1, 2, or 3 respectively. The second column: freq, contains the
## frequency or count of the ngram found in linesCorpus.
##
## ng - Defines the type of n-gram to be extracted: unigram if ng=1,
##      bigram if ng=2, trigram if n=3, etc.
## linesCorpus - character vector which is a line from a corpus file
## prefixFilter - character vector: If not NULL, tells the function to return
##                only rows where the ngram column starts with prefixFilter.
##                If NULL, returns all the ngram and count rows.
getNgramTables <- function(ng, linesCorpus, prefixFilter=NULL) {
    ngrams <- getNgramFreqs(ng, linesCorpus)
    ngrams.dt <- data.table(ngram=names(ngrams), freq=ngrams)
    if(length(grep('^SOS', ngrams.dt$ngram)) > 0) {
        ngrams.dt <- ngrams.dt[-grep('^SOS', ngrams.dt$ngram),]
    }
    if(!is.null(prefixFilter)) {
        regex <- sprintf('%s%s', '^', prefixFilter)
        ngrams.dt <- ngrams.dt[grep(regex, ngrams.dt$ngram),]
    }
    
    return(ngrams.dt)
}

## Returns a two column data.frame of observed trigrams that start with
## bigramPrefix in the first column named ngram and frequencies/counts in the 
## second column named freq. If no observed trigrams with bigramPrefix exist,
## an empty data.frame is returned.
##
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
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
## that start with the first two words of obsTrigs.
##
## obsTrigs - character vector of observed trigrams delimited by _ of the form:
##            w3_w2_w1
## unigs - 2 column data.frame of all the unigrams in the corpus:
##         ngram = unigram
##         freq = frequency/count of each unigram
getUnobsTrigTails <- function(obsTrigs, unigs) {
    obs_trig_tails <- str_split_fixed(obsTrigs, "_", 3)[, 3]
    unobs_trig_tails <- unigs[!(unigs$ngram %in% obs_trig_tails), ]$ngram
    return(unobs_trig_tails)
}

## Returns a two column data.frame of observed trigrams that start with
## bigramPrefix in the first column named ngram and the probabilities
## qbo(w_i | w_i-2, w_i-1) in the second column named probs. If no observed
## trigrams with bigramPrefix exist, NULL returned.
##
## The first column of the datatable are the trigrams corresponding to the
## probability estimate that are in the second column.  If no observed trigrams
## exist, returns NULL.
##
## obsTrigs - 2 column data.frame or data.table. The first column: ngram,
##            contains all the observed trigrams that start with the bigram
##            prefix we are attempting to the predict the next word ofin the
##            corpus. The second column: freq, contains the frequency or count
##            of each trigram.
## bigrs - 2 column data.frame or data.table. The first column: ngram,
##         contains all the bigrams in the corpus. The second column:
##         freq, contains the frequency/count of each bigram.
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## triDiscount - amount to discount observed trigrams
getObsTriProbs <- function(obsTrigs, bigrs, bigPre, triDiscount=0.5) {
    if(nrow(obsTrigs) < 1) return(NULL)
    obsCount <- filter(bigrs, ngram==bigPre)$freq[1]
    obsTrigProbs <- mutate(obsTrigs, freq=((freq - triDiscount) / obsCount))
    colnames(obsTrigProbs) <- c("ngram", "prob")
    
    return(obsTrigProbs)
}

## Returns a character vector of bigrams of the form w2_w1. These are all the
## (w_i-1, w) bigrams where the w_i-1 is the tail word of the bigram prefix
## which we are trying to complete and w are the tail words of unobserved
## bigrams that start with bigram prefix
##
## bigPre - single-element char array of the form w2_w1 which are first two
##          words of the trigram we are predicting the tail word of
## unobsTrigTails - character vector that are tail words of unobserved trigrams
getBoBigrams <- function(bigPre, unobsTrigTails) {
    w_i_minus1 <- str_split(bigPre, "_")[[1]][2]
    boBigrams <- paste(w_i_minus1, unobsTrigTails, sep = "_")
    return(boBigrams)
}

## Returns a two column data.frame of backed-off bigrams (ngram column) and
## their frequency/counts (freq column).
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
## obsBoBigram -
getUnobsBoBigrams <- function(bigPre, unobsTrigTails, obsBoBigram) {
    boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
    unobs_bigs <- boBigrams[!(boBigrams %in% obsBoBigram$ngram)]
    return(unobs_bigs)
}

## Returns a dataframe of 2 columns: ngram and probs.  Values in the ngram
## column are bigrams of the form: w2_w1 which are observed as the last
## two words in .  The values in the prob column are p(w1 | w2)
## calculated from from equation 10.
##
## obsBigTailFreqs - a dataframe with 2 columns: ngram and freq. The ngram
##                   column contains bigrams of the form w1_w2 which are 
##                   are observed bigrams that are the last 2 words of
##                   unobserved trigrams. The freq column contains integers that
##                   are the counts of these observed bigrams in the corpus.
## unigs - 2 column data.frame of all the unigrams in the corpus:
##         ngram = unigram
##         freq = frequency/count of each unigram
## bigDisc - bigram discount rate which should be between 0.01 and 1.99
getObsBigProbs <- function(obsBigTailFreqs, unigs, bigDisc=0.5) {
    first_words <- str_split_fixed(obsBigTailFreqs$ngram, "_", 2)[, 1]
    first_word_freqs <- unigs[unigs$ngram %in% first_words, ]
    obsBigProbs <- (obsBigTailFreqs$freq - bigDisc) / first_word_freqs$freq
    obsBigProbs <- data.frame(ngram=obsBigTailFreqs$ngram, prob=obsBigProbs)
    
    return(obsBigProbs)
}

## Tests the getObsBigProbs function
testGetObsBigProbs <- function(d1=0.5) {
    ng1 = c("the_house", "the_book"); frq1 = c(3, 5)
    obt <- data.frame(ngram=ng1, freq=frq1)
    ng2 = c("house", "car", "spoon", "book"); frq2 = c(7,9,11,13)
    ungs <- data.frame(ngram=ng2, freq=frq2)
    obp <- getObsBigProbs(obt, ungs, d1)
    tol = .Machine$double.eps ^ 0.5
    pass1 <- isTRUE(all.equal(obp$prob, (frq1 - d1)/c(7,13), tolerance=tol))
    
    return(pass1)
}

## Returns the total probability mass discounted from all observed bigrams
## calculated from equation 14.  This is the amount of probability mass which
## is redistributed to UNOBSERVED bigrams. If no bigrams starting with
## unigram$ngram[1] exist, NULL is returned.
##
## unigram - single row, 2 column frequency table. The first column: ngram,
##           contains the w_i-1 unigram (2nd word of the bigram prefix). The
##           second column: freq, contains the frequency/count of this unigram.
## bigrams - 2 column data.frame or data.table. The first column: ngram,
##           contains all the bigrams in the corpus. The second column:
##           freq, contains the frequency or count of each bigram.
## bigDiscount - amount to discount observed bigrams
getAlphaBigram <- function(unigram, bigrams, bigDiscount=0.5) {
    # get all bigrams that start with unigram
    regex <- sprintf("%s%s%s", "^", unigram$ngram[1], "_")
    bigsThatStartWithUnig <- bigrams[grep(regex, bigrams$ngram),]
    if(nrow(bigsThatStartWithUnig) < 1) return(NULL)
    alphaBi <- 1 - (sum(bigsThatStartWithUnig$freq - bigDiscount) /
                        unigram$freq)
    return(alphaBi)
}

## Returns a dataframe of 2 columns: ngram and prob.  Values in the ngram
## column are unobserved bigrams of the form: w2_w1.  The values in the prob
## column are p(w1 | w2) calculated from from equation 16.
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
    # convert
    qboUnobsBigs <- unigs[unigs$ngram %in% qboUnobsBigs, ]
    denom <- sum(qboUnobsBigs$freq)
    qboUnobsBigs <- data.frame(ngram=unobsBoBigrams,
                               prob=(alphaBig * qboUnobsBigs$freq / denom))
    
    return(qboUnobsBigs)
}

## Returns the total probability mass discounted from all observed trigrams.
## calculated from equation 14. This is the amount of probability mass which is
## redistributed to UNOBSERVED trigrams. If no trigrams starting with
## bigram$ngram[1] exist, NULL is returned.
##
## obsTrigs - 2 column data.frame or data.table. The first column: ngram,
##            contains all the observed trigrams that start with the bigram
##            prefix we are attempting to the predict the next word ofin the
##            corpus. The second column: freq, contains the frequency or count
##            of each trigram.
## bigram - single row frequency table where the first col: ngram, is the bigram
##          which are the first two words of unobserved trigrams we want to
##          estimate probabilities of (same as bigramPrefix in previous functions
##          listed above) delimited with '_'. The second column: freq, is the
##          frequency/count of the bigram listed in the 1st column.
## triDiscount - amount to discount observed trigrams
getAlphaTrigram <- function(obsTrigs, bigram, triDiscount=0.5) {
    if(nrow(obsTrigs) < 1) return(NULL)
    alphaTri <- 1 - sum((obsTrigs$freq - triDiscount) / bigram$freq[1])
    
    return(alphaTri)
}

##
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## unobsTrigTails - character vector which are the tail words of unobserved
##                  trigrams that start with bigPre
## qboUnobsBigrams - 2 column data.frame -
##                   ngram: unobserved bigrams of the form w2_w1
##                   probs: the probability estimate for unobserved bigrams
##                          calc'd from equation 16.
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