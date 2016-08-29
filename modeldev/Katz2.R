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
## bigramPrefix - single element character vector which is a bigram of the
##                form: word1_word2 e.g. sell_the
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
getObsTrigs <- function(bigramPrefix, trigrams) {
    trigs.winA <- data.frame(ngrams=vector(mode = 'character', length = 0),
                             freq=vector(mode = 'integer', length = 0))
    regex <- sprintf("%s%s", "^", bigramPrefix)
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
## bigramPrefix - single-element char array of the form w2_w1
## trigrams - 2 column data.frame or data.table. The first column: ngram,
##            contains all the trigrams in the corpus. The second column:
##            freq, contains the frequency or count of each trigram.
## triDiscount - amount to discount observed trigrams
getObsTriProbs <- function(bigramPrefix, trigrams, triDiscount=0.5) {
    obsTrigsA <- getObsTrigs(bigramPrefix, trigrams)
    if(nrow(obsTrigsA) < 1) return(NULL)
    obsCount <- sum(obsTrigsA$freq)
    probs <- (obsTrigsA$freq - triDiscount) / obsCount
    qBO.A <- data.table(ngram=obsTrigsA$ngram, prob=probs)
    
    return(qBO.A)
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

## Returns a two column data.frame of backed-off bigrams which are observed
## 
## bigPre -  single-element char array of the form w2_w1 which are first two
##           words of the trigram we are predicting the tail word of
## unobsTrigTails - character vector that are tail words of unobserved trigrams
## bigrs - 2 column data.frame or data.table. The first column: ngram,
##         contains all the bigrams in the corpus. The second column:
##         freq, contains the frequency or count of each bigram.
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
## column are bigrams of the form: word2_word1 which are observed as the last
## two words in .  The values in the probs
## columns are p(word1 | word2) calc'd from eqn. 10.
##
## obsBigTailFreqs - a dataframe with 2 columns: ngram and freq. The ngram
##                   column contains bigrams of the form w1_w2 which are 
##                   are observed bigrams that are the last 2 words of
##                   unobserved trigrams. The freq column contains integers that
##                   are the counts of these observed bigrams in the corpus.
## unigs - all the unigrams in the corpus
## bigDisc - bigram discount rate which should be between 0.01 and 1.99
getObsBigProbs <- function(obsBigTailFreqs, unigs, bigDisc=0.5) {
    first_words <- str_split_fixed(obsBigTailFreqs$ngram, "_", 2)[, 1]
    first_word_freqs <- unigs[unigs$ngram %in% first_words, ]
    obsBigProbs <- (obsBigTailFreqs$freq - bigDisc) / first_word_freqs$freq
    obsBigProbs <- data.frame(ngram=obsBigTailFreqs$ngram, probs=obsBigProbs)
    
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
    pass1 <- isTRUE(all.equal(obp$probs, (frq1 - d1)/c(7,13), tolerance=tol))
    
    return(pass1)
}



##########################################################

## Returns two column data.frame of observed bigrams in the first column: ngram
## and the frequency/count of those bigrams in the second column: freq
## 
## The ngram column are bigrams delimited by _. E.g. w1_w2
##
## bigramPrefix - This is the first two words from which we define the set of 
##                unobserved trigrams.
## trigs - all the trigrams in the corpus
## bigrs - all the bigrams in the corpus
## unigs - all the unigrams in the corpus
getObsBigrams <- function(bigramPrefix, trigs, bigrs, unigs) {
    unobsTrigs <- getUnobsTrigs(bigPre, trigs, unigs) # get unobs'd trigrams
    big_tails <- str_split_fixed(unobsTrigs, "_", 3)[,2:3] # get bigram tails
    big_tails <- paste(big_tails[,1], big_tails[,2], sep = "_")
    # observed bigram tales will have an entry in bigs table
    return(bigrs[bigrs$ngram %in% big_tails, ])
}





## Returns a dataframe of 2 columns: ngram and probs...
##
## alpha_big - discounted probability mass at the bigram level calculated
##             from eqn 14.
## obs_big_tail_words - 
## unigs - all the unigrams in the corpus
getUnObsBigProbs <- function(alpha_big, obs_big_tail_words, unigs) {
    obs_tail_words <- str_split_fixed(obs_big_tail_words$ngram, "_", 2)[, 2]
    obs_tail_word_freqs <- unigs[unigs$ngram %in% obs_tail_words, ]
    denom <- sum(unigs$freq) - sum(obs_tail_word_freqs$freq)
    unobs_big_tail_words <- setdiff(unigs$ngram, obs_big_tail_words$ngram)
    unobs_big_tail_freqs <- unigs[unigs$ngram %in% unobs_big_tail_words, ]
    unobs_big_tail_probs <- alpha_big * unobs_big_tail_freqs$ngram / denom
    unobs_big_tail_probs <- data.frame(ngram = unobs_big_tail_freqs$ngram,
                                       probs = unobs_big_tail_probs)
    
    return(unobs_big_tail_probs)
}