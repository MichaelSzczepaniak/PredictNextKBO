rm(list = ls())
source("Katz2.R")


# 1, 2 select corpus, discount rates, and n
### litte test corpus
# use_corpus <- readLines("../data/little_test_corpus1.txt")
# unigs <- getNgramTables(1, use_corpus)
# bigrs <- getNgramTables(2, use_corpus)
# trigs <- getNgramTables(3, use_corpus)
# bigPre <- "sell_the"
### blogs corpus
corpType <- "blogs"
dataPrefix <- "en_US."
dataPostfix <- c(".train.12unigrams.nosins.csv",
                 ".train.13bigrams.nosins.csv",
                 ".train.14trigrams.nosins.csv")
dataDir <- "D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/"
ngramPaths <- sprintf("%s%s%s%s",
                      dataDir, dataPrefix, corpType, dataPostfix)

unigs <- read.csv(ngramPaths[1])
bigrs <- read.csv(ngramPaths[2]) # about 4 sec
trigs <- read.csv(ngramPaths[3]) # about 8 sec

bigPre <- "a_baby"

gamma3 <- 0.5  # trigram discount
gamma2 <- 0.5  # bigram discount
top_n <- 3
# gather bigrams for denom of eqn 17
obs_trigs <- getObsTrigs(bigPre, trigs)
unobs_trig_tails <- getUnobsTrigTails(obs_trigs$ngram, unigs)
bo_bigrams <- getBoBigrams(bigPre, unobs_trig_tails)
# separate bigrams which use eqn 10 and those that use 16
obs_bo_bigrams <- getObsBoBigrams(bigPre, unobs_trig_tails, bigrs)
unobs_bo_bigrams <- getUnobsBoBigrams(bigPre, unobs_trig_tails, obs_bo_bigrams)
# unobs_bo_bigrams = c("the_buy", "the_EOS", "the_paint", "the_sell", "the_the")
# calc obs'd bigram prob's from eqn 10
qbo_obs_bigrams <- getObsBigProbs(obs_bo_bigrams, unigs, gamma2) #ngram     probs
# calc alpha_big & unobs'd bigram prob's from eqn 16             #the_house 0.3125
unig <- str_split(bigPre, "_")[[1]][2]
unig <- unigs[unigs$ngram == unig,]
alpha_big <- getAlphaBigram(unig, bigrs, gamma2)
# distrib discounted bigram prob mass to unobs bigrams in prop to unigram ML
qbo_unobs_bigrams <- getQboUnobsBigrams(unobs_bo_bigrams, unigs, alpha_big)
# calc trigram probabilities - start with observed trigrams: eqn 12
qbo_obs_trigrams <- getObsTriProbs(obs_trigs, bigrs, bigPre, gamma3)
# finally, calc trigram unobserved probabilities: eqn 17
bigram <- bigrs[bigrs$ngram %in% bigPre, ]
alpha_trig <- getAlphaTrigram(obs_trigs, bigram, gamma3)
qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
                                       qbo_unobs_bigrams, alpha_trig)
qbo_trigrams <- rbind(qbo_obs_trigrams, qbo_unobs_trigrams)
qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]
sum(qbo_trigrams$prob)
