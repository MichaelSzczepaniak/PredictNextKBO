rm(list = ls())
source("Katz2.R")


# 1, 2 select corpus, discount rates, and n
ltcorpus <- readLines("../data/little_test_corpus1.txt")
unigs <- getNgramTables(1, ltcorpus)
bigrs <- getNgramTables(2, ltcorpus)
trigs <- getNgramTables(3, ltcorpus)
gamma3 <- 0.5  # trigram discount
gamma2 <- 0.5  # bigram discount
top_n <- 3
bigPre <- "sell_the"
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
# finally, calc trigram probabilities - start with observed trigrams: eqn 12
qbo_obs_trigrams <- getObsTriProbs(bigPre, trigs, gamma3)
bigram <- bigrs[bigrs$ngram %in% bigPre]
alpha_trig <- getAlphaTrigram(bigram, trigs, gamma3)
qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
                                       qbo_unobs_bigrams, alpha_trig)
