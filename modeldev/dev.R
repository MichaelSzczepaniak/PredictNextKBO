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
unobs_bo_bigrams
