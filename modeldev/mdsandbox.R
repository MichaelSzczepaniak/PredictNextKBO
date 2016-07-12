

ddir <- file.path("D:", "Dropbox", "sw_dev", "projects", "PredictNextKBO",
                  "data", "en_US", "ngrams")
# sort by freq (primary) then by ngram (secondary)
unigrams.blogs.raw <- arrange(unigrams.blogs.raw, freq, ngram)
unigrams.news.raw <- arrange(unigrams.news.raw, freq, ngram)
unigrams.twitter.raw <- arrange(unigrams.twitter.raw, freq, ngram)
# merge tables

# rm(list = ls())
source('Ngrams.R')
merged.unigrams.raw <- mergeFreqTables(unigrams.blogs.raw, unigrams.news.raw, 1000, TRUE)
merged.unigrams.raw <- arrange(merged.unigrams.raw, frequency, ngram)
write.csv(merged.unigrams.raw, sprintf("%s%s", ddir, "ngrams/merged.blogs.news.csv"), row.names=FALSE)

merged2.unigrams.raw <- mergeFreqTables(merged.unigrams.raw, unigrams.twitter.raw, 10000, TRUE)
merged2.unigrams.raw <- arrange(merged2.unigrams.raw, freq, ngram)
write.csv(merged2.unigrams.raw, sprintf("%s%s", ddir, "ngrams/merged.all.raw.csv"), row.names=FALSE)

# get unigram singletons
unigSingles.all <- merged2.unigrams.raw[merged2.unigrams.raw$freq==1,]
write.csv(unigSingles.all, sprintf("%s%s", ddir, "ngrams/unigramSingletonsAll.csv"), row.names=FALSE)

rm(list = ls())
source('Ngrams.R')
filePath <- sprintf("%s%s", ddir, 'ngrams/unigramSingletonsAll.csv')
usings <- read.csv(filePath)
breaking.indices <- getBreakingIndices(filePath)
breakWritePartdFiles()
singletons <- loadIgnoreParts()

# build bigram freq tables for each corpus type
rm(list = ls())
source('Ngrams.R')
loadLibs()
input.postfix <- ".10fixeos.txt"
input.prefixes <- c("en_US.blogs.train", "en_US.news.train", "en_US.twitter.train")
input.paths <- sprintf("%s%s%s", ddir, input.prefixes, input.postfix)
output.bigram.paths <- sprintf("%s%s%s", ddir, input.prefixes, ".12bigrams.csv")
output.trigram.paths <- sprintf("%s%s%s", ddir, input.prefixes, ".13trigrams.csv")

charvect <- read_lines(input.paths[1])
bigrams.blogs <- getNgramTables(2, charvect, "USIN") # ignore unigram singletons
write.csv(bigrams.blogs, output.bigram.paths[1], row.names = FALSE)

charvect <- read_lines(input.paths[2])
bigrams.news <- getNgramTables(2, charvect, "USIN") # ignore unigram singletons
write.csv(bigrams.news, output.bigram.paths[2], row.names = FALSE)

charvect <- read_lines(input.paths[3])
bigrams.twitter <- getNgramTables(2, charvect, "USIN") # ignore unigram singletons
write.csv(bigrams.twitter, output.bigram.paths[3], row.names = FALSE)