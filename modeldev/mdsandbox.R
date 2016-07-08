

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


