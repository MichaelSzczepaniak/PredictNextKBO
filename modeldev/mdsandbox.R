

ddir <- file.path("D:", "Dropbox", "sw_dev", "projects", "PredictNextKBO",
                  "data", "en_US")
# sort by freq (primary) then by ngram (secondary)
unigrams.blogs.raw <- arrange(unigrams.blogs.raw, freq, ngram)
unigrams.news.raw <- arrange(unigrams.news.raw, freq, ngram)
unigrams.twitter.raw <- arrange(unigrams.twitter.raw, freq, ngram)

##
table.dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/"
filePrefix="en_US."
inFilePostfix=".train.11unigrams.raw.csv"
outFilePostfix=".train.12unigrams.nous.csv"
fileTypes=c("blogs", "news", "twitter")



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
# Install required packages only if they are needed.
list.of.packages <- c('dplyr', 'readr', 'stringr', 'quanteda')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
lapply(list.of.packages, require, character.only=TRUE)  # load libs

ddir <- "D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US"

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

# remove singletons
data.path <- sprintf("%s%s", ddir, "/en_US.blogs.train.12bigrams.nous.csv")
bigrams.blogs12 <- read.csv(data.path)
bigrams.blogs13 <- filter(bigrams.blogs12, freq > 1)
out.path <- sprintf("%s%s", ddir, "/en_US.blogs.train.13bigrams.nobsins.csv")
write.csv(bigrams.blogs13, out.path, row.names = FALSE)

data.path <- sprintf("%s%s", ddir, "/en_US.blogs.train.14trigrams.nous.csv")
trigrams.blogs20 <- read.csv(data.path)
trigrams.blogs21 <- filter(trigrams.blogs20, freq > 1)
out.path <- sprintf("%s%s", ddir, "/en_US.blogs.train.15trigrams.nobsins.csv")
write.csv(trigrams.blogs21, out.path, row.names = FALSE)

## bigrams singleton removal
prefixes <- c('/en_US.blogs.train.', '/en_US.news.train.', '/en_US.twitter.train.')
postfix.in <- '12bigrams.nous.csv'
postfix.out <- '13bigrams.nobsins.csv'
infiles <- c(sprintf('%s%s%s', ddir, prefixes[1], postfix.in),
             sprintf('%s%s%s', ddir, prefixes[2], postfix.in),
             sprintf('%s%s%s', ddir, prefixes[3], postfix.in))
outfiles <- c(sprintf('%s%s%s', ddir, prefixes[1], postfix.out),
             sprintf('%s%s%s', ddir, prefixes[2], postfix.out),
             sprintf('%s%s%s', ddir, prefixes[3], postfix.out))
for(i in 1:3) {
    bigrams.in <- read.csv(infiles[i])
    bigrams.out <- filter(bigrams.in, freq > 1)
    write.csv(bigrams.out, outfiles[i], row.names = FALSE)
}

## trigrams singleton remove
postfix.in <- '14trigrams.nous.csv'
postfix.out <- '15trigrams.nobsins.csv'
infiles <- c(sprintf('%s%s%s', ddir, prefixes[1], postfix.in),
             sprintf('%s%s%s', ddir, prefixes[2], postfix.in),
             sprintf('%s%s%s', ddir, prefixes[3], postfix.in))
outfiles <- c(sprintf('%s%s%s', ddir, prefixes[1], postfix.out),
              sprintf('%s%s%s', ddir, prefixes[2], postfix.out),
              sprintf('%s%s%s', ddir, prefixes[3], postfix.out))
for(i in 1:3) {
    trigrams.in <- read.csv(infiles[i])
    trigrams.out <- filter(trigrams.in, freq > 1)
    write.csv(trigrams.out, outfiles[i], row.names = FALSE)
}
