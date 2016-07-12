# Install required packages only if they are needed.
list.of.packages <- c('dplyr', 'readr', 'stringr', 'quanteda')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
lapply(list.of.packages, require, character.only=TRUE)  # load libs

ddir <- "D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US"
infiles <- c(sprintf('%s%s', dataDir, 'en_US.blogs.train..txt'),
             sprintf('%s%s', dataDir, 'en_US.news.train.3ascii.txt'),
             sprintf('%s%s', dataDir, 'en_US.twitter.train.3ascii.txt'))
names(infiles) <- c('blogs', 'news', 'twitter')
outfiles <- c(sprintf('%s%s', dataDir, 'en_US.blogs.train.4ascii.txt'),
              sprintf('%s%s', dataDir, 'en_US.news.train.4ascii.txt'),
              sprintf('%s%s', dataDir, 'en_US.twitter.train.4ascii.txt'))
names(outfiles) <- names(infiles)

# find all contractions
unicodePatter <- "[A-Za-z]{1}<U[+][A-Za-z0-9]{4}>(s|d|ve|t|ll|re)"
i <- 3
data <- read_lines(infiles[i])
ucodes <- unlist(str_extract_all(data, unicodePatter))
ucodesTable <- sort(table(ucodes), decreasing = TRUE)
write.csv(data.frame(tag=names(ucodesTable), freq=ucodesTable),
          sprintf('%s%s', names(infiles[i]), '.contractions.csv'), row.names=FALSE)


## Builds and writes out frquency tables on the unicode tags in the 3ascii.txt
## files
writeUnicodeTagFreqTables <-
    function(index, dataDir="C:/data/dev/PredictNextKBO/data/en_US/") {
        infiles <- c(sprintf('%s%s', dataDir, 'en_US.blogs.train.3ascii.txt'),
                     sprintf('%s%s', dataDir, 'en_US.news.train.3ascii.txt'),
                     sprintf('%s%s', dataDir, 'en_US.twitter.train.3ascii.txt'))
        names(infiles) <- c('blogs', 'news', 'twitter')
        unicodePatter <- "<U[+][A-F0-9]{4}>"
        data <- read_lines(infiles[index])
        ucodes <- unlist(str_extract_all(data, unicodePatter))
        ucodesTable <- sort(table(ucodes), decreasing = TRUE)
        write.csv(data.frame(tag=names(ucodesTable), freq=ucodesTable),
                  sprintf('%s%s', names(infiles[index]), '.utags.csv'), row.names=FALSE)
}

rm(list = ls())
setwd('../preprocess')
source('PreEda.R')

inpost <- '.2sents.txt'
outpost <- '.3ascii.txt'
# function: convertToAscii

inpost <- '.3ascii.txt'
outpost <- '.4notags.txt'
# function: convertUnicodeTags

inpost <- '.4notags.txt'
outpost <- '.5nourls.txt'
# function: removeUrls

inpost <- '.5.txt'
outpost <- '.6.txt'
filePres <- 'blogs.test'

inFilePostfix <- inpost
outFilePostfix <- outpost
filePrefixes <- filePres


rm(list = ls())
setwd('../preprocess')
source('PreEda.R')

inpost <- '.5nourls.txt'
outpost <- '.6preeos.txt'
runFilterAndWrite(preEosClean, ddir, inpost, outpost)

inpost <- '.6preeos.txt'
outpost <- '.7eos.txt'
runFilterAndWrite(addEosMarkers, ddir, inpost, outpost)

inpost <- '.7eos.txt'
outpost <- '.8posteos.txt'
# runFilterAndWrite(postEosClean, ddir, inpost, outpost, filePrefixes='en_US.blogs.train')
runFilterAndWrite(postEosClean, ddir, inpost, outpost)

rm(list = ls())
setwd('../preprocess')
source('PreEda.R')

inpost <- '.9ustokens.txt'
outpost <- '.10fixeos.txt'
runFilterAndWrite(fixSentWoEos, ddir, inpost, outpost)

###########################################################
# rm(list = ls())
# setwd('../preprocess')
# source('PreEda.R')
# 
# inpost <- '.7eos.txt'
# outpost <- '.8posteos.txt'
# runFilterAndWrite(postEosClean, ddir, inpost, outpost, filePrefixes='en_US.blogs.train')
# runFilterAndWrite(postEosClean, ddir, inpost, outpost, filePrefixes='en_US.news.train')
# runFilterAndWrite(postEosClean, ddir, inpost, outpost, filePrefixes='en_US.twitter.train')
# runFilterAndWrite(postEosClean, ddir, inpost, outpost)

rm(list = ls())
postfix <- '.8posteos.txt'
setwd('../modeldev')
source('Ngrams.R')
loadLibs()
prefixes <- c('en_US.blogs.train', 'en_US.news.train', 'en_US.twitter.train')
# infiles <- c(sprintf('%s%s', ddir, 'en_US.twitter.train.8posteos.txt'))
infiles <- c(sprintf('%s%s%s', ddir, prefixes[1], postfix),
             sprintf('%s%s%s', ddir, prefixes[2], postfix),
             sprintf('%s%s%s', ddir, prefixes[3], postfix))

charvect <- read_lines(infiles[1])
unigrams.blogs.raw <- getNgramTables(1, charvect)
write.csv(unigrams.blogs.raw,
         'D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/ngrams/unigrams.blogs.raw.csv',
         row.names = FALSE)

charvect <- read_lines(infiles[2])
unigrams.news.raw <- getNgramTables(1, charvect)
write.csv(unigrams.news.raw,
         'D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/ngrams/unigrams.news.raw.csv',
         row.names = FALSE)

charvect <- read_lines(infiles[3])
unigrams.twitter.raw <- getNgramTables(1, charvect)
write.csv(unigrams.twitter.raw,
          'D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/ngrams/unigrams.twitter.raw.csv',
          row.names = FALSE)


# blogs7 <- 'C:/data/dev/PredictNextKBO/data/en_US/en_US.blogs.train.7eos.txt'
# chvect <- read_lines(blogs7)
# blogs8 <- fixSpecial(chvect)
# b8filename <- 'D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/en_US.blogs.train.8eos.txt'
# writeLines(blogs8, b8filename)
##################
rm(list = ls())
setwd('../modeldev')
source('Ngrams.R')
loadLibs()
prefixes <- c('en_US.blogs.train.', 'en_US.news.train.', 'en_US.twitter.train.')
infiles <- c(sprintf('%s%s', ddir, 'en_US.blogs.train.8posteos.txt'),
             sprintf('%s%s', ddir, 'en_US.news.train.8posteos.txt'),
             sprintf('%s%s', ddir, 'en_US.twitter.train.8posteos.txt'))

makeRawUnigrams <- function() {
    charvect <- read_lines(infiles[1])
    unigrams.blogs.raw <- getNgramTables(1, charvect)
    write.csv(unigrams.blogs.raw, 
              'D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/ngrams/unigrams.blogs.raw.csv',
              row.names = FALSE)
    
    charvect <- read_lines(infiles[2])
    unigrams.news.raw <- getNgramTables(1, charvect)
    write.csv(unigrams.news.raw,
              'D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/ngrams/unigrams.news.raw.csv',
              row.names = FALSE)
    
    charvect <- read_lines(infiles[3])
    unigrams.twitter.raw <- getNgramTables(1, charvect)
    write.csv(unigrams.twitter.raw,
              'D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/ngrams/unigrams.twitter.raw.csv',
              row.names = FALSE)
}


charVect <- c('this has EOS', 'this is missing eo marker', 'that has EOS')


