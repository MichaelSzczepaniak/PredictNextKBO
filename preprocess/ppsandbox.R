# Install required packages only if they are needed.
list.of.packages <- c('dplyr', 'readr', 'stringr', 'quanteda')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
lapply(list.of.packages, require, character.only=TRUE)  # load libs

ddir <- "C:/data/dev/PredictNextKBO/data/en_US/"
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
source('PreEda.R')

inpre <- '.2sents.txt'
outpre <- '.3ascii.txt'
# function: convertToAscii

inpre <- '.3ascii.txt'
outpre <- '.4notags.txt'
# function: convertUnicodeTags

inpre <- '.4notags.txt'
outpre <- '.5nourls.txt'
# function: removeUrls

inpre <- '.5nourls.txt'
outpre <- '.6preeos.txt'
# function: preEosClean

inpre <- '.6preeos.txt'
outpre <- '.7eos.txt'
# function: addEosMarkers

runFilterAndWrite(addEosMarkers, ddir, inpre, outpre)


