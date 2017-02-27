source('PreEda.R')

# Purpose of this module is to explore performace improvements (both speed and
# accuracy).  This exploration follows 3 overarching paths:
# 1) "Hashify" the n-gram lookups: Original app does string lookups in the 
#     n-gram tables as part of computing next word probabilities. Converting
#     the n-grams to a hash key which can quickly retrieve counts should
#     noticeably improve performance.
# 2) Remove lower count n-grams: Original app removed singletons, but further
#    performance improvements can be achieved by removing n-grams with counts
#    from 2 to 5.  This will result in add'l probability mass which can be
#    ditributed to unobserved n-grams which should improve accuracy as well as
#    performance.
# 3) Add 4-grams: Original app built prediction model on n-grams from n=1 to 3.
#    If enough spaced has been freed up as a result of 2), expanding the model
#    to include using 4-grams should improve prediction accuracy.

# Setup paths to the ngram frequency tables which exclude singletons
uniPaths <- c("https://www.dropbox.com/s/033qzeiggmcauo9/en_US.blogs.train.12unigrams.nosins.csv?dl=1",
              "https://www.dropbox.com/s/uo971onngv468t1/en_US.news.train.12unigrams.nosins.csv?dl=1",
              "https://www.dropbox.com/s/41yzwgwdviv8i5k/en_US.twitter.train.12unigrams.nosins.csv?dl=1")
bigPaths <- c("https://www.dropbox.com/s/6cgqa487xb0srbt/en_US.blogs.train.13bigrams.nosins.csv?dl=1",
              "https://www.dropbox.com/s/5xobfsotplbqtv3/en_US.news.train.13bigrams.nosins.csv?dl=1",
              "https://www.dropbox.com/s/47hwbqffufmg16m/en_US.twitter.train.13bigrams.nosins.csv?dl=1")
triPaths <- c("https://www.dropbox.com/s/z0rz707mt3da1h1/en_US.blogs.train.14trigrams.nosins.csv?dl=1",
              "https://www.dropbox.com/s/6e8eueyvnqa3jgs/en_US.news.train.14trigrams.nosins.csv?dl=1",
              "https://www.dropbox.com/s/6y0rvzd2bt45f1q/en_US.twitter.train.14trigrams.nosins.csv?dl=1")

unigrams_blogs_inc2 <- read.csv(uniPaths[1])
unigrams_news_inc2 <- read.csv(uniPaths[2])
unigrams_twitter_inc2 <- read.csv(uniPaths[3])

bigrams_blogs_inc2 <- read.csv(bigPaths[1])
bigrams_news_inc2 <- read.csv(bigPaths[2])
bigrams_twitter_inc2 <- read.csv(bigPaths[3])

trigrams_blogs_inc2 <- read.csv(triPaths[1])
trigrams_news_inc2 <- read.csv(triPaths[2])
trigrams_twitter_inc2 <- read.csv(triPaths[3])

# Removes entries from an n-gram frequency/count table (ngram_table) that are
# at or below a theshold (thresh_count), writes the revised table to a file
# (outfile) and returns the dataframe which was written out.
# ngram_table - dataframe with 2 columns: ngram and freq. The former is a col
#               of strings while the later is an integer that's the count of
#               the ngram value in the corpus of interest.
# thresh_count - integer that is the count of the n-grams to be removed from
#                ngram_table
# outfile - full path to the updated n-gram frequency/count table to be written
#           as output 
removeLowerCounts <- function(ngram_table, thresh_count=1,
                              outfile='../data/out.csv') {
    # ngram_tables are sorted by ngram
    # new_df <- arrange(ngram_table, freq, ngram)
    new_df <- filter(new_df, freq > thresh_count)
    write.csv(new_df, outfile, row.names = FALSE)
    
    return(arrange(new_df, ngram))
}