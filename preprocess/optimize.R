source('PreEda.R')

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