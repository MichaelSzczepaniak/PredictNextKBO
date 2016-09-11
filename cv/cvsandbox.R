
rm(list = ls())
source("../predictnextkbo/Katz.R")
source("KboCv.R")

## Make a test data grid
data_grid <- makeEmptyDataGrid(g2_start=0.1, g2_end=1.9, g3_start=0.1,
                               g3_end=1.9, intv=0.1, trials=100)

corpus_data <- c("https://www.dropbox.com/s/9dx3oo1w5uf8n1t/en_US.blogs.train.8posteos.txt?dl=1",
                 "https://www.dropbox.com/s/54cvi36161y6pvk/en_US.news.train.8posteos.txt?dl=1",
                 "https://www.dropbox.com/s/6ayhavfnzs5lmqa/en_US.twitter.train.8posteos.txt?dl=1")

corpus_lines <- read_lines(corpus_data[1])

ng_paths=c("https://www.dropbox.com/s/033qzeiggmcauo9/en_US.blogs.train.12unigrams.nosins.csv?dl=1",
           "https://www.dropbox.com/s/6cgqa487xb0srbt/en_US.blogs.train.13bigrams.nosins.csv?dl=1",
           "https://www.dropbox.com/s/z0rz707mt3da1h1/en_US.blogs.train.14trigrams.nosins.csv?dl=1")

runTrials(corpus_lines, data_grid, ng_paths)


corpus_lines <- corpus_lines[1:24]
ng=3

x <- paste(str_split_fixed(target_trigram, "_", 3)[1,1:2], collapse = "_")

trigram_targets <- vector(mode = "character", 100)
for(i in 1:100) {
    trigram_targets[i] <- getRandomNgram(corpus_lines, 3, " ")
}

s1 <- str_split_fixed(trigram_targets, " ", 3)[,1]
s2 <- str_split_fixed(trigram_targets, " ", 3)[,2]
s3 <- str_split_fixed(trigram_targets, " ", 3)[,3]
s4 <- rep("       ", 100)
bigs <- paste(s1, s2, sep = "_")
df <- data.frame(bigram_prefix=bigs, target_word=s3, predicted_word=s4)
# blogs ngram frequency table paths
b=c("https://www.dropbox.com/s/033qzeiggmcauo9/en_US.blogs.train.12unigrams.nosins.csv?dl=1",
    "https://www.dropbox.com/s/6cgqa487xb0srbt/en_US.blogs.train.13bigrams.nosins.csv?dl=1",
    "https://www.dropbox.com/s/z0rz707mt3da1h1/en_US.blogs.train.14trigrams.nosins.csv?dl=1")
unigrams <- read.csv(b[1], stringsAsFactors = FALSE)
bigrams <- read.csv(b[2], stringsAsFactors = FALSE)
trigrams <- read.csv(b[3], stringsAsFactors = FALSE)
source("C:/data/dev/PredictNextKBO/cv/KboCv.R")
source("C:/data/dev/PredictNextKBO/predictnextkbo/Katz.R")
for(i in 1:100) {
    big_pre <- bigs[i]
    cat("start prediction for: [", big_pre, "] @ time:", as.character(Sys.time()), "\n")
    predicted_word <- getTopPrediction(big_pre, gamma2=0.5, gamma3=0.5,
                                       unigrams, bigrams, trigrams)
    df$predicted_word[i] <- predicted_word
    cat("     predicted:", predicted_word, "at time:", as.character(Sys.time()), "\n")
}

write.csv(df, "blogs_test.csv", row.names = FALSE)

## test getTopPrediction function
bigPre <- c("i_love", "i_will", "love_to")
for(i in 1:3) {
    big_pre <- bigPre[i]
    cat("start prediction for: [", big_pre, "] @ time:", as.character(Sys.time()), "\n")
    predicted_word <- getTopPrediction(big_pre, gamma2=0.5, gamma3=0.5,
                                       unigrams, bigrams, trigrams)
    cat("   predicted: [", predicted_word, "] @ time:", as.character(Sys.time()), "\n")
}