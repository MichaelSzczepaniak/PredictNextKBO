library(readr)
library(stringr)

rm(list = ls())
source("../predictnextkbo/PredictNextWord.R")
corpus_data <- c("https://www.dropbox.com/s/9dx3oo1w5uf8n1t/en_US.blogs.train.8posteos.txt?dl=1",
                 "https://www.dropbox.com/s/54cvi36161y6pvk/en_US.news.train.8posteos.txt?dl=1",
                 "https://www.dropbox.com/s/6ayhavfnzs5lmqa/en_US.twitter.train.8posteos.txt?dl=1")
g2_start=0.1; g2_end=1.9; g3_start=0.1; g3_end=1.9; intv=0.1; trials=100
corpus_lines <- read_lines(corpus_data[1])
ng=3

## Returns 3 columns data.frame: gamma2 = bigram discount
##                               gamma3 = trigram discount
##                               trials = number of trials used to calc predacc
##                               predacc = prediction accuracy
## g2_start - 
## g2_end - 
## g3_start - 
## g3_end - 
## intv - spacing interval between gx_start and gx_end
## trials - number of trials used to calc prediction accuracy (predacc)
makeEmptyDataGrid <- function(g2_start=0.1, g2_end=1.9, g3_start=0.1,
                              g3_end=1.9, intv=0.1, trials=100) {
    # make grid manually
    g3_seq <- seq(g3_start, g3_end, intv)
    g2_seq <- unlist(lapply(g3_seq, rep, length(g3_seq)))
    g3_seq <- rep(g3_seq, length(g3_seq))
    df_data_grid <- data.frame(gamma2=g2_seq, gamma3=g3_seq,
                               trials=trials, predacc=-1)
    return(df_data_grid)
}

## Returns an underscore (_) delimited string of ng words of the form:
## w1_w2_...wN where N=ng is the number of words to return from a line.
## A random line is selected from corpus_lines and a random n-gram of size ng
## is selected from within the random line and returned as a single _ delimited
## string.
## corpus_lines - 
## ng - number of words in the returned n-gram, default = 3
getRandomNgram <- function(corpus_lines, ng=3) {
    # pick a line at random that has enough words in it
    random_line <- ""
    # pick a line that has at least ng number of words in it
    while (length(str_split(random_line, " ")[[1]]) < ng) {
        line_index <- sample(1:length(corpus_lines), 1, TRUE)
        random_line <- corpus_lines[line_index]
    }
    # pick a random n-gram from within the line
    ngram_index <- sample(1:(length(str_split(random_line, " ")[[1]]) -
                                 ng + 1), 1, TRUE)
    ngram <- getNgram(random_line, ngram_index)
    
    return(ngram)
}

## Returns an underscore (_) delimited string of nw words of the form:
## w1_w2_...wN where N=nw is the number of words to return from a single
## element character vector rline.
## rline - single element character vector with at least nw words in it
## nindex - index within rline of the first word in the n-gram to be returned
## nw - number of words in the n-gram to be returned
getNgram <- function(rline, nindex, nw) {
    line_tokens <- str_split(rline, " ")[[1]]
    line_tokens <- line_tokens[nindex:(nindex+nw-1)]
    # why use collapse: https://gist.github.com/briandk/d9231ba1e2603eed0df1
    return(paste(line_tokens, collapse="_"))
}

## Runs ntrials number of trials on corpus data at corpus_uri and outputs the
## results to resultsFile.
## corpus_lines - large character vector where each element holds a line from
##                the corpus from which predictions are being made
## data_grid - dataframe with following columns:
##             gamma2: bigram discount rate used for this set of trials
##             gamma3: trigram discount rate used for this set of trials
##             trials: number of predictions made on the corpus to calculate
##                     predacc over
##             predacc: prediction accuracy over trials number of trials
## ntrials - int, the number of prediction trials to execute on the corpus per
##           discount pair
## results_file - file holding the results of ntrial prediction trials on the
##                corpus
runTrials <- function(corpus_lines, data_grid, ntrials=100,
                      corpus_uri=corpus_data[1],
                      results_file="blogs_t=100.csv") {
    
}

## heat map experimentation
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

mydata <- mtcars[, c(1,3,4,5,6,7)]
# create data to make heat map from
cormat <- round(cor(mydata),2)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)
p <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                              midpoint = 0, limit = c(-1,1), space = "Lab", 
                              name="Pearson\nCorrelation")
p
