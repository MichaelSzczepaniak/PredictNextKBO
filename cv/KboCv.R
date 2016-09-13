library(readr)
library(stringr)

init <- function() {
    rm(list = ls())
    corpus_data <- c("https://www.dropbox.com/s/9dx3oo1w5uf8n1t/en_US.blogs.train.8posteos.txt?dl=1",
                     "https://www.dropbox.com/s/54cvi36161y6pvk/en_US.news.train.8posteos.txt?dl=1",
                     "https://www.dropbox.com/s/6ayhavfnzs5lmqa/en_US.twitter.train.8posteos.txt?dl=1")
    b=c("https://www.dropbox.com/s/033qzeiggmcauo9/en_US.blogs.train.12unigrams.nosins.csv?dl=1",
        "https://www.dropbox.com/s/6cgqa487xb0srbt/en_US.blogs.train.13bigrams.nosins.csv?dl=1",
        "https://www.dropbox.com/s/z0rz707mt3da1h1/en_US.blogs.train.14trigrams.nosins.csv?dl=1")
    n=c("", "", "") # TODO
    t=c("", "", "") # TODO
    ngram_paths <- list(blogs=b, news=n, twitter=t)
    
    
    g2_start=0.1; g2_end=1.9; g3_start=0.1; g3_end=1.9; intv=0.1; trials=100
    corpus_lines <- read_lines(corpus_data[1])
    ng=3
}

## Returns a single word character vector which has the highest probability of
## completing the trigram starting with the two words defined in the bigram
## prefix parameter bigPre based on the KBO Trigram alogrithm.
##
## bigPre - last 2 words of user input separated by an _ e.g. sell_the
##          This is also referred to as the bigram prefix in code futher
##          downstream.
## gamma2 - bigram discount rate
## gamma3 - trigram discount rate
## unigrams - 2 column data.frame: ngram - a unigram in the corpus of interest
##                                 freq - count of this unigram in the corpus
## bigrams - 2 column data.frame: ngram - a bigram in the corpus of interest
##                                freq - count of this bigram in the corpus
## trigrams - 2 column data.frame: ngram - a trigram in the corpus of interest
##                                 freq - count of this trigram in the corpus
getTopPrediction <- function(bigPre, gamma2, gamma3,
                             unigrams, bigrams, trigrams) {
    obs_trigs <- getObsTrigs(bigPre, trigrams)
    unobs_trig_tails <- getUnobsTrigTails(obs_trigs$ngram, unigrams)
    bo_bigrams <- getBoBigrams(bigPre, unobs_trig_tails)
    # separate bigrams which use eqn 10 and those that use 16
    obs_bo_bigrams <- getObsBoBigrams(bigPre, unobs_trig_tails, bigrams)
    unobs_bo_bigrams <- getUnobsBoBigrams(bigPre, unobs_trig_tails,
                                          obs_bo_bigrams)
    # calc obs'd bigram prob's from eqn 10
    qbo_obs_bigrams <- getObsBigProbs(obs_bo_bigrams, unigrams, gamma2)
    # calc alpha_big & unobs'd bigram prob's from eqn 16
    unig <- str_split(bigPre, "_")[[1]][2]
    unig <- unigrams[unigrams$ngram == unig,]
    alpha_big <- getAlphaBigram(unig, bigrams, gamma2)
    qbo_unobs_bigrams <- getQboUnobsBigrams(unobs_bo_bigrams, unigrams, alpha_big)
    # calc trigram probabilities - start with observed trigrams: eqn 12
    qbo_obs_trigrams <- getObsTriProbs(obs_trigs, bigrams, bigPre, gamma3)
    # finally, calc trigram unobserved probabilities: eqn 17
    bigram <- bigrams[bigrams$ngram %in% bigPre, ]
    alpha_trig <- getAlphaTrigram(obs_trigs, bigram, gamma3)
    qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
                                           qbo_unobs_bigrams, alpha_trig)
    qbo_trigrams <- rbind(qbo_obs_trigrams, qbo_unobs_trigrams)
    qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]
    predicted_word <- qbo_trigrams[1]$ngram
    predicted_word <- str_split(predicted_word, "_")[[1]][3]
    
    return(predicted_word)
}

## Returns 4 columns data.frame: gamma2 = bigram discount
##                               gamma3 = trigram discount
##                               trials = number of trials used to calc predacc
##                               predacc = prediction accuracy
## g2_start - smallest value for bigram discount gamma2 to eval from
## g2_end - largest value for bigram discount gamma2 to eval up to
## g3_start - smallest value for trigram discount gamma3 to eval from
## g3_end - largest value for trigram discount gamma3 to eval up to
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
getRandomNgram <- function(corpus_lines, ng=3, delim="_") {
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
    ngram <- getNgram(random_line, ngram_index, ng, delim)
    
    return(ngram)
}

## Returns string delimited by delimiter (default _) of nw words of the form:
## w1_w2_...wN where N=nw is the number of words to return from a single
## element character vector rline.
## rline - single element character vector with at least nw words in it
## nindex - index within rline of the first word in the n-gram to be returned
## nw - number of words in the n-gram to be returned
## delimiter - character(s) to delimit returned n-gram
getNgram <- function(rline, nindex, nw, delimiter="_") {
    line_tokens <- str_split(rline, " ")[[1]]
    line_tokens <- line_tokens[nindex:(nindex+nw-1)]
    # why use collapse: https://gist.github.com/briandk/d9231ba1e2603eed0df1
    return(paste(line_tokens, collapse=delimiter))
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
## ngram_paths - 
## results_file_prefix - prefix of the file holding the results of ntrial
##                       prediction trials on the corpus
## ng - n-gram size, default 3 (trigram)
## dgrid_start - which row in data_grid to start processing at, default = 1
## out_dir - directory to write the results file to
## fold - int, fold identifier
runTrials <- function(corpus_lines, data_grid, ngram_paths, fold=1,
                      results_file_prefix="blogs_t=", ng=3, dgrid_start=1,
                      out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/",
                      seed_value=709) {
    cat("Start reading ngram frequency tables @ time:", as.character(Sys.time()), "\n")
    unigrams <- read.csv(ngram_paths[1])
    bigrams <- read.csv(ngram_paths[2])
    trigrams <- read.csv(ngram_paths[3])
    cat("Finish reading ngram frequency tables @ time:", as.character(Sys.time()), "\n")
    cat("Begin data grid evaluation @ time:", as.character(Sys.time()), "\n")
    set.seed(seed_value)  # for reproducibility
    for(experiment in dgrid_start:nrow(data_grid)) {
        gam2 <- data_grid$gamma2[experiment]
        gam3 <- data_grid$gamma3[experiment]
        ntrials <- data_grid$trials[experiment]
        # cat("***** START experiment:", experiment, "@ time:", as.character(Sys.time()), "\n")
        # cat("      # of trials =", ntrials, ", gamma2 =", gam2, ", gamma3 =", gam3, "\n")
        good_predictions = 0
        for(trial in 1:ntrials) {
            target_trigram <- getRandomNgram(corpus_lines, ng)
            target_word <- str_split_fixed(target_trigram, "_", 3)[1,3]
            bigPre <- paste(str_split_fixed(target_trigram, "_", 3)[1,1:2],
                            collapse = "_")
            predicted_word <- getTopPrediction(bigPre, gam2, gam3,
                                               unigrams, bigrams, trigrams)
            good_predictions <- good_predictions +
                                (target_word == predicted_word)
        }
        accuracy <- good_predictions / ntrials
        data_grid$predacc[experiment] <- accuracy
        # cat("      Experiment accuracy =", accuracy, "\n")
        # cat("***** END Experiment:", experiment, "@ time:", as.character(Sys.time()), "*****\n")
        exp_results <- sprintf("%.1f%s%.1f%s%i%s%.4f%s%s",
                               gam2, ",", gam3, ",", ntrials, ",",
                               accuracy, ",", as.character(Sys.time()))
        cat(exp_results, "\n")
        # write the intermediate results to be safe
        out_file <- sprintf("%s%s%s%i%s", results_file_prefix,
                            data_grid$trials[1], ".fold.", fold, ".csv")
        out_file <- sprintf("%s%s", out_dir, out_file)
        write.csv(data_grid, out_file, row.names = FALSE)
    }
    
    cat("FINISH data grid evaluation with", ntrials, "trials @ time:",
        as.character(Sys.time()), "\n")
}

# https://www.dropbox.com/s/1kpclcq7o907t61/blogs_test.csv?dl=1
gamma_df <- read.csv("blogs_test.csv")

makeHeatMapAccVgammas <- function(gamma_df=gamma_df) {
    library(ggplot2)
    p <- ggplot(data = gamma_df, aes(x=gamma2, y=gamma3, fill=predacc)) + geom_tile()
    p <- p + scale_fill_gradient2(low = "white", high = "red",
                                  space = "Lab",
                                  name="Accuracy\n")
    # http://stackoverflow.com/questions/9639127#21016269
    p <- p + theme(legend.position="right")
    p <- p + guides(color=guide_colourbar(title.vjust=0.5))
    p
}

makeCountourAccVgammas <- function() {
    library(ggplot2)
    # Controur - http://docs.ggplot2.org/0.9.3.1/stat_contour.html
    # Basic plot
    v <- ggplot(gamma_df, aes(x=gamma2, y=gamma3, z=predacc))
    v + stat_contour()
    v + xlab("bigram discount") + ylab("trigram discount")
    v
}


make3dAccVgammas <- function(df=gamma_df) {
    # 3D Plot - https://www.r-bloggers.com/3d-plots-in-r/
    # install.packages("plot3D")
    library(plot3D)
    par(mar = c(2, 2, 2, 2))
    par(mfrow = c(1, 1))
    df <- gamma_df[,c(1,2,4)]
    xmat <- matrix(df$gamma2, nrow=19, ncol=19)
    ymat <- matrix(df$gamma3, nrow=19, ncol=19)
    zmat <- matrix(df$predacc, nrow=19, ncol=19)
    
    surf3D(x=xmat, y=ymat, z=zmat, xlab="bigram discount", ylab="trigram discount",
           zlab="prediction accuracy",
           colkey=TRUE, bty="b2",
           main="Bigram & Trigram Discount Rates vs. Prediction Accuracy")
}




## heat map experimentation
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

# mydata <- mtcars[, c(1,3,4,5,6,7)]
# # create data to make heat map from
# cormat <- round(cor(mydata),2)
# library(reshape2)
# melted_cormat <- melt(cormat)
# library(ggplot2)
# p <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
# p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                               midpoint = 0, limit = c(-1,1), space = "Lab", 
#                               name="Pearson\nCorrelation")
# p
