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
    n=c("", "", "") # TODO news
    t=c("", "", "") # TODO twitter
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

## Returns 3 columns data.frame: gamma2 = bigram discount
##                               gamma3 = trigram discount
##                               predacc = prediction accuracy
## g2_start - smallest value for bigram discount gamma2 to eval from
## g2_end - largest value for bigram discount gamma2 to eval up to
## g3_start - smallest value for trigram discount gamma3 to eval from
## g3_end - largest value for trigram discount gamma3 to eval up to
## intv - spacing interval between gx_start and gx_end
makeEmptyDataGrid <- function(g2_start=0.1, g2_end=1.9, g3_start=0.1,
                              g3_end=1.9, intv=0.1) {
    # make grid manually
    g3_seq <- seq(g3_start, g3_end, intv)
    g2_seq <- unlist(lapply(g3_seq, rep, length(g3_seq)))
    g3_seq <- rep(g3_seq, length(g3_seq))
    df_data_grid <- data.frame(gamma2=g2_seq, gamma3=g3_seq, predacc=-1)
    
    return(df_data_grid)
}

## Returns a character array of trigram_count elements. Each element is an
## _ delimited trigram of the form w1_w2_w3 randomly extracted from
## corpus_lines.  EACH TRIGRAM IS UNIQUE.
## corpus_lines - character array where each element is a line of text from
##                a corpus file such as blogs, news, or twitter
## ngram_count - the number of randomly selected ngrams to return from
##               corpus_lines
## ng - number of words in the returned n-gram, default = 3
## delim - delimiter used in the returned ngram e.g. if delim and ng are
##         their default values, then returned value will be of the form:
##         w1_w2_w3
## seed_val - 
getUniqueRandomNgrams <- function(corpus_lines, ngram_count,
                                  ng=3, delim="_", seed_val=719) {
    set.seed(seed_val)
    random_ngrams <- vector(mode = "character")
    for(i in 1:ngram_count) {
        ngram <- ""
        while((nchar(ngram)[1] < 2) || (ngram %in% random_ngrams)) {
            # cat("ngram", ngram, "is empty or in set already\n")
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
        }
        # cat("adding ngram:", ngram)
        random_ngrams <- append(random_ngrams, ngram)
    }
    
    return(random_ngrams)
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

## Trains the model on corpus_lines and fills in the predacc column of the
## gamma_grid dataframe that is passed in.  Three columns in gamma_grid are:
## gamma2 - bigram discount
## gamma3 - trigram discount
## predacc - prediction accuracy est'd from nitrs predicitons on each
##           (gamma2, gamma3) pair
## Function writes results to: out_dir/cv_<corpus_type>_<fold>fold_<nitrs>.csv
## E.g out_dir/cv_blogs_1fold_500.csv
##
## Precondition: 1) Function assumes that fold_ngrams list is in the workspace.
##                  If fold_ngrams is not in the workspace, it attempts to read
##                  this data from out_dir/foldNgramTables.RData.
##                  If this file can't be found, an error will occur.
##               2) If the path to the predict trigrams is not supplied, 
##                  function attempts to read this data from
##                  out_dir/fold_xy_predict.txt where x is the fold #: 1-5 and 
##                  y is the corpus type e.g. 'blogs', 'news', or 'twitter'
##
## PARAMETERS:
## ngram_tables - k element outer list: one element per fold. Each list
##                contains inner list of 3 items:
##                1st inner list is the unigram frequency table
##                2nd inner list is the bigram frequency table
##                3rd inner list is the trigram frequency table
## gamma_grid - 3 columns dataframe as described above
## write_freq - frequency in which to write updated calculations to output file
## fold - the fold within folds list to run trials on
## predict_words_path - path to the trigrams which are to be predicted as part
##                      of model training. If NULL (default) this file name is 
##                      assumed to be of the form:
##                      fold_<fold><corpus_type>_predict.txt
##                      and located in the out_dir directory
## ggrid_start - row in gamma_grid to start running trials on
## itr_start - row in the gamma_grid dataframe to start processing from
## corpus_type - type of corpus: "blogs", "news", "twitter"
## out_dir - directory to write output to
## ofile_prefix - prefix to use for the output file name
## ofile_postfix - postfix to use for the output file name
trainFold <- function(gamma_grid, write_freq=100, fold=1,
                      predict_words_path=NULL, ggrid_start=1, itr_start=1,
                      corpus_type="blogs",
                      out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/",
                      file_prefix="fold_", ofile_postfix="cv_results.csv") {
    if(is.null(predict_words_path)) {
        predict_words_path <- paste0(out_dir, file_prefix, fold,
                                     corpus_type, "_predict.txt")
    }
    predict_words <- read_lines(predict_words_path)
    nitrs <- length(predict_words)
    if(!exists("fold_ngrams")) { 
        fold_ngrams <- importFoldNgramtables()
        cat("Fold ngram table data read has completed at",
            as.character(Sys.time()), "\n")
    }
    out_file <- paste0(out_dir, "cv_", corpus_type, "_", "fold", fold, "_itrs",
                       nitrs, ".csv")  # e.g. cv_blogs_fold1_itrs500.csv
    exp_results <- data.frame(gamma2=as.numeric(rep(-1, nrow(gamma_grid))),
                              gamma3=as.numeric(rep(-1, nrow(gamma_grid))),
                              acc=as.numeric(rep(-1, nrow(gamma_grid))),
                              predict=as.numeric(rep(-1, nrow(gamma_grid))),
                              success=as.numeric(rep(-1, nrow(gamma_grid))))
    # Get ngram tables for this fold.
    unigs <- fold_ngrams[[fold]][1][[1]]
    bigrs <- fold_ngrams[[fold]][2][[1]]
    trigs <- fold_ngrams[[fold]][3][[1]]
    for(i in ggrid_start:nrow(gamma_grid)) {
        good_predictions <- 0
        g2 <- gamma_grid$gamma2[i]
        g3 <- gamma_grid$gamma3[i]
        # These are the actual training steps.  Take trigram samples from
        # the training set that weren't used to build n-gram tables and make
        # predictions using each (gamma2, gamma3) pair in gamma_grid.
        for(j in itr_start:nitrs) {
            ttp <- predict_words[j]  # target to predict
            target_word <- str_split_fixed(ttp, "_", 3)[1,3]
            bigPre <- paste(str_split_fixed(ttp, "_", 3)[1,1:2],
                            collapse = "_")
            top_pred <- getTopPrediction(bigPre, g2, g3,
                                         unigs, bigrs, trigs)
            good_predictions <- good_predictions + (target_word == top_pred)
            accuracy <- good_predictions / j
            if(j %% write_freq == 0) {
                exp_results$gamma2[i] <- g2
                exp_results$gamma3[i] <- g3
                exp_results$acc[i] <- accuracy
                exp_results$predict[i] <- j
                exp_results$success[i] <- good_predictions
                write.csv(exp_results, out_file, row.names = FALSE)
                console_msg <- paste0("iteration ", j, ",", g2, ",", g3, ",",
                                      accuracy, ",", as.character(Sys.time()),
                                      "\n")
                cat(console_msg)
            }
        }
        exp_results$gamma2[i] <- g2
        exp_results$gamma3[i] <- g3
        exp_results$acc[i] <- accuracy
        exp_results$predict[i] <- j
        exp_results$success[i] <- good_predictions
        write.csv(exp_results, out_file, row.names = FALSE)
        out_line <- sprintf("%s%s%s%s%s%s%s%s", g2, ",",g3, ",",
                            accuracy, ",",  as.character(Sys.time()), "\n")
        cat(out_line)  # feedback for during very long set of computations
    }
    cat("*** FINAL *** results written to:\n", out_file, "\n",
        "at ", as.character(Sys.time()))
    
    return(exp_results)
}

## Returns a list with nfolds items. Each list contains the indices for the 
## data in each fold. Indices are then written to files: one set of indices
## per fold.
## indices_count - int that are the number of items to take a sample from. If
##                 sample data is a data frame, this is typically nrows(data).
## nfolds - number of folds in the data to make
## write_folds - TRUE if indices for each fold should be written to files
## fold_indices_file_prefix - start of the output file name
## fold_indices_file_postfix - end of the output file name
## out_dir - directory to write the output files if write_folds == TRUE
## seed_value - seed value for random selects, set for reproducibility
makeFolds <- function(indices_count, nfolds=5, write_folds=TRUE,
                      fold_indices_file_prefix="fold_",
                      fold_indices_file_postfix="blogs",
                      fold_indices_file_ext=".txt",
                      out_dir="./",
                      seed_value=719) {
    set.seed(seed_value)
    folds <- vector("list", nfolds)
    inds <- 1:indices_count
    min_per_fold <- length(inds) / nfolds # min # of samples in each fold
    for(i in 1:nfolds) {
        samp_inds = sample(inds, min_per_fold) # get indices for fold
        folds[[i]] <- samp_inds
        inds <- setdiff(inds, samp_inds) # remaining after taking for fold
        if(i == nfolds) {
            cat("there are ", length(inds), "remaining samples to distribute.\n")
            for(j in 1:length(inds)) {
                samp <- sample(inds, 1)
                folds[[j]] <- c(folds[[j]], samp)
                inds <- setdiff(inds, samp)
            }
        }
    }
    # write out the indices in each fold
    if(write_folds) {
        for(k in 1:nfolds) {
            out_file <- sprintf("%s%s%s%s", fold_indices_file_prefix, k,
                                fold_indices_file_postfix,
                                fold_indices_file_ext)
            out_file <- sprintf("%s%s", out_dir, out_file)
            write.table(folds[[k]], out_file, quote=FALSE, sep="\n",
                        row.names=FALSE, col.names=FALSE)
            cat("Finished writing", out_file, "\n")
        }
    }
    
    return(folds)
}

## Exports ngram frequency tables for a series of CV folds as an RData object.
##
## Precondition: n-gram tables are assumed to have file names of the form:
##               fold_<fold><table_type><ngram>grams.csv
##
## ngram_table_dir - path to the dir containing n-gram tables for each fold.
##                   This dir is also were output is written.
## in_file_prefix - char vector representing the first part of the name for
##                  n-gram tables used for each cv fold, default="fold_"
## folds - vector of ints representing the folds in the cv, default=1:5
## table_type - char vector representing the type of corpus in which n-grams
##              are being constructed, valid values: "train_blogs_",
##              "train_news_", and "train_twitter_"
## ngrams - vector of ints representing the n-gram tables to be exported,
##          default 1:3 (unigram, bigram, trigram)
## in_file_suffix - suffix of the n-gram table to import
exportFoldNgramTables <- 
    function(ngram_table_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/",
             in_file_prefix="fold_", folds=1:5, table_type="train_blogs_",
             ngrams=1:3, in_file_suffix="grams.csv") {
        fold_ngrams <- vector("list", length(folds))
        for(i in folds) {
            inner_ngrams <- vector("list", length(ngrams))
            for(j in ngrams) {
                ng_table_path <- paste0(ngram_table_dir, in_file_prefix, i,
                                        table_type, j, in_file_suffix)
                ng_table <- read.csv(ng_table_path)
                inner_ngrams[[j]] <- ng_table
            }
            fold_ngrams[[i]] <- inner_ngrams
        }
        ofile_path <- paste0(ngram_table_dir, file="foldNgramTables.RData")
        save(fold_ngrams, file=ofile_path)
    }

importFoldNgramtables <- 
    function(ngram_table_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/") {
    
        ofile_path <- paste0(ngram_table_dir, file="foldNgramTables.RData")
        load(ofile_path)
        
        return(fold_ngrams)
}

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


f1 <- read.csv('https://www.dropbox.com/s/524dgfw0ej2d4m3/cv_blogs_fold1_itrs500.csv?dl=1')
f2 <- read.csv('https://www.dropbox.com/s/2k5ypy3sovn6g9d/cv_blogs_fold2_itrs500.csv?dl=1')
f3 <- read.csv('https://www.dropbox.com/s/85ex5o9km20014m/cv_blogs_fold3_itrs500.csv?dl=1')
f4 <- read.csv('https://www.dropbox.com/s/5rf7mbhrpq617e9/cv_blogs_fold4_itrs500.csv?dl=1')
f5 <- read.csv('https://www.dropbox.com/s/vxq9bl6bmagv1x2/cv_blogs_fold5_itrs500.csv?dl=1')


