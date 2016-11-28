


## only need for dev
if(!exists('corpus_urls')) {
    rm(list = ls())
    corpus_urls <- c("https://www.dropbox.com/s/9dx3oo1w5uf8n1t/en_US.blogs.train.8posteos.txt?dl=1",
                     "https://www.dropbox.com/s/54cvi36161y6pvk/en_US.news.train.8posteos.txt?dl=1",
                     "https://www.dropbox.com/s/6ayhavfnzs5lmqa/en_US.twitter.train.8posteos.txt?dl=1")
    names(corpus_urls) <- c("blogs", "news", "twitter")
    # cat("reading corpus_lines...\n")
    # corpus_lines <- read_lines(corpus_urls[1])
}

if(!exists('fold_paths')) {
    # Note: These files need to be generated from the makeFolds function in KboCv.R
    blogs_paths <- c("D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_1blogs.txt",
                     "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_2blogs.txt",
                     "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_3blogs.txt",
                     "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_4blogs.txt",
                     "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_5blogs.txt")
    news_paths <- c("D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_1news.txt",
                    "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_2news.txt",
                    "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_3news.txt",
                    "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_4news.txt",
                    "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_5news.txt")
    twitr_paths <- c("D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_1twitter.txt",
                     "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_2twitter.txt",
                     "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_3twitter.txt",
                     "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_4twitter.txt",
                     "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_5twitter.txt")
    fold_paths <- data.frame(blogs=blogs_paths, news=news_paths,
                             twitter=twitr_paths, stringsAsFactors = FALSE)
}

source("../predictnextkbo/Katz.R")
source("KboCv.R")

## Returns a list of length(fold_paths) + 1 items.  Each item in the list is a
## vector of ints that are indices assigned to a validation fold except for
## the last item which is the total line count of all the folds
## fold_paths - a 5 x 3 data frame where each column is a corpus type:
##              "blogs", "news", or "twitter" and each row is a fold.
##              Each element is a url to a file which defines the indices
##              of the validation set for the corpus type and fold
## corp_type - character string, type of corpus: "blogs", "news", "twitter"
readFolds <- function(fold_paths, corp_type='blogs') {
    fold_count <- nrow(fold_paths)
    folds <- vector("list", fold_count+1)
    line_count <- 0
    for(i in 1:fold_count) {
        path <- fold_paths[i, corp_type]
        fold <- read.csv(path, header=FALSE)
        line_count <- line_count + nrow(fold)
        folds[[i]] <- fold$V1
    }
    
    folds[[fold_count + 1]] <- line_count
    
    return(folds)
}

# ng_paths=c("https://www.dropbox.com/s/033qzeiggmcauo9/en_US.blogs.train.12unigrams.nosins.csv?dl=1",
#            "https://www.dropbox.com/s/6cgqa487xb0srbt/en_US.blogs.train.13bigrams.nosins.csv?dl=1",
#            "https://www.dropbox.com/s/z0rz707mt3da1h1/en_US.blogs.train.14trigrams.nosins.csv?dl=1")


## Returns a named list of int vectors which are indices of training and  
## testing sets in the Non-Validation partition.  The odd numbered lists 
## contain the indices of the lines used for the training set.  Each set of 
## training fold indices are named in the following manner:
## fold_xtrain_y where x is an integer 1 to number of folds and
##                     y is one of the corpus types: blogs, news, twitter
## The even numbered lists contain the indices of the lines used for testing
## set.  Each set of testing fold indices are named in the following manner:
## fold_xtest_y where x is an integer 1 to number of folds and
##                    y is one of the corpus types: blogs, news, twitter
## The function writes out both training and test set indices to text files
## using the naming convention described above with a .txt extension.
## 
## PARAMETERS:
## folds - list of int vectors of size length(folds) which are indices of the
##         lines for each validation partition. Indices not in this set are 
##         further segmented into training and testing sets
## corp_type - character string, type of corpus: "blogs", "news", "twitter"
## train_fraction - float betwee 0 and 1 (non-inclusive), fraction of samples
##                  used for the test set, 
## seed_vals - seed values to use for train/test set selections
## ofile_prefix - string, output file name prefix
## ofile_postfix - string, output file name postfix
## out_dir - string, directory to write output files
makeTrainTestNV <- function(folds=default_folds, corp_type="blogs",
                            train_fraction=0.8,
                            seed_vals=c(7,11,13,17,19),
                            ofile_prefix="fold_",
                            ofile_postfix=c("train.txt", "test.txt"),
                     out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/") {
    corpus_lines <- read_lines(corpus_urls[corp_type])
    fold_count <- length(folds) - 1  # because last count is total line count
    results <- list()
    for(i in 1:fold_count) {
        seed_val <- seed_vals[i]
        set.seed(seed_val)  # set for reproducibility
        validn_fold_indices <- folds[[i]]
        non_validn_fold_indices <- setdiff(1:folds[[fold_count+1]],
                                           validn_fold_indices)
        validn_data <- corpus_lines[validn_fold_indices]
        # Take 80% (train set) of non-validation data to build n-gram tables 
        # and use the other 20% (test set) to make predicitions on.
        train_ngrams_indices <-
            sample(non_validn_fold_indices,
                   train_fraction * length(non_validn_fold_indices))
        test_gammas_indices <- setdiff(non_validn_fold_indices,
                                       train_ngrams_indices)
        # Write out indices for the non-valildation training set
        name_prefix <- sprintf("%s%s%s%s","fold_", i, "train_", corp_type)
        fname <- sprintf("%s%s%s", out_dir, name_prefix, ".txt")
        results[[name_prefix]] <- train_ngrams_indices
        write.table(train_ngrams_indices, fname, row.names=FALSE, col.names=FALSE)
        # Write out indices for the non-valildation test set
        name_prefix <- sprintf("%s%s%s%s","fold_", i, "test_", corp_type)
        fname <- sprintf("%s%s%s", out_dir, name_prefix, ".txt")
        results[[name_prefix]] <- test_gammas_indices
        write.table(test_gammas_indices, fname, row.names=FALSE, col.names=FALSE)
    }
    
    return(results)
}

## Reads in the training set, creates the 1 thru n-gram frequency tables and
## writes them out to files.
## corp_data - 2 col dataframe: 1st column, ctype are the corpora type
##                              2nd column, corp_urls are the urls for the
##                                          data of each corpora type
## ng - int vector specifying the n-grams to be created
## folds - list of int lists which are the output of the readFolds function
## cv_dir - directory to find cv fold indices data and where n-gram tables
##          are written to
## ofile_prefix - prefix of output file names
## ofile_postfix - postfix of output file names
makeFoldNgramTables <- function(corp_data=
                                data.frame(ctype=c("blogs", "news", "twitter"),
                                           corp_url=corpus_urls),
                                ng=1:3, folds=default_folds,
              cv_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/",
              ofile_prefix="fold_", ofile_postfix="grams.csv") {
    for(c in 1:nrow(corp_data)) {
        corpus_type <- corp_data$ctype[c]
        corp_url <- corp_data$corp_url[c]
        corp_lines <- read_lines(corp_url)
        for(fold in 1:(length(folds)-1)) {
            train_fold_path <- sprintf("%s%s%s%s%s%s", cv_dir, "fold_", fold,
                                       "train_", corpus_type, ".txt")
            train_fold_indices <- read.table(train_fold_path, sep = "\n")$V1
            train_fold <- corp_lines[train_fold_indices]
            for(g in ng) {
                fname <- sprintf("%s%s%s%s%s%s%s", ofile_prefix, fold,
                                 "train_", corpus_type, "_", g, ofile_postfix)
                fpath <- sprintf("%s%s", cv_dir, fname)
                cat("Start table", fname, "building @ time:",
                    as.character(Sys.time()), "\n")
                ngram_table <- getNgramTables(g, train_fold)
                ngram_table <- filter(ngram_table, freq > 1) # remove singletons
                write.csv(ngram_table, fpath, row.names = FALSE)
                cat("Finish table", fpath, "write @ time:",
                    as.character(Sys.time()), "\n")
            }
        }
    }
}


corp_type="blogs"; npredict=500; i=1
corp_urls=corpus_urls
test_lines_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/"
test_lines_files=c("fold_1test_blogs.txt", "fold_2test_blogs.txt",
                   "fold_3test_blogs.txt", "fold_4test_blogs.txt",
                   "fold_5test_blogs.txt")
ofile_prefix="fold_"; ofile_postfix="blogs_predict.txt"
out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/"

## Creates and write out n lists of random trigrams which can be used to
## determine prediction accuracy.  PARAMETERS:
## corp_type - string, corpus data to process: "blogs", "news", "twitter"
## npredict - # of predictions to make per parameter set (gamma2, gamma3)
## corp_urls - char vector, urls of where the corpora data files lives
## test_lines_dir - string, local dir where test partion indices live
## test_lines_files - char vector, paths to test partion indices files 
## ofile_prefix - string, output file prefix
## ofile_postfix - string, output file postfix
makePredictTrigrams <- function(corp_type="blogs", npredict=500,
                                corp_urls=corpus_urls,
              test_lines_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/",
              test_lines_files=c("fold_1test_blogs.txt", "fold_2test_blogs.txt",
                                 "fold_3test_blogs.txt", "fold_4test_blogs.txt",
                                 "fold_5test_blogs.txt"),
                    ofile_prefix="fold_", ofile_postfix="blogs_predict.txt",
              out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/") {
    fold_count <- length(test_lines_files)
    for(i in 1:fold_count) {
        cat("reading test partition indices...\n")
        fold_indices_name <- paste0(test_lines_dir, test_lines_files[i])
        test_indices <- read.table(fold_indices_name, sep = "\n")$V1
        cat("reading corpus lines...\n")
        corpus_lines <- read_lines(corp_urls[corp_type])
        corpus_lines <- corpus_lines[test_indices]
        cat("selecting unique random trigrams...\n")
        predict_trigrams <- getUniqueRandomNgrams(corpus_lines, npredict)
        ofile_name <- paste0(out_dir, ofile_prefix, i, ofile_postfix)
        cat("writing unique random trigrams to", ofile_name, "...\n")
        writeLines(predict_trigrams, ofile_name)
    }
}

#corpus_lines <- read_lines(corpus_urls[1])  # blogs default
#if(!exists('gamma_grid')) gamma_grid <- makeEmptyDataGrid()
# if(!exists('default_folds')) {
#     cat("reading fold data...\n")
#     default_folds <- readFolds(fold_paths)
# }
# write_freq=100; fold=1; predict_words_path=NULL; ggrid_start=1; itr_start=1
# corpus_type="blogs"; out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/"
# file_prefix="fold_"; ofile_postfix="cv_results.csv"

## Trains the word prediction model on a corpus of text.  Function makes a copy
## of the gamma_grid dataframe which is passed in, changes the name of the
## predacc column to acc, reads in a set of _ delimited trigrams, parses those
## trigrams into the bigram prefixes and trigram tails, and then for every 
## pair of (gamma2, gamma3) discounts in gamma_grid, makes prediction using the
## bigram prefixes as inputs.  The accuracy of each prediction is determined
## by comparing the output of the model with the trigram tail associated with
## bigram prefix which was used as input.
## 
## gamma_grid dataframe that is passed in and return a dataframe with the
## populated .  Three columns in gamma_grid are:
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
## validation - If TRUE, tells function to get predict words from validation
##              set.  If FALSE (default), predict words are assumed to be
##              obtained from the fold training set
trainFold <- function(gamma_grid, write_freq=100, fold=1,
                      predict_words_path=NULL, ggrid_start=1, itr_start=1,
                      corpus_type="blogs",
                      out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/",
                      file_prefix="fold_", validation=FALSE) {
    if(is.null(predict_words_path) && !validation) {
        predict_words_path <- paste0(out_dir, file_prefix, fold,
                                     corpus_type, "_predict.txt")
    } else {
        cat("Using validation predict words...\n")
        predict_words_path <- paste0(out_dir, file_prefix, fold,
                                     "valid_predict.txt")
    }
    predict_words <- read_lines(predict_words_path)
    nitrs <- length(predict_words)
    if(!exists("fold_ngrams")) { 
        fold_ngrams <- importFoldNgramtables()
        cat("Fold ngram table data read has completed at",
            as.character(Sys.time()), "\n")
    }
    out_file = ""
    if(validation) {
        out_file <- paste0(out_dir, "validation/cv_", corpus_type, "_", "fold", fold, "_itrs",
                           nitrs, ".csv")  # e.g. /validation/cv_blogs_fold1_itrs500.csv
    } else {
        out_file <- paste0(out_dir, "cv_", corpus_type, "_", "fold", fold, "_itrs",
                           nitrs, ".csv")  # e.g. cv_blogs_fold1_itrs500.csv
    }
    
    exp_results <- data.frame(gamma2=as.numeric(rep(-1, nrow(gamma_grid))),
                              gamma3=as.numeric(rep(-1, nrow(gamma_grid))),
                              acc=as.numeric(rep(-1, nrow(gamma_grid))),
                              predict=as.numeric(rep(-1, nrow(gamma_grid))),
                              success=as.numeric(rep(-1, nrow(gamma_grid))))
    # Get ngram tables for this fold.  The fold_ngrams list has the following
    # structure: k element outer list: one element per fold. Each list
    #            contains inner list of 3 items:
    #            1st inner list is the unigram frequency table
    #            2nd inner list is the bigram frequency table
    #            3rd inner list is the trigram frequency table
    unigs <- fold_ngrams[[fold]][[1]]
    bigrs <- fold_ngrams[[fold]][[2]]
    trigs <- fold_ngrams[[fold]][[3]]
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

getValidationResults <- function(paths_best_training=NULL) {
    # Load paths to best discount parameters for each fold found in training
    if(is.null(paths_best_training)) {
        paths_best_training = vector(mode="character", length=5)
        paths_best_training[1] = "https://www.dropbox.com/s/u15w787s9v6qcj8/fold1_best_train.csv?dl=1"
        paths_best_training[2] = "https://www.dropbox.com/s/gos25qrdjhvqght/fold2_best_train.csv?dl=1"
        paths_best_training[3] = "https://www.dropbox.com/s/fb7wpjmjnadh2h6/fold3_best_train.csv?dl=1"
        paths_best_training[4] = "https://www.dropbox.com/s/492kt8jfnf03v85/fold4_best_train.csv?dl=1"
        paths_best_training[5] = "https://www.dropbox.com/s/ul23lcb8ln80gdd/fold5_best_train.csv?dl=1"
    }
    # Generate the inputs to the prediction model from the validation sets
    makePredictTrigrams(corp_type="blogs", npredict=500, corp_urls=corpus_urls,
                        test_lines_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/",
                        test_lines_files=c("fold_1blogs.txt", "fold_2blogs.txt",
                                           "fold_3blogs.txt", "fold_4blogs.txt",
                                           "fold_5blogs.txt"),
                        ofile_prefix="fold_", ofile_postfix="valid_predict.txt",
                        out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/")
    for(fold in 1:length(paths_best_training)) {
        gg <- read.csv(paths_best_training[fold])
        gg_results <- trainFold(gg, 100, fold,
                                out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/",
                                validation=TRUE)
    }
}

# training results
# f1 <- read.csv('https://www.dropbox.com/s/524dgfw0ej2d4m3/cv_blogs_fold1_itrs500.csv?dl=1')
# f2 <- read.csv('https://www.dropbox.com/s/2k5ypy3sovn6g9d/cv_blogs_fold2_itrs500.csv?dl=1')
# f3 <- read.csv('https://www.dropbox.com/s/85ex5o9km20014m/cv_blogs_fold3_itrs500.csv?dl=1')
# f4 <- read.csv('https://www.dropbox.com/s/5rf7mbhrpq617e9/cv_blogs_fold4_itrs500.csv?dl=1')
# f5 <- read.csv('https://www.dropbox.com/s/vxq9bl6bmagv1x2/cv_blogs_fold5_itrs500.csv?dl=1')
# validation results
# f1 <- read.csv('https://www.dropbox.com/s/t8kv3eultbrg5b4/cv_blogs_fold1_itrs500.csv?dl=1')
# f2 <- read.csv('https://www.dropbox.com/s/va3uq4f1h6fb1n8/cv_blogs_fold2_itrs500.csv?dl=1')
# f3 <- read.csv('https://www.dropbox.com/s/rxuviidj7gb16k1/cv_blogs_fold3_itrs500.csv?dl=1')
# f4 <- read.csv('https://www.dropbox.com/s/dzser8x5dcgevkn/cv_blogs_fold4_itrs500.csv?dl=1')
# f5 <- read.csv('https://www.dropbox.com/s/n4d4mj0rcryfvin/cv_blogs_fold5_itrs500.csv?dl=1')

## Converts a dataframe of xyz values to a matrix which can be consumed by
## the r base graphic function contour
convertDfToContourMatrix <- function(df, xname, yname, zname) {
    # http://stackoverflow.com/questions/7531868
    names(df)[names(df) == xname] <- 'x'
    names(df)[names(df) == yname] <- 'y'
    names(df)[names(df) == zname] <- 'z'
    x_values <- sort(unique(df$x))
    y_values <- sort(unique(df$y))
    cont_mat <- matrix(-1, nrow = length(x_values), ncol = length(y_values))
    rownames(cont_mat) <- x_values
    colnames(cont_mat) <- y_values
    index <- 1
    for(ix in 1:length(x_values)) {
        for(jy in 1:length(y_values)) {
            cont_mat[ix, jy] <- df$z[index]
            index <- index + 1
        }
    }
    
    return(cont_mat)
}


# xgrid1 <- sort(unique(f1$gamma2))
# ygrid1 <- sort(unique(f1$gamma3))
# cmat1 <- convertDfToContourMatrix(f1, 'gamma2', 'gamma3', 'acc')
# contour(x=xgrid1, y=ygrid1, cmat1, xlab='gamma2', ylab='gamma3')

# createContour <- function(data) {
#     xgrid <- sort(unique(data$gamma2))
#     ygrid <- sort(unique(data$gamma3))
#     cmat <- convertDfToContourMatrix(data, 'gamma2', 'gamma3', 'acc')
#     contour(x=xgrid, y=ygrid, cmat, xlab='gamma2', ylab='gamma3')
# }
# 
# cm1 <- convertDfToContourMatrix(f1, 'gamma2', 'gamma3', 'acc')
# cm2 <- convertDfToContourMatrix(f2, 'gamma2', 'gamma3', 'acc')
# cm3 <- convertDfToContourMatrix(f3, 'gamma2', 'gamma3', 'acc')
# cm4 <- convertDfToContourMatrix(f4, 'gamma2', 'gamma3', 'acc')
# cm5 <- convertDfToContourMatrix(f5, 'gamma2', 'gamma3', 'acc')

# comp_mat <- cm1+cm2+cm3+cm4+cm5
# xgrid <- sort(unique(f1$gamma2))  # could use any of the 5 fold results
# ygrid <- sort(unique(f1$gamma3))
# contour(x=xgrid, y=ygrid, comp_mat, xlab='gamma2', ylab='gamma3')

## Convince myself that:
## fold_1blogs.txt       - validation indices
## fold_1train_blogs.txt - training indices in the non-validation partition
## fold_1test_blogs.txt  - testing indices in the non-validation partition
testConsistency <- function() {
    f1_valid_path <- "https://www.dropbox.com/s/fo7ybvw4h7de2e8/fold_1blogs.txt?dl=1"
    f1_train_path <- "https://www.dropbox.com/s/n4cne11vjodd6lg/fold_1train_blogs.txt?dl=1"
    f1_test_path <- "https://www.dropbox.com/s/ezlxnaxwfb7df4x/fold_1test_blogs.txt?dl=1"
    
    f1_valid_indices <- read.table(f1_valid_path, sep = "\n")$V1
    f1_train_indices <- read.table(f1_train_path, sep = "\n")$V1
    f1_test_indices <- read.table(f1_test_path, sep = "\n")$V1
    
    inter1 <- intersect(f1_valid_indices, f1_train_indices) # empty, check
    inter2 <- intersect(f1_valid_indices, f1_test_indices)  # empty, check
    inter3 <- intersect(f1_train_indices, f1_test_indices)  # empty, check
    all_indices <- union(f1_valid_indices, f1_train_indices)
    all_indices <- union(all_indices, f1_test_indices)
    all_indices <- sort(all_indices)
    
    return(all_indices)  # length(all_indices) >> [1] 1689507 check!
}

## Returns base_df dataframe using sub_df rows where ever the x and y column
## values are the same.
## Precondition: 1) base_df and sub_df have the same first 3 columns:
##                  x, y, and z
##               2) base_df has all the combinations of x and y column values
##                  as sub_df
expandToGrid <- function(base_df, sub_df, x="gamma2", y="gamma3", z="acc",
                         tol=0.001) {
    return_df <- base_df
    for (i in 1:nrow(sub_df)) {
        x_val <- sub_df[i, x]
        y_val <- sub_df[i, y]
        z_val <- sub_df[i, z]
        cat("checking: x=", x_val, ", y=", y_val, ", z=", z_val, "\n")
        for (j in 1:nrow(return_df)) {
            if((x_val - return_df[j, x]) < tol &&
               (y_val - return_df[j, y]) < tol) {
                cat('    **** found match:', x_val, y_val, z_val, "****\n")
                return_df[j, z] <- z_val
                break
            }
        }
    }
    
    return(return_df)
}

## Expands the validation results to the original grid size and writes out these
## expanded results to files.
expandAllValidationResults <- 
function(out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/validation/",
         validation_results_files=
         c('https://www.dropbox.com/s/t8kv3eultbrg5b4/cv_blogs_fold1_itrs500.csv?dl=1',
           'https://www.dropbox.com/s/va3uq4f1h6fb1n8/cv_blogs_fold2_itrs500.csv?dl=1',
           'https://www.dropbox.com/s/rxuviidj7gb16k1/cv_blogs_fold3_itrs500.csv?dl=1',
           'https://www.dropbox.com/s/dzser8x5dcgevkn/cv_blogs_fold4_itrs500.csv?dl=1',
           'https://www.dropbox.com/s/n4d4mj0rcryfvin/cv_blogs_fold5_itrs500.csv?dl=1')) {
    
    empty_df <- makeEmptyDataGrid(default_fill = 0, col3name = "acc")
    for(i in 1:length(validation_results_files)) {
        vfold_results <- read.csv(validation_results_files[i])
        exp_results <- expandToGrid(empty_df, vfold_results)
        opath <- paste0(out_dir, "fold", i, "valid_exp.csv")
        cat(">>> writing output to:", opath, "<<<\n")
        write.csv(exp_results, opath, row.names = FALSE)
    }
}

## Returns a dataframe that has summed all the accuracy values (acc column) in
## each of the files in the results_files vector.
aggregateFolds <-
function(out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/validation/",
         results_files=
             c('https://www.dropbox.com/s/9n44g9d36yysn06/fold1valid_exp.csv?dl=1',
               'https://www.dropbox.com/s/dnw7zzizoc27mfa/fold2valid_exp.csv?dl=1',
               'https://www.dropbox.com/s/qhfnoygwueodz0u/fold3valid_exp.csv?dl=1',
               'https://www.dropbox.com/s/deu5zqrra4livt8/fold4valid_exp.csv?dl=1',
               'https://www.dropbox.com/s/lkgqo1xurtvllp2/fold5valid_exp.csv?dl=1')) {
    
    g2_vector <- read.csv(results_files[1])$gamma2
    g3_vector <- read.csv(results_files[1])$gamma3
    acc_totals <- vector(mode = "numeric", length = length(g3_vector))
    for(file in results_files) {
        acc_totals <- acc_totals + read.csv(file)$acc  # just add acc vector
    }
    
    return(data.frame(gamma2=g2_vector, gamma3=g3_vector, acc=acc_totals))
}

## Returns dataframe with 2 columns: trigram and trial
getTestTrigrams <- function(trigram_path='',
                            trigram_sample_count=500,
                            trial_count=100) {
    
}

## Cleans a corpus file at corpus_path as described at:
## http://rpubs.com/mszczepaniak/predictkbo1preproc
## and writes the results to out_path/out_name. The function then creates the
## 
## By default, only blogs test file is cleaned.
cleanTestData <- function(preManual=TRUE,
                          out_prefix=c("en_US.blogs.test"),
                          corpus_path='https://www.dropbox.com/s/8hgb7kfl0gnngyh/en_US.blogs.test.txt?dl=1',
                          out_path='D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/non_train/',
                          preproc_path='D:/dev/PredictNextKBO/preprocess/PreEda.R',
                          ngrams_path='D:/dev/PredictNextKBO/modeldev/Ngrams.R') {
    
    source(preproc_path)
    
    if(preManual) {
        cat("cleanTestData in pre-manual mode START AT:",
            as.character(Sys.time()), "\n")
        # create the en_US.blogs.test.1sents.txt and en_US.blogs.test.2sents.txt
        # files as described in the "Sentence Parsing" section
        parseSentsToFile('blogs', out_path, out_path, FALSE)
        cat("cleanTestData in pre-manual mode FINISH AT:",
            as.character(Sys.time()), "\n")
    } else {
        cat("cleanTestData in post-manual mode START AT:",
            as.character(Sys.time()), "\n")
        out_prefix <- c("en_US.blogs.test")  # only process blogs
        # after running the 8 steps using Notepad++, create en_US.blogs.train.3ascii.txt
        inpost <- '.2sents.txt'
        outpost <- '.3ascii.txt'
        runFilterAndWrite(convertToAscii, out_path, inpost, outpost, out_prefix)
        
        # create the en_US.blogs.test.4notags.txt file
        inpost <- '.3ascii.txt'
        outpost <- '.4notags.txt'
        runFilterAndWrite(convertUnicodeTags, out_path, inpost, outpost, out_prefix)
        
        # create the en_US.blogs.test.5nourls.txt file
        inpost <- '.4notags.txt'
        outpost <- '.5nourls.txt'
        runFilterAndWrite(removeUrls, out_path, inpost, outpost, out_prefix)
        
        # create the en_US.blogs.test.6preeos.txt file
        inpost <- '.5nourls.txt'
        outpost <- '.6preeos.txt'
        runFilterAndWrite(preEosClean, out_path, inpost, outpost, out_prefix)
        
        # create en_US.blogs.test.7eos.txt file
        inpost <- '.6preeos.txt'
        outpost <- '.7eos.txt'
        runFilterAndWrite(addEosMarkers, out_path, inpost, outpost, out_prefix)
        
        # create en_US.blogs.test.8posteos.txt file
        inpost <- '.7eos.txt'
        outpost <- '.8posteos.txt'
        runFilterAndWrite(postEosClean, out_path, inpost, outpost, out_prefix)
        cat("cleanTestData in post-manual mode FINISH AT:",
            as.character(Sys.time()), "\n")
        
    }
    
}

## Returns a numeric vector of nitrs prediction accuracies where each value is
## calculated by making samples_per_iter number of predictions.  PARAMETERS:
## gamma2 - Absolute bigram discount rate. Default = 1.6
## gamma3 - Absolute trigram discount rate. Default = 0.1
## samples_per_iter - Number of samples taken per perdiction accuracy
##                    calculation iteration. Default is 500.
## nitrs - Number of prediction accuracy estimates or iterations to make. For
##         each iteration, samples_per_iter samples are taken to make each 
##         prediction accuracy estimate. Default is 100.
## unigram_path - path to the unigram frequency table
## bigram_path - path to the bigram frequency table
## trigram_path - path to the trigram frequency table
## 
getTestEstimates <- function(gamma2=1.6, gamma3=0.1, write_freq=100,
                             res_file_name='en_US.blogs.test.acc_estimate.csv',
                             pred_file_name='predictions.csv',
                             samples_per_iter=500, nitrs=100, seed_val=711,
                             path_corpus='https://www.dropbox.com/s/h49r5hlexhb375n/en_US.blogs.test.8posteos.txt?dl=1',
                             path_unigrams='https://www.dropbox.com/s/3iv0licjiu1g1t4/fold_5train_blogs_1grams.csv?dl=1',
                             path_bigrams='https://www.dropbox.com/s/znmtr4qsk49yysb/fold_5train_blogs_2grams.csv?dl=1',
                             path_trigrams='https://www.dropbox.com/s/euo1dxrn8iu6b68/fold_5train_blogs_3grams.csv?dl=1',
                             dat_dir='D:/Dropbox/sw_dev/projects/PredictNextKBO/data/en_US/non_train/',
                             pred_trigs_path='https://www.dropbox.com/s/yt369pf6p4oagni/en_US.blogs.test.50000_predict_trigrams_seed711.txt?dl=1') {
    source('KboCv.R')
    seedv <- seed_val
    acc_ests <- vector(mode='numeric', length=nitrs)
    cat('Reading test corpus at       ', as.character(Sys.time()), "\n")
    corp_lines <- read_lines(path_corpus)
    cat('Reading of unigram tables at ', as.character(Sys.time()), "\n")
    unigs <- read.csv(path_unigrams)
    cat('Reading of bigram tables at  ', as.character(Sys.time()), "\n")
    bigrs <- read.csv(path_bigrams)
    cat('Reading of trigram tables at ', as.character(Sys.time()), "\n")
    trigs <- read.csv(path_trigrams)
    out_results_path <- paste0(dat_dir, res_file_name)
    out_pred_path <- paste0(dat_dir, pred_file_name)
    predict_trigs <- NULL
    if(is.null(pred_trigs_path)) {
        cat('BUILDING prediction trigrams at  ', as.character(Sys.time()), "\n")
        predict_trigs <- getUniqueRandomNgrams(corp_lines, samples_per_iter * nitrs,
                                               3, '_', seedv)
        cat(length(predict_trigs), ' prediction trigrams BUILT... ', as.character(Sys.time()), "\n")
    } else {
        cat('READING prediction trigrams at   ', as.character(Sys.time()), "\n")
        predict_trigs <- read_lines(pred_trigs_path)  # must faster than regenerating...
        cat(length(predict_trigs), 'prediction trigrams READ   ', as.character(Sys.time()), "\n")
    }
    
    pred_index <- 0
    predictions <- data.frame(trigram=predict_trigs, predict="", correct=-1,
                              itr=-1, samp=-1)
    for(i in 1:nitrs) {
        good_predictions <- 0
        accuracy <- -1
        for(j in 1:samples_per_iter) {
            pred_index <- pred_index + 1
            ttp <- predict_trigs[pred_index]  # target to predict
            target_word <- str_split_fixed(ttp, "_", 3)[1,3]
            bigPre <- paste(str_split_fixed(ttp, "_", 3)[1,1:2],
                            collapse = "_")
            top_pred <- getTopPrediction(bigPre, gamma2, gamma3,
                                         unigs, bigrs, trigs)
            predictions$predict[pred_index] <- top_pred
            predictions$itr[pred_index] <- i
            predictions$samp[pred_index] <- j
            pred <- (target_word == top_pred) + 0
            predictions$correct[pred_index] <- pred
            good_predictions <- good_predictions + pred
            # if(target_word == top_pred) {
            #     cat(good_predictions, ttp, 'matched', top_pred, "\n")
            # }
            accuracy <- good_predictions / j
            if(j %% write_freq == 0) {
                console_msg <- paste0("iteration ", j, ", ",
                                      "good_predictions", "=", good_predictions, ", ",
                                      "out of ", j, ", accuracy=", accuracy, " at ",
                                      as.character(Sys.time()), "\n")
                cat(console_msg)
                write.csv(predictions, out_pred_path, row.names = FALSE)
            }
        }
        cat("-----------------------------------------------------\n")
        acc_ests[i] <- accuracy
        write.table(acc_ests, out_results_path, row.names=FALSE, col.names=FALSE)
    }
    
}

