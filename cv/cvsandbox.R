


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


## Returns a list of int vectors which are indices of training and testing sets 
## in the Non-Validation partition.  The odd numbered lists contain the indices
## of the lines used for the training set.  The even numbered lists contain
## the indices of the lines used for testing set.  All of these sets are
## assumed to be within the Non-Validation fold/partition.
##
## folds - list of int vectors of size length(folds), indices of
##         validation partitions, indices not in this set are futher
##         segmented in training and testing sets
## corp_type - character string, type of corpus: "blogs", "news", "twitter"
## train_fraction - float betwee 0 and 1 (non-inclusive), fraction of samples
##                  used for the test set, 
## seed_vals - seed values to use for train/test set selections
## ofile_prefix - string, output file name prefix
## ofile_postfix - string, output file name postfix
## out_dir - string, directory to write output files to
makeTrainTestNV <- function(folds=default_folds, corp_type="blogs",
                            train_fraction=0.8,
                            seed_vals=c(7,11,13,17,19),
                            ofile_prefix="fold_",
                            ofile_postfix=c("train.txt", "test.txt"),
                     out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/") {
    corpus_lines <- read_lines(corpus_urls[corp_type])
    fold_count <- length(folds) - 1  # because last count is total line count
    results <- list() #vector("list", 2*fold_count)
    ofile_paths <- vector(mode = "character")
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
        
        name_prefix <- sprintf("%s%s%s%s","fold_", i, "train_", corp_type)
        fname <- sprintf("%s%s%s", out_dir, name_prefix, ".txt")
        results[[name_prefix]] <- train_ngrams_indices
        write.table(train_ngrams_indices, fname, row.names=FALSE, col.names=FALSE)
        ofile_paths <- append(ofile_paths, fname)
        
        name_prefix <- sprintf("%s%s%s%s","fold_", i, "test_", corp_type)
        fname <- sprintf("%s%s%s", out_dir, name_prefix, ".txt")
        results[[name_prefix]] <- test_gammas_indices
        write.table(test_gammas_indices, fname, row.names=FALSE, col.names=FALSE)
        ofile_paths <- append(ofile_paths, fname)
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

## Creates and write out the trigrams used to determine prediction accuracy.
## corp_type - string, corpus data to process: "blogs", "news", "twitter"
## npredict - # of predictions to make per parameter set (gamma2, gamma3)
## corp_urls - char vector, urls of where the corpora data lives
## test_lines_dir - string, local dir where test partion indices live
## test_lines_files - char vector, file names of test indices
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

corpus_lines <- read_lines(corpus_urls[1])  # blogs default
if(!exists('gamma_grid')) gamma_grid <- makeEmptyDataGrid()
# if(!exists('default_folds')) {
#     cat("reading fold data...\n")
#     default_folds <- readFolds(fold_paths)
# }
# write_freq=100; fold=1; predict_words_path=NULL; ggrid_start=1; itr_start=1
# corpus_type="blogs"; out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/"
# file_prefix="fold_"; ofile_postfix="cv_results.csv"

## Runs K-Fold CV on corpus_lines and fills in the predacc column of gamma_grid
## dataframe that is passed in.  Three columns in gamma_grid are:
## gamma2 - bigram discount
## gamma3 - trigram discount
## predacc - prediction accuracy est'd from nitrs predicitons on each
##           (gamma2, gamma3) pair
## Function writes results to: out_dir/cv_<corpus_type>_<fold>fold_<nitrs>.csv
## E.g out_dir/cv_blogs_1fold_500.csv
##
## Precondition: Function assumes that fold_ngrams list is in the workspace.
##               If fold_ngrams is not in the workspace, it attempts to read
##               this data from out_dir\foldNgramTables.RData.
##               If this file can't be found, an error will occur.
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
                cat("g2=", g2, ", g3=", g3, "iteration", j,
                    "update written", as.character(Sys.time()), "\n")
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


