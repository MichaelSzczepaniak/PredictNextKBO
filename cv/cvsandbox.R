


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
    fold_paths <- data.frame(blogs=blogs_paths, news=news_paths, twitter=twitr_paths)
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

## Make a test data grid
gamma_grid <- makeEmptyDataGrid(g2_start=0.1, g2_end=1.9, g3_start=0.1,
                               g3_end=1.9, intv=0.1)
## Read folds data
cat("reading fold data...\n")
default_folds <- readFolds(fold_paths)

# ng_paths=c("https://www.dropbox.com/s/033qzeiggmcauo9/en_US.blogs.train.12unigrams.nosins.csv?dl=1",
#            "https://www.dropbox.com/s/6cgqa487xb0srbt/en_US.blogs.train.13bigrams.nosins.csv?dl=1",
#            "https://www.dropbox.com/s/z0rz707mt3da1h1/en_US.blogs.train.14trigrams.nosins.csv?dl=1")


## Returns a list of int vectors which are indices of training and testing sets 
## in the non-validation partition.  The odd numbered lists contain the indices
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
## writes that out to files.
## corp_data - 
## ng - int vector specifying the vectors to be created
## folds - 
## cv_dir -
## ofile_prefix - 
## ofile_postfix - 
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

makePredictTrigrams <- function(corp_types=c("blogs", "news", "twitter"),
                                npredict=500,
              test_lines_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/",
              test_lines_files=c("fold_1test.txt", "fold_2test.txt",
                                 "fold_3test.txt", "fold_4test.txt",
                                 "fold_5test.txt"), folds=1:5,
                    ofile_prefix="fold_", ofile_postfix="predict.txt",
              out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/") {
    
}

trainFold <- function(fold, corp_type, train_data_path, test_data_path,
                      ngram_tables, predict_words_path,
                      out_dir="D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/",
                      ofile_prefix="fold_", ofile_postfix="cv_results.csv") {
    
}

default_folds <- readFolds(fold_paths)
out_default <- "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/"
topn=3; corpus_type="blogs"
ggrid_start=1; folds=default_folds; fold_start=1; nitrs=500
kfolds=5; out_dir=out_default; seed_val=719

## Runs K-Fold CV on corpus_lines and returns a data.frame of topn of best
## pairs of (gamma2, gamma3) with the highest prediction accuracy for each
## validation fold
runKfoldTrials <- function(corpus_lines, gamma_grid, corpus_type="blogs",
                           topn=3, ggrid_start=1, folds=default_folds,
                           fold_start=1, nitrs=500,
                           kfolds=5, out_dir=out_default,
                           new_seeds=TRUE, seed_val=719) {
    best_gammas <- data.frame(vfold=rep(1:kfolds, each=topn),
                              rank=rep(1:topn, kfolds),
                              gamma2=rep(-1, kfolds*topn),
                              gamma3=rep(-1, kfolds*topn),
                              pred_acc=rep(-1, kfolds*topn))
    out_file <- paste0(out_dir, "cv_", corpus_type, "_", kfolds, "fold_",
                       nitrs, ".csv")
    for(f in fold_start:length(folds)) {
        valid_fold_indices <- folds[[f]]
        valid_data <- corpus_lines[valid_fold_indices]
        train_data <- corpus_lines[-valid_fold_indices]
        
        # take 80% of training data to build n-gram tables
        set.seed(getNextSeed(f))  # set for reproducibility
        train_ngrams_indices <- sample(1:length(train_data), 0.8*length(train_data))
        train_ngrams_data <- train_data[train_ngrams_indices]
        train_gammas_data <- train_data[-train_ngrams_indices]
        unigs <- getNgramTables(1, train_ngrams_data)  # ~ 1 min for blogs
        unigs <- filter(unigs, freq > 1)
        bigrs <- getNgramTables(2, train_ngrams_data)  # ~ 4 mins for blogs
        bigrs <- filter(bigrs, freq > 1)
        trigs <- getNgramTables(3, train_ngrams_data)  # ~ 8.5 mins
        trigs <- filter(trigs, freq > 1)
        
        # This is the actual training steps.  Take nitrs trigram samples from
        # the training setthat weren't used to build n-gram tables and make
        # predictions using each (gamma2, gamma3) pair in gamma_grid.
        trigrams_to_predict <- getUniqueRandomTrigrams(train_gammas_data, nitrs)
        # experiment results:
        exp_results <- data.frame(gamma2=as.numeric(rep(-1, nrow(gamma_grid))),
                                  gamma3=as.numeric(rep(-1, nrow(gamma_grid))),
                                  acc=as.numeric(rep(-1, nrow(gamma_grid))))
        for(experiment in ggrid_start:nrow(gamma_grid)) {
            good_predictions <- 0
            gam2 <- gamma_grid$gamma2[experiment]
            gam3 <- gamma_grid$gamma3[experiment]
            # Now try to predict each of the unknown trigram tails with
            # current (gam2, gam3):
            for(ttp in trigrams_to_predict) {
                target_word <- str_split_fixed(ttp, "_", 3)[1,3]
                bigPre <- paste(str_split_fixed(ttp, "_", 3)[1,1:2],
                                collapse = "_")
                top_pred <- getTopPrediction(bigPre, gam2, gam3,
                                             unigs, bigrs, trigs)
                good_predictions <- good_predictions + (target_word == top_pred)
            }
            accuracy <- good_predictions / nitrs
            exp_results$acc[experiment] <- accuracy
            exp_results$gamma2[experiment] <- gam2
            exp_results$gamma3[experiment] <- gam3
            out_line <- sprintf("%s%s%s%s%s%s%s%s", gam2, ",",gam3, ",",
                                accuracy, ",",  as.character(Sys.time()), "\n")
            cat(out_line)  # feedback for during very long set of computations
        }
        # get the topn most accurate predictions
        exp_results <- arrange(exp_results, desc(acc))[1:topn,]
        # load topn results
        start_block <- ((f - 1) * topn) + 1
        end_block <- f * topn
        best_gammas$vfold[start_block:end_block] <- f
        best_gammas$gamma2[start_block:end_block] <- exp_results$gamma2[1:topn]
        best_gammas$gamma3[start_block:end_block] <- exp_results$gamma3[1:topn]
        best_gammas$pred_acc[start_block:end_block] <- exp_results$acc[1:topn]
        
        write.csv(best_gammas, out_file, row.names = FALSE)
        cat("results written to:", out_file, "\n")
    }
    
    return(best_gammas)
}


