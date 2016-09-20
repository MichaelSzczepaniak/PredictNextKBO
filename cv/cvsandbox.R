
rm(list = ls())
source("../predictnextkbo/Katz.R")
source("KboCv.R")


## Make a test data grid
gamma_grid <- makeEmptyDataGrid(g2_start=0.1, g2_end=1.9, g3_start=0.1,
                               g3_end=1.9, intv=0.1)

corpus_data <- c("https://www.dropbox.com/s/9dx3oo1w5uf8n1t/en_US.blogs.train.8posteos.txt?dl=1",
                 "https://www.dropbox.com/s/54cvi36161y6pvk/en_US.news.train.8posteos.txt?dl=1",
                 "https://www.dropbox.com/s/6ayhavfnzs5lmqa/en_US.twitter.train.8posteos.txt?dl=1")

corpus_ <- read_lines(corpus_data[1])

# ng_paths=c("https://www.dropbox.com/s/033qzeiggmcauo9/en_US.blogs.train.12unigrams.nosins.csv?dl=1",
#            "https://www.dropbox.com/s/6cgqa487xb0srbt/en_US.blogs.train.13bigrams.nosins.csv?dl=1",
#            "https://www.dropbox.com/s/z0rz707mt3da1h1/en_US.blogs.train.14trigrams.nosins.csv?dl=1")

fold_paths <- c("D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_1blogs.txt",
                "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_2blogs.txt",
                "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_3blogs.txt",
                "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_4blogs.txt",
                "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_5blogs.txt")

readFolds <- function(fold_paths) {
    folds <- vector("list", length(fold_paths))
    for(i in 1:length(fold_paths)) {
        path <- fold_paths[i]
        fold <- read.csv(path, header=FALSE)
        folds[[i]] <- fold$V1
    }
    
    return(folds)
}

default_folds <- readFolds(fold_paths)
out_default <- "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/"
topn=3; corpus_type="blogs"
ggrid_start=1; folds=default_folds; fold_start=1; nitrs=200
kfolds=5; out_dir=out_default; seed_val=719

## Runs K-Fold CV on corpus_lines and returns a data.frame of topn of best
## pairs of (gamma2, gamma3) with the highest prediction accuracy for each
## validation fold
runKfoldTrials <- function(corpus_lines, gamma_grid, corpus_type="blogs",
                           topn=3, ggrid_start=1, folds=default_folds,
                           fold_start=1, nitrs=200,
                           kfolds=5, out_dir=out_default, seed_val=719) {
    best_gammas <- data.frame(vfold=rep(1:kfolds, each=topn),
                              rank=rep(1:topn, kfolds),
                              gamma2=rep(-1, kfolds*topn),
                              gamma3=rep(-1, kfolds*topn),
                              pred_acc=rep(-1, kfolds*topn))
    set.seed(seed_val)  # set for reproducibility
    out_file <- paste0(out_dir, "cv_", corpus_type, "_", kfolds, "fold_",
                       nitrs, ".csv")
    for(f in fold_start:length(folds)) {
        valid_fold_indices <- folds[[f]]
        valid_data <- corpus_lines[valid_fold_indices]
        train_data <- corpus_lines[-valid_fold_indices]
        
        # take 80% of training data to build n-gram tables
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
            out_line <- sprintf("%s%s%s%s%s", gam2, ",",gam3, ",", accuracy)
            cat(out_line, ",", as.character(Sys.time()), "\n")
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


