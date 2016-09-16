
rm(list = ls())
source("../predictnextkbo/Katz.R")
source("KboCv.R")


## Make a test data grid
gamma_grid <- makeEmptyDataGrid(g2_start=0.1, g2_end=1.9, g3_start=0.1,
                               g3_end=1.9, intv=0.1)

corpus_data <- c("https://www.dropbox.com/s/9dx3oo1w5uf8n1t/en_US.blogs.train.8posteos.txt?dl=1",
                 "https://www.dropbox.com/s/54cvi36161y6pvk/en_US.news.train.8posteos.txt?dl=1",
                 "https://www.dropbox.com/s/6ayhavfnzs5lmqa/en_US.twitter.train.8posteos.txt?dl=1")

corpus_lines <- read_lines(corpus_data[1])

# ng_paths=c("https://www.dropbox.com/s/033qzeiggmcauo9/en_US.blogs.train.12unigrams.nosins.csv?dl=1",
#            "https://www.dropbox.com/s/6cgqa487xb0srbt/en_US.blogs.train.13bigrams.nosins.csv?dl=1",
#            "https://www.dropbox.com/s/z0rz707mt3da1h1/en_US.blogs.train.14trigrams.nosins.csv?dl=1")

readFolds <- function(fold_paths) {
    folds <- vector("list", length(fold_paths))
    for(i in 1:length(fold_paths)) {
        path <- fold_paths[i]
        fold <- read.csv(path, header=FALSE)
        folds[[i]] <- fold
    }
    
    return(folds)
}

fold_paths <- c("D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_1.txt",
                "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_2.txt",
                "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_3.txt",
                "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_4.txt",
                "D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/fold_5.txt")

default_folds <- readFolds(fold_paths)

## Runs K-Fold CV on corpus_train and returns a list of best pairs of (gamma2,
## gamma3) with the highest prediction accuracy for each validation fold
runKfoldTrials <- function(corpus_train, gamma_grid, ggrid_start=1,
                           folds=default_folds, fold_start=1, nitrs=200,
                           kfolds=5, out_dir="./", seed_val=719) {
    best_gammas <- data.frame(vfold=seq(1, 5), gamma2=rep(-1, 5),
                              gamma3=rep(-1, 5), pred_acc=rep(-1, 5))
    set.seed(seed_val)  # set for reproducibility
    # folds <- makeFolds(length(corpus_lines), kfolds) # writes fold indices
    out_file <- paste0("out_dir", "cv_results.csv")
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
        
        # This is the actual training steps.  Take nitrs lines in training set
        # that weren't used to build n-gram tables and make predictions using 
        # each (gamma2, gamma3) pair in gamma_grid
        trigrams_to_predict <- getRandomTrigrams(train_gammas_data, nitrs)
        # [[1]] accuracy, [[2]] gamma2, [[3]] gamma3
        exp_results <- list(acc=as.numeric(seq(1, nrow(gamma_grid))),
                            gamma2=as.numeric(seq(1, nrow(gamma_grid))),
                            gamma3=as.numeric(seq(1, nrow(gamma_grid))))
        for(experiment in ggrid_start:nrow(gamma_grid)) {
            good_predictions <- 0
            gam2 <- gamma_grid$gamma2[experiment]
            gam3 <- gamma_grid$gamma3[experiment]
            # Now try to predict each of the unknown trigram tails with
            # current (gam2, gam3):
            cat("START", nitrs, "predictions with gamma2=", gam2,
                "and gamma3=", gam3, "at ", as.character(Sys.time()), "\n")
            for(ttp in trigrams_to_predict) {
                target_word <- str_split_fixed(ttp, "_", 3)[1,3]
                bigPre <- paste(str_split_fixed(ttp, "_", 3)[1,1:2],
                                collapse = "_")
                top_pred <- getTopPrediction(bigPre, gam2, gam3,
                                             unigs, bigrs, trigs)
                good_predictions <- good_predictions + (target_word == top_pred)
            }
            accuracy <- good_predictions / nitrs
            exp_results[[1]][experiment] <- accuracy
            exp_results[[2]][experiment] <- gam2
            exp_results[[3]][experiment] <- gam3
            cat("*** FINISH", nitrs, "predictions with accurarcy =", accuracy,
                " at time =", as.character(Sys.time()), "***\n")
        }
        acc_max_index <- which.max(exp_results[[1]])
        best_gammas$vfold[f] <- f
        best_gammas$gamma2[f] <- exp_results[[1]][acc_max_index]
        best_gammas$gamma3[f] <- exp_results[[2]][acc_max_index]
        best_gammas$pred_acc[f] <- exp_results[[3]][acc_max_index]
        write.csv(best_gammas, out_file, row.names = FALSE)
        cat("results written to:", out_file, "\n")
    }
    
    
    
    return(best_gammas)
}


