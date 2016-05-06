# install packages if needed
list.of.packages <- c("dplyr", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
libs <- c("dplyr", "readr")
lapply(libs, require, character.only=TRUE)  # load libs
options(stringsAsFactors = FALSE)  # strings are what we are operating on...
# set parameters
dataDir <- "../data/en_US/"
filenames <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
fullpaths <- sprintf("%s%s", dataDir, filenames)
## Read data
getLocalDataLines <- function(fileName, file.is.path=TRUE, dataDir=NULL) {
    fileLines <- NULL
    filePath <- fileName
    if(!file.is.path) { filePath <- sprintf("%s%s", dataDir, fileName) }
    fileLines <- read_lines(filePath)
    
    return(fileLines)
}