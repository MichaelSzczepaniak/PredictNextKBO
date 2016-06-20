# Example of merging 2 contingency tables. Adapted from:
# http://stackoverflow.com/questions/29163591/merging-two-contingency-tables
# Start by creating some data to illustrate how it works:
first <- structure(c(48, 1, 1, 1, 1, 1, 1, 6), .Dim = 8L, 
                   .Dimnames = list(
                       c("0", "5", "11", "17", "19", "50", "95", "100")), class = "table")

second <- structure(c(67, 1, 1, 1, 1, 2, 2, 2, 8), 
                    .Dim = 9L, .Dimnames = list(
                        c("0", "11", "17", "30", "33", "50", "67", "83", "100")), 
                    class = "table")
# create 3rd table with all levels in the first and second, but with 0 counts
Un <- union(names(first), names(second))
third <- as.table(setNames(rep(0, length(Un)), Un))

# build table like first, but with all factors
first1 <- c(first, third)
first2 <- first1[!duplicated(names(first1))]
firstFull <- as.table(first2[order(as.numeric(names(first2)))])
# do the same for the second table
second1 <- c(second, third)
second2 <- second1[!duplicated(names(second1))]
secondFull <- as.table(second2[order(as.numeric(names(second2)))])
# now add the two tables together now that they have the same structure
thirdFull <- firstFull + secondFull

# Now let's adapt this to work with character labels
first <- structure(c(48, 1, 1, 1, 1, 1, 1, 6), .Dim = 8L, 
                   .Dimnames = list(
                       c("a", "b", "c", "d", "e", "h", "k", "l")), class = "table")

second <- structure(c(67, 1, 1, 1, 1, 2, 2, 2, 8), 
                    .Dim = 9L, .Dimnames = list(
                        c("a", "c", "d", "f", "g", "h", "i", "j", "l")), 
                    class = "table")

## Merges two contigency tables firstTab and secondTab and returns the merged table
mergeContingencyTables <- function(firstTab, secondTab, char.labels=TRUE) {
    # normalize the first table
    Un <- union(names(firstTab), names(secondTab))
    third <- as.table(setNames(rep(0, length(Un)), Un))
    firstTab1 <- c(firstTab, third)
    firstTab2 <- firstTab1[!duplicated(names(firstTab1))]
    if(char.labels) {
        firstTabFull <- as.table(firstTab2[order(names(firstTab2))])
    } else {
        firstTabFull <- as.table(firstTab2[order(as.numeric(names(firstTab2)))])
    }
    # normalize the second table
    secondTab1 <- c(secondTab, third)
    secondTab2 <- secondTab1[!duplicated(names(secondTab1))]
    if(char.labels) {
        secondTabFull <- as.table(secondTab2[order(names(secondTab2))])
    } else {
        secondTabFull <- as.table(secondTab2[order(as.numeric(names(secondTab2)))])
    }
    
    thirdFull <- firstTabFull + secondTabFull
    
    return(thirdFull)
}

