setwd("~/Desktop/R/Coursera/3_Getting&Cleaning_Data/Stockfish Data Analysis")
library(data.table)
library(readr)
chessData <- read_file("StockfishNeue.txt")
chessData <- gsub(pattern = " 1. ", replacement = "tobesub 1. ", chessData)
moves <- strsplit(chessData, split = "tobesub")  ## "tobesub" is a handle
moves <- unlist(moves)[-1]
class(moves)
length(moves)  ## 33 games were played
sum(grepl("#", moves))  ## 6 games ended with checkmate


### Moves from two sides are combined
firstXMovesCombined <- function(a,b) {
    games <- list()
    titles <- vector()
    for(i in (1:length(moves))) {
        title <- paste("Game", as.character(i), sep = "")
        x <- unlist(strsplit(moves[i],
                    split=" ([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-9][0-9])[.] "))[a:b+1]
        y <- as.vector(x)
        games <- cbind(games, y)
        titles <- c(titles, title)
    }
    colnames(games) <- titles
    games
}
firstXMovesCombined(1,5)


### Moves from two sides separated
firstXMovesSep <- function(a,b) {
    games <- list()
    titles <- vector()
    for(i in (1:length(moves))) {
        title <- paste("Game", as.character(i), sep = "")
        x <- unlist(strsplit(moves[i],
                             split=" ([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-9][0-9])[.] "))[a:b+1]
        y <- strsplit(x, split = " ")
        z <- t(as.data.frame(y))
        colnames(z) <- c("White", "Black")
        rownames(z) <- NULL
        games <- array(c(games, z), dim = c(b-a+1, 2, i))
        titles <- c(titles, title)
    }
    dimnames(games) <- list(a:b, c("White", "Black"), titles)
    games
}
firstXMovesSep(1,5)



