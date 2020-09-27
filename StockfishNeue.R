setwd("~/Desktop/R/Coursera/3_Getting&Cleaning_Data/Stockfish Data Analysis")
library(data.table)
library(readr)
library(plyr)
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
firstXMovesSep(1,10)


### Individual Games
if(!dir.exists("./GamesCatalog")) {
    dir.create("GamesCatalog")
}
cleanDF <- function(g) {
    White <- vector()
    Black <- vector()
    x <- unlist(strsplit(moves[g],
                    split=" ([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-9][0-9])[.] "))[-1]
    y <- strsplit(x, split = " ")
    z <- unlist(y)
    for(i in 1: length(z)) {
        if(i%%2 == 1) {
            White <- c(White, z[i])
        } else {
            Black <- c(Black, z[i])
        }
    }
    if (length(White) != length(Black)){
        Black <- c(Black, NA)
    }
    W <- data.frame(White)
    B <- data.frame(Black)
    cbind(W, B)
}

for(i in 1:length(moves)) { 
    if(grepl("#", cleanDF(i)[nrow(cleanDF(i)), 1]) == TRUE) {handle = "WhiteWins"
    } else if(grepl("#", cleanDF(i)[nrow(cleanDF(i)), 2]) == TRUE) {handle = "BlackWins"
    } else {handle = "Draw"
    }
    if(!file.exists(paste("./GamesCatalog/","Game", as.character(i),"-", handle, ".txt", sep = ""))) {
        write.table(cleanDF(i), quote = FALSE,
                    file = paste("./GamesCatalog/","Game", as.character(i),"-", handle, ".txt", sep = ""))}
}


### Classify by types of games - create Opening folder
if(!dir.exists("./Openings")) {
    dir.create("./Openings")
}

for(i in 1:length(moves)) {
    if(grepl("#", cleanDF(i)[nrow(cleanDF(i)), 1]) == TRUE) {handle = "White"
    } else if(grepl("#", cleanDF(i)[nrow(cleanDF(i)), 2]) == TRUE) {handle = "Black"
    } else {handle = "Draw"
    }
    if(cleanDF(i)[1,1]=="e4"&cleanDF(i)[1,2]=="c5") {
        opening = "Sicilian"
    } else if(cleanDF(i)[1,1]=="e4"&cleanDF(i)[1,2]=="e5"&cleanDF(i)[2,1]=="Nf3"&cleanDF(i)[2,2]=="Nc6"&cleanDF(i)[3,1]=="Bb5"){
        opening = "RuyLopez"
    } else if(cleanDF(i)[1,1]=="e4"&cleanDF(i)[1,2]=="e5"&cleanDF(i)[2,1]=="Nf3"&cleanDF(i)[2,2]=="Nc6"&cleanDF(i)[3,1]=="Bc4"){
        opening = "Italian"
    } else if(cleanDF(i)[1,1]=="e4"&cleanDF(i)[1,2]=="e6"){
        opening = "French"
    } else if(cleanDF(i)[1,1]=="e4"&cleanDF(i)[1,2]=="e5"&cleanDF(i)[2,1]=="Nf3"&cleanDF(i)[2,2]=="Nc6"&cleanDF(i)[3,1]=="d4"){
        opening = "Scotch"
    } else if(cleanDF(i)[1,1]=="e4"&cleanDF(i)[1,2]=="e5"&cleanDF(i)[2,1]=="Nf3"&cleanDF(i)[2,2]=="Nf6"){
        opening = "Petrov's"
    } else if(cleanDF(i)[1,1]=="e4"&cleanDF(i)[1,2]=="e5"&cleanDF(i)[2,1]=="Nf3"&cleanDF(i)[2,2]=="d6"){
        opening = "Philidor"
    } else if(cleanDF(i)[1,1]=="e4"&cleanDF(i)[1,2]=="e5"&cleanDF(i)[2,1]=="Nf3"&cleanDF(i)[2,2]=="Nc6"&cleanDF(i)[3,1]=="Nc3"&cleanDF(i)[3,2]=="Nf6"
              |cleanDF(i)[1,1]=="e4"&cleanDF(i)[1,2]=="e5"&cleanDF(i)[2,1]=="Nc3"&cleanDF(i)[2,2]=="Nc6"&cleanDF(i)[3,1]=="Nf3"&cleanDF(i)[3,2]=="Nf6"){
        opening = "FourKnights"
    } else if(cleanDF(i)[1,1]=="e4"&cleanDF(i)[1,2]=="e5"&cleanDF(i)[2,1]=="Nf3"&cleanDF(i)[2,2]=="Nc6"&cleanDF(i)[3,1]=="Nc3"&cleanDF(i)[3,2]!="Nf6"){
        opening = "ThreeKnights"
    } else {
        opening = "ToBeClassified"
    }
    if(!dir.exists(paste("./Openings/", opening, sep = ""))) {
        dir.create(paste("./Openings/", opening, sep = ""))
    }
    if(!file.exists(paste("./Openings/",opening,"/","Game", as.character(i),"-", handle, ".txt", sep = ""))) {
        write.table(cleanDF(i), quote = FALSE,
                    file = paste("./Openings/",opening,"/","Game", as.character(i),"-", handle, ".txt", sep = ""))}
}

