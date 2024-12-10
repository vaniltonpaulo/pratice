ex01Winner <- function(position) {
  # your code
  assertMatrix(position, mode= "character")
  assertSubset(position, choices = c("X","O",NA))
  
  getwinner <- function(sequence){
    winner <-unique(sequence)
    if(length(winner) == 1) return(winner)
    NA_character_
    
  }
  
  allWinners <- c(
    apply(position, 1, getwinner),
    apply(position, 2, getwinner),
    getwinner(diag(position)),
    getwinner(diag(position[ , 3:1]))
  )
  
  result <- unique(allWinners[!is.na(allWinners)])
  
  if (length(result) == 1) {
    return(result)
  }
  
  if (length(result) == 0) {
    if (any(is.na(position))) {
      return(NA)
    }else{
      return("")
    }
    
  }
  stop("more than one winner")
}




# Part 02: Write a function that takes a Gravity Tic Tac Toe position and a column that a player chooses to
# play, and converts them to numeric c(row, col) matrix coordinates of where the player's token will come
# to rest, checking if the column is a valid move in the process.
#
# Your function should take two inputs: `column`, and `position`.
# - `column` must be checked to be a `character(1)` with no missing values (using checkmate).
# - `position` should be a 3x3 position matrix with same constraints as in ex01Winner.
#   (It is *not* necessary to check whether `position` has a winner, or if there are two winners etc.,
#   but it should be checked for being a character matrix with valid content)
# `column` must be one of "A", "B", or "C". If it is not one of these, or if the indicated column is already
# full (i.e. the first row of that column is occupied), an error of class
# "InvalidMoveError" should be thrown, for example by using
# `stop(errorCondition(<message>, class = "InvalidMoveError"))`
# Note the difference between having an invalid type for `column` (e.g. if it is not a `character`, or if it
# is a `character` vector with more than one element), and having `column` of correct type (`character(1)`) but
# with invalid *content*. The first should be checked with `checkmate`, the second should generate an
# `"InvalidMoveError"`.
# You may assume without checking that `position` is a valid matrix when considering "gravity", i.e. that if
# row r, col c are occupied by "X" or "O", then all rows below r of that column c are also occupied.
# An example `position` could be
example.position <- matrix(
  c("X",  NA,  NA,
    "O", "X",  NA,
    "X", "O",  NA),
  ncol = 3, byrow = TRUE)
# In this case, with `column` "A", the function should throw an `InvalidMoveError`. With `column` "B", the
# function should return c(1, 2). With `column` "C", the return value should be c(3, 3).
ex02MoveStringToCoordinate <- function(column,position) {
  # your code
  assertMatrix(position,mode = "character",ncols = 3,nrows = 3)
  assertSubset(position,choice = c("X","O",NA))
  assertString(column)
  
  col <- match(column, LETTERS[1:3])
  
  if(is.na(col)){
    stop(errorCondition(sprintf("column %s is not valid",column), class = "InvalidMoveError"))
  }
  
  row <-max(which(is.na(position[,col])),0)
  if(row == 0){
    stop(errorCondition(sprintf("column %s is full",column), class = "InvalidMoveError"))
  }
  
  c(row,col)
}

ex02MoveStringToCoordinate("A",example.position)




# Part 03: Write a function that interacts with a human player. Your function should take the arguments
# `position` and `player`.
# - `position` should be a 3x3 position matrix, checked in the same way as in ex02MoveStringToCoordinate.
# - `player` should be checked to be either the string "X" or the string "O".
#
# The function is called whenever the current position is `position`, and player `player` is asked to
# make a move. If the game is already over (a player has won, or there is a draw, as per ex01Winner),
# then an error should be thrown, as the player can no longer make a move.
# Otherwise, the function should prompt the player to make his choice, e.g. by printing the position,
# announcing that player `player` is to play, and getting an input by using the `readline()` function.
# (It is essential that `readline()` is called to prompt for input; this function will be replaced
# in the submission check).
# The user input should be sanitized (removing spaces e.g. using `gsub` or `trimws`, converting input
# to uppercase using `toupper`, so that an input of "  a " still counts as valid) and checked:
#  - if the input is "Q", an error should be thrown because the player has ended the game.
#  - if the input is not a valid coordinate as by `ex02MoveStringToCoordinate`, e.g. because the chosen
#    space is occupied or the input is not valid, then an information message should be printed
#    and the user should be prompted *again* for a new input. It is recommended to use the
#    ex02MoveStringToCoordinate function here, in combination with tryCatch, to check user input.
#
# The function should return the numeric coordinate c(row, col) where the token of the player will be
# placed; this is the result of ex02MoveStringToCoordinate of the user's input if the input was valid.
#
# Example interaction with the program:
# > ex03HumanPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3), "X")
## Position:
##   A   B   C
## 1 "X" NA  NA
## 2 "O" "O" NA
## 3 "X" "O" NA
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> d
## Invalid move: D.
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> a
## Column A already full.
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> b
# <RETURN VALUE:> c(1, 2)
#
# > ex03HumanPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3), "X")
## Position:
##   A   B   C
## 1 "X" NA  NA
## 2 "O" "O" NA
## 3 "X" "O" NA
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> Q
## Error in ex03HumanPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA),  :
##   Player X Quit
#
# The output of your answer does not need to be exactly like this, and
# the tests do not check if the function prints anything, but it is recommended to give some output
# to make everything nicer.
ex03HumanPlayer <- function(position, player) {
  # your code
  assertMatrix(position, mode = "character", nrows = 3, ncols = 3)
  assertSubset(position,choices = c("X", "O", NA))
  assertChoice(player, choices = c("X", "O"))
  
  if(!is.na(ex01Winner(position))) stop("Its over son")
  colnames(position) <-LETTERS[1:3]
  rownames(position) <- 1:3
  repeat{
    response <- readline(sprintf("Player %s to move. What is your move?
 (\"A\", \"B\", \"C\"; or \"Q\" to quit):",player))
    response <- trimws(toupper(response))
  }
}


position <- matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3)
player <- "X"

name <- readline("Enter your name: ")
cat("Hello,", name, "!\n")


final <- function(n,m ,i) {
  mt <- matrix(0,nrow = n,ncol = m)
  
  for (x in seq_along(i)) {
    result<-strsplit(i[[x]], "")[[1]]
    if(result[[2]] == "r"){
      mt[as.numeric(result[1]), ] <-mt[as.numeric(result[1]), ] + 1
    }else{
      mt[,as.numeric(result[1]) ] <-mt[, as.numeric(result[1])] + 1
    }
  }
  mt
  
}



# final(2, 2, c("1r", "1r", "1r", "2c")) ➞ [
#   [3, 4],
#   [0, 1]
# ]
# 
# final(2, 2, c("1c")) ➞ [
#   [1, 0],
#   [1, 0]
# ]
# 
# final(3, 3, c('1c', '2c', '2c', '3c', '3c', '3c')) ➞ [
#   [1, 2, 3],
#   [1, 2, 3],
#   [1, 2, 3]
# ]
# 
# final(3, 3, c()) ➞ [
#   [0, 0, 0],
#   [0, 0, 0],
#   [0, 0, 0]
# ]
