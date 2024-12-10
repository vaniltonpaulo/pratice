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
