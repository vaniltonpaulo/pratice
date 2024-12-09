# The Game "Tic Tac Toe" is a two player game with players taking turns occupying spaces on a 3x3 grid.
# A player wins by occupying either a full diagonal, a row, or a column of 3 spaces.
#
# The playing field is a 3x3 matrix, with columns named A..C, and rows numbered 1..3
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 |   |   |   |
# --+---+---+---+
# 3 |   |   |   |
# --+---+---+---+
#
# We are considering a modified "Tic Tac Toe" game with "gravity": The players take turns choosing a
# column "A", "B" or "C". They then occupy the lowest empty field of that column, as if they had put
# a token into the column and it had fallen down. E.g.:
#
# Player "X" chooses "A":
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 |   |   |   |
# --+---+---+---+
# 3 | X |   |   |
# --+---+---+---+
#
# Player "O" chooses "B":
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 |   |   |   |
# --+---+---+---+
# 3 | X | O |   |
# --+---+---+---+
#
# Player "X" chooses "A":
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 | X |   |   |
# --+---+---+---+
# 3 | X | O |   |
# --+---+---+---+
#
# We are calling this version of the game "Gravity Tic Tac Toe"
#
# The goal of this exercise is to write a function that lets someone play 3x3 Gravity Tic Tac Toe against another
# human player, or against the computer.
# The state of the game (i.e. which fields are occupied by which players) is called the "position", similar to chess.
# Positions are represented by a 3x3-matrix with entries "X", "O" or NA.

# Part 01: Write a function that takes a position matrix and determines if there is a winner, and if
# so, who has won. The function should have a single input `position`; If this is not
# a 3x3 character matrix that may only contain "X", "O" or NA, an error should be thrown.
# The return value should be:
# - "X", if the "X"-player has won
# - "O", if the "O"-player has won
# - "" (empty string), if the game is a draw: no player has won, but there are no more possible moves
# - NA, if no player has won yet
# an error should be thrown if there are "two winners", e.g. a row of "X" and another row of "O",
# since this is an invalid position.
# It is not necessary to check for other indicators of invalid states. A position with two "X" and
# no "O" should explicitly be allowed, even though it would not be a reachable state in normal play
# (this could happen if one is giving one player an initial "advantage").
# Likewise, it is not necessary to verify whether `position` is a valid position when considering "gravity":
# Although a position where A3 is NA but A2 is "X" or "O" is not possible in this game, checking for this
# is not necessary.
#
# Hint: If you use `assertMatrix(mode = "character")`, you will find that
# `matrix(NA, nrow = 3, ncol = 3)` will trigger an error, because the "mode" of `NA` is `logical`
# by default. To get a character-NA-matrix, use `matrix(NA_character_, nrow = 3, ncol = 3)`. See
# > is.character(NA)  # FALSE
# > is.character(NA_character_)  # TRUE
# (Advanced students: There is a reason why the default `NA` is of type `logical`, can you think of what it is?)
ex01Winner <- function(position) {
  # your code
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
