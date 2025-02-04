isAdjacent <-  function(df,node1, node2) {
  ifelse(df[node1, node2] == 1, TRUE, FALSE)
}




df <- matrix(c(
  0, 1, 0, 0,
  1, 0, 1, 1,
  0, 1, 0, 1,
  0, 1, 1, 0
), nrow = 4, byrow = TRUE)

isAdjacent(df, 1,2)
isAdjacent(df, 1,3)


transposeMatrix <-  function(x) {
  result <- t(x)
  final <- apply(result,1 ,paste0, collapse = " ")
  paste(final, collapse = " ")
}

transposeMatrix(matrix(c("Enter", "the", "Matrix!"), nrow = 3, byrow = FALSE))
# Output: "Enter the Matrix!"

transposeMatrix(matrix(c("The", "columns", "are", "rows."), nrow = 2, byrow = FALSE))
# Output: "The columns are rows."

transposeMatrix(matrix(c("You", "must", "transpose", "the", "table", "order."), nrow = 3, byrow = FALSE))
# Output: "You must transpose the table order



makeTranspose <-  function(x) {
  # always start with a empty matrix
  result <- matrix(0, ncol = nrow(x), nrow = ncol(x))
  
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      
      result[j,i] <-  x[i,j]
      
    }
    
  }
  result
  
}
# Test cases
print(makeTranspose(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)))
# Output:
#      [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9

print(makeTranspose(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)))
# Output:
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# [3,]    5    6

print(makeTranspose(matrix(c("a", "b"), nrow = 1, byrow = TRUE)))
# Output:
#      [,1]
# [1,] "a" 
# [2,] "b"


idMtrx <- function(n) {
  if(n > 0) {
    result <- matrix(0, ncol = n, nrow = n)
    
    for (i in seq_len(nrow(result))) {
      for (j in seq_len(ncol(result))) {
        if(i == j) {
          result[i,j] <-  1
        } else {
          result[i,j] <-  0
        }
        
      }
      
    }
    
    return(result)
  } else {
    n <-  n * -1
    result <- matrix(0, ncol = n, nrow = n)
    
    for (i in seq_len(nrow(result))) {
      for (j in seq_len(ncol(result))) {
        if(i != j) {
          result[i,j] <-  1
        } else {
          result[i,j] <-  0
        }
        
      }
      
    }
    
    return(result)
    
  }
}

# idMtrx(2) ➞ [
#   [1, 0],
#   [0, 1]
# ]
# 
# idMtrx(-2) ➞ [
#   [0, 1],
#   [1, 0]
# ]
# 
# idMtrx(0) ➞ []

 
orderedMatrix <-  function(x, y) {
   matrix(1:(x * y), nrow = x, ncol = y)
  
}
# orderedMatrix(5, 5) ➞  matrix(c(
#   1, 2, 3, 4, 5,
#   6, 7, 8, 9, 10,
#   11, 12, 13, 14, 15,
#   16, 17, 18, 19, 20,
#   21, 22, 23, 24, 25
# ), nrow = 5, byrow = TRUE)


orderedMatrix(1, 5) 


lowerTriang <-  function(result) {
  
  for (i in seq_len(nrow(result))) {
    for (j in seq_len(ncol(result))) {
      if(i < j) {
        result[i,j] <-  0
      } 
      
    }
    
  }
  
  result
}
# Test cases
print(lowerTriang(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)))
# Output:
#      [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    4    5    0
# [3,]    7    8    9

print(lowerTriang(matrix(c(5, 7, 7, 9), nrow = 2, byrow = TRUE)))
# Output:
#      [,1] [,2]
# [1,]    5    0
# [2,]    7    9

print(lowerTriang(matrix(c(1, 8, 8, 1, 2, 7, 7, 2, 3, 6, 6, 3, 4, 5, 5, 4), nrow = 4, byrow = TRUE)))
# Output:
#      [,1] [,2] [,3] [,4]
# [1,]    1    0    0    0
# [2,]    2    7    0    0
# [3,]    3    6    6    0
# [4,]    4    5    5    4



spiralMatrix <- function(n) {
  # Create an empty n x n matrix
  matrix_result <- matrix(0, nrow = n, ncol = n)
  
  # Initialize directions (right, down, left, up)
  directions <- list(c(0, 1), c(1, 0), c(0, -1), c(-1, 0))
  current_direction <- 1
  
  # Starting point
  row <- 1
  col <- 1
  
  for (value in 1:(n * n)) {
    # Fill the current cell
    matrix_result[row, col] <- value
    
    # Compute the next position
    next_row <- row + directions[[current_direction]][1]
    next_col <- col + directions[[current_direction]][2]
    
    # Check if the next position is valid
    if (next_row < 1 || next_row > n || next_col < 1 || next_col > n || matrix_result[next_row, next_col] != 0) {
      # Change direction clockwise
      current_direction <- (current_direction %% 4) + 1
      next_row <- row + directions[[current_direction]][1]
      next_col <- col + directions[[current_direction]][2]
    }
    
    # Move to the next position
    row <- next_row
    col <- next_col
  }
  
  return(matrix_result)
}

# Test cases
print(spiralMatrix(2))
# Output:
#      [,1] [,2]
# [1,]    1    2
# [2,]    4    3

print(spiralMatrix(3))
# Output:
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    8    9    4
# [3,]    7    6    5

print(spiralMatrix(4))
# Output:
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    3    4
# [2,]   12   13   14    5
# [3,]   11   16   15    6
# [4,]   10    9    8    7


