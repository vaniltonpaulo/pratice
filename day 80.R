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
