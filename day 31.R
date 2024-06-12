#Tall People

block <- function(block_matrix){

  result <- which(block_matrix == 2,arr.ind = TRUE)
  rows.nm<-result[,1 ]
  cols.nm <- result[,2]
  okay <-numeric(0)
  for (i in seq_len(length(rows.nm))) {
    for (j in seq_len(nrow(block_matrix) - rows.nm[[i]])) {
      okay[[length(okay) + 1]] <- block_matrix[rows.nm[[i]] + j,cols.nm[[i]]]
    }
  }
  length(okay)
  
}


# block([
#   [1, 1, 1, 1, 1],
#   [1, 1, 1, 1, 1],
#   [1, 1, 1, 1, 2],
#   [1, 1, 1, 1, 1],
#   [1, 1, 1, 1, 1]
# ]) ➞ 2
# 
# # The tall person blocks 2 people behind him thus
# # the function returns 2.
# 
# 
# block([
#   [1, 2, 1, 1],
#   [1, 1, 1, 2],
#   [1, 1, 1, 1],
#   [1, 1, 1, 1],
# ]) ➞ 5
# 




# # There are 2 tall people that block everyone behind
# # them. The first tall person in the first row blocks 3
# # people behind him while the second tall person in
# # the second row blocks 2 people behind him thus the
# # function returns 5.
# 
# 
# block([
#   [1, 1, 1, 1],
#   [2, 1, 1, 2],
#   [1, 1, 1, 1],
#   [1, 1, 1, 1],
# ]) ➞ 4

elements <- c(
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 2,
  1, 1, 1, 1, 1,
  1, 1, 1, 1, 1
)

# Create the matrix
my_matrix <- matrix(elements, nrow = 5, ncol = 5, byrow = TRUE)

block(my_matrix)




elements <- c(
  1, 2, 1, 1,
  1, 1, 1, 2,
  1, 1, 1, 1,
  1, 1, 1, 1
)

# Create the matrix
block_matrix <- matrix(elements, nrow = 4, ncol = 4, byrow = TRUE)
block(block_matrix)




elements <- c(
  1, 1, 1, 1,
  2, 1, 1, 2,
  1, 1, 1, 1,
  1, 1, 1, 1
)

# Create the matrix
block_matrix <- matrix(elements, nrow = 4, ncol = 4, byrow = TRUE)
block(block_matrix)
