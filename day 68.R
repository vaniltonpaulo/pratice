show_the_love <- function(x){
  nums <- min(x)
  toAdd <- sum(x[x != min(x)] * 0.25)
  x[x != min(x)] <- x[x != min(x)] * 0.75
  
  
  x[x==nums] <- sum(nums,toAdd)
  x
}


# show_the_love(c(4, 1, 4)) ➞ [3, 3, 3]
# 
# show_the_love(c(16, 10, 8)) ➞ [12, 7.5, 14.5]
# 
# show_the_love(c(2, 100)) ➞ [27, 75]


find_fulcrum <- function(x){
  result <- numeric(0)
  
  for (i in seq_along(x)) {
    if(isTRUE(all.equal(sum(x[1:(i - 1)]),sum(x[(i + 1):length(x)])))){
      result[[length(result) + 1]] <- x[i]
    }
  }
  
  if(length(result) == 0) return(-1)
  result[[1]]
}


# find_fulcrum(c(1, 2, 4, 9, 10, -10, -9, 3)) ➞ 4
# 
# find_fulcrum(c(9, 1, 9)) ➞ 1
# 
# find_fulcrum(c(7, -1, 0, -1, 1, 1, 2, 3)) ➞ 0
# 
# find_fulcrum(c(8, 8, 8, 8)) ➞ -1


makeTranspose <- function(x){
  t(x)
}
matrix1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)
# Example 2
matrix2 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)

# Example 3
matrix3 <- matrix(c("a", "b"), nrow = 1, byrow = TRUE)



# makeTranspose([
#   [1, 2, 3],
#   [4, 5, 6],
#   [7, 8, 9]
# ]) ➞ [
#   [1, 4, 7],
#   [2, 5, 8],
#   [3, 6, 9]
# ]
# 
# makeTranspose([
#   [1, 2],
#   [3, 4],
#   [5, 6]
# ]) ➞ [
#   [1, 3, 5],
#   [2, 4, 6]
# ]
# 
# makeTranspose([
#   ["a", "b"]
# ]) ➞ [
#   ["a"],
#   ["b"]
# ]

makeTranspose(matrix1)

min_miss_pos <- function(x){
  y<-sort(x)
  result <-min(y):max(y)
  final<-result[!result %in% unique(y)]
  min(final[final > 0])  
}

# min_miss_pos(c(-2, 6, 4, 5, 7, -1, 1, 3, 6, -2, 9, 10, 2, 2)) ➞ 8
# # After sorting, list becomes [-2, -2, -1, 1, 2, 2, 3, 4, 5, 6, 6, 7, 9, 10]
# # So the smallest missing positive integer is 8
# 
# min_miss_pos(c(5, 9, -2, 0, 1, 3, 9, 3, 8, 9)) ➞ 2
# # After sorting, list becomes [-2, 0, 1, 3, 3, 5, 8, 9, 9, 9]
# # So the smallest missing positive integer is 2
# 
# min_miss_pos(c(0, 4, 4, -1, 9, 4, 5, 2, 10, 7, 6, 3, 10, 9)) ➞ 1
# # After sorting, list becomes [-1, 0, 2, 3, 4, 4, 4, 5, 6, 7, 9, 9, 10, 10]
# # So the smallest missing positive integer is 1



## very hard
can_see_stage <- function(x){
  apply(matrix1, 2,function(k) all(diff(k) >0 ))
  
}
matrix1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)

# Example 2
matrix2 <- matrix(c(0, 0, 0, 1, 1, 1, 2, 2, 2), nrow = 3, byrow = TRUE)

# Example 3
matrix3 <- matrix(c(2, 0, 0, 1, 1, 1, 2, 2, 2), nrow = 3, byrow = TRUE)

# Example 4
matrix4 <- matrix(c(1, 0, 0, 1, 1, 1, 2, 2, 2), nrow = 3, byrow = TRUE)



id_mtrx <- function(n){
  if(n < 0){
    k <- n * -1
    mat <- matrix(0,nrow = k,ncol = k)
    for(i in seq_len(nrow(mat))){
      for (j in seq_len(ncol(mat))) {
        if( i ==j){
          mat[i,i] <- 1
        }
        
      }
    } 
    return(mat[rev(seq_len(nrow(mat))), ])
    
  }else{
    mat <- matrix(0,nrow = n,ncol = n)
    for(i in seq_len(nrow(mat))){
      for (j in seq_len(ncol(mat))) {
        if( i ==j){
          mat[i,i] <- 1
        }
        
      }
    }
    return(mat)
  }
}


# id_mtrx(2) ➞ [
#   [1, 0],
#   [0, 1]
# ]
# 
# id_mtrx(-2) ➞ [
#   [0, 1],
#   [1, 0]
# ]
# 
# id_mtrx(0) ➞ []



staircase <- function(n){
  if(n < 0) {
    k <- n * -1
    result <- character(0)
    for (i in seq_len(k)) {
      result[[length(result) + 1]] <- paste0(paste0(rep("_",(k - i)), collapse = ""),paste0(rep("#",i),collapse = ""))
    }
    result <- rev(result)
    final <-paste0(result,collapse = "\n")
    
    return(cat(final,sep = "\n"))
  }
  result <- character(0)
  for (i in seq_len(n)) {
    result[[length(result) + 1]] <- paste0(paste0(rep("_",(n - i)), collapse = ""),paste0(rep("#",i),collapse = ""))
  }
  final <-paste0(result,collapse = "\n")
  
  return(cat(final,sep = "\n"))
}


# staircase(3) ➞ "__#\n_##\n###"
# __#
# _##
# ###



# staircase(7) ➞ "______#\n_____##\n____###\n___####\n__#####\n_######\n#######"
# ______#
# _____##
# ____###
# ___####
# __#####
# _######
# #######
# 
# staircase(2) ➞ "_#\n##"
# _#
# ##
# 
# staircase(-8) ➞ "########\n_#######\n__######\n___#####\n____####\n_____###\n______##\n_______#"
# ########
# _#######
# __######
# ___#####
# ____####
# _____###
# ______##
# _______#