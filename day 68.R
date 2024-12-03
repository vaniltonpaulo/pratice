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


