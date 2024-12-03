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


