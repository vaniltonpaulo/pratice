filter_primes <- function(x){
  x<-unlist(x)
  x[vapply(x,ksiprime,logical(1))]
  
  
  ksiprime<-function(n){
    if(n == 1) return(FALSE)
    if(n == 2) return(TRUE)
    if(n == 3) return(TRUE)
    if(n %% 2 == 0) return(FALSE)
    
    for (i in 3:sqrt(n)) {
      if(n %% i == 0 ) return(FALSE)
    }
    return(TRUE)    
    
  }
  x[vapply(x,ksiprime,logical(1))]
}


# filter_primes(list(7, 9, 3, 9, 10, 11, 27)) ➞ [7, 3, 11]
# 
# filter_primes(list(10007, 1009, 1007, 27, 147, 77, 1001, 70)) ➞ [10007, 1009]
# 
# filter_primes(list(1009, 10, 10, 10, 3, 33, 9, 4, 1, 61, 63, 69, 1087, 1091, 1093, 1097)) ➞ [1009, 3, 61, 1087, 1091, 1093, 1097]


find_the_difference <- function(s,t) {
 
  
  s.k<-table(strsplit(s, "")[[1]])
  t.k <- table(strsplit(t, "")[[1]])
  sooo <- setdiff(names(t.k), names(s.k))
  if(length(sooo) == 0){
    sooo <- names(t.k)[which(t.k > s.k[names(t.k)])]
    
  }
  
  return(sooo)
}



# find_the_difference("abcd", "abcde") ➞ "e"
# 
# find_the_difference("", "y") ➞ "y"
# 
# find_the_difference("ae", "aea") ➞ "a"




fib_fast<- function(n){
  if(n == 0) return(0)
  if(n == 1) return(1)
  
  fib_fast( n -1) + fib_fast( n -2)
  
}




# fib_fast(5) ➞ 5
# 
# fib_fast(10) ➞ 55
# 
# fib_fast(20) ➞ 6765
# 
# fib_fast(50) ➞ 12586269025
fib_fast(20)


multiplication_table<- function(x){
  mt<-matrix(numeric(0),ncol = x,nrow = x)
  for (i in seq_len(nrow(mt))) {
    for (j in seq_len(ncol(mt))) {
      if(i == 1){
        mt[i,j] <- j
      }
      mt[i,j]  <- i *j
    }
    
  }
  mt
  
}


# multiplication_table(1) ➞ [[1]]
# 
# multiplication_table(3) ➞ [[1, 2, 3], [2, 4, 6], [3, 6, 9]]




# cup_swapping(c("AB", "CA")) ➞ "C"
# 
# cup_swapping(c("AC", "CA", "CA", "AC")) ➞ "B"
# 
# cup_swapping(c("BA", "AC", "CA", "BC")) ➞ "A"




cup_swapping <- function(swaps) {
  ball_position <- "B"  # Ball starts at position B
  
  # Loop through each swap
  for (swap in swaps) {
    if (ball_position %in% strsplit(swap, "")[[1]]) {
      # If the ball is in one of the cups involved in the swap, move it to the other cup
      ball_position <- setdiff(strsplit(swap, "")[[1]], ball_position)
    }
  }
  
  return(ball_position)
}

owij_sum<- function(n){
  result <- (n* (n+1))/2
  x<-seq_len(result)
  x<- sort(x,decreasing = TRUE)
  sum(x[1:n])
}



# owij_sum(1) ➞ 1
# 
# owij_sum(2) ➞ 5
# 
# owij_sum(4) ➞ 34


is_palindrome_possible <- function(x){
  result <-strsplit(x,"")[[1]]
  result<-table(result)
  odd_count <-sum(result %% 2 == 1)
  
  return(odd_count <= 1)
}




# is_palindrome_possible("rearcac") ➞ True
# # You can make "racecar"
# 
# is_palindrome_possible("suhbeusheff") ➞ True
# # You can make "sfuehbheufs" (not a real word but still a palindrome)
# 
# is_palindrome_possible("palindrome") ➞ False
# # It's impossible
is_palindrome_possible("avkkiaapiusuapspiip")

#hello
