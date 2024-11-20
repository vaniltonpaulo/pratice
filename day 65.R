prime_numbers <- function(n){
  is_prime <- function(n) {
    if(n == 2) return(TRUE)
    if (n <= 1) {
      return(FALSE)  # Numbers <= 1 are not prime
    }
    for (i in 2:sqrt(n)) {
      if (n %% i == 0) {
        return(FALSE)  # Divisible by a number other than 1 and itself
      }
    }
    return(TRUE)  # Number is prime
  }
  
  return(length(Filter(is_prime,seq_len(n))))
}



# prime_numbers(10) ➞ 4
# # 2, 3, 5 and 7
# 
# prime_numbers(20) ➞ 8
# # 2, 3, 5, 7, 11, 13, 17 and 19
# 
# prime_numbers(30) ➞ 10
# # 2, 3, 5, 7, 11, 13, 17, 19, 23 and 29




larger_than_right <- function(x){
  result <-  numeric(0)
  
  for (i in seq_len(length(x))) {
    if(isTRUE(all(x[i] > x[(i+1):length(x)]))){
      result[[length(result) + 1]] <- x[i]
    }
  }
  append(result,x[[length(x)]])
}


# larger_than_right(c(3, 13, 11, 2, 1, 9, 5)) ➞ [13, 11, 9, 5]
# # 13 is larger than all numbers to its right, etc.
# 
# larger_than_right(c(5, 5, 5, 5, 5, 5)) ➞ [5]
# # Must be strictly larger.
# # Always include the last number.
# 
# larger_than_right(c(5, 9, 8, 7)) ➞ [9, 8, 7]


