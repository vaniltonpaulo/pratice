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



pos_neg_sort <- function(x){
  if(length(x) == 0) return(numeric(0))
  x.sorted <- sort(x[x >0])
  
  for (i in seq_len(length(x))) {
    if(isTRUE(x[[i]] >0)){
      x[[i]] <- x.sorted[[1]]
      x.sorted <- x.sorted[-1]
    } 
  }
  return(x)
}


# pos_neg_sort(c(6, 3, -2, 5, -8, 2, -2)) ➞ [2, 3, -2, 5, -8, 6, -2]
# 
# pos_neg_sort(c(6, 5, 4, -1, 3, 2, -1, 1)) ➞ [1, 2, 3, -1, 4, 5, -1, 6]
# 
# pos_neg_sort(c(-5, -5, -5, -5, 7, -5)) ➞ [-5, -5, -5, -5, 7, -5]
# 
# pos_neg_sort(c()) ➞ []




lengthen <- function(x,y){
  if(isTRUE(nchar(y) < nchar(x))){
    result <- paste0(rep(y,nchar(x)- nchar(y)),collapse = "")
    return(substr(result,1,nchar(x)))
  }else{
  result <- paste0(rep(x,nchar(y)- nchar(x)),collapse = "")
  return(substr(result,1,nchar(y)))
  }
}

# lengthen("abcdefg", "ab") ➞ "abababa"
# 
# lengthen("ingenius", "forest") ➞ "forestfo"
# 
# lengthen("clap", "skipping") ➞ "clapclap"


primorial <- function(x){
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
  result <- numeric(0)
  n <-2
  repeat{
    
    if(isTRUE(is_prime(n))) {
      result[[length(result) + 1]] <- n
      if(length(result) >= x){
        break;
      }
    }
    n <- n + 1
  }
  
  prod(result)
}

# primorial(1) ➞ 2
# 
# primorial(2) ➞ 6
# 
# primorial(8) ➞ 9699690

