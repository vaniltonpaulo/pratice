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
