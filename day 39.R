
fizz_buzz <- function(x){
  if(x %% 5 == 0 & x %% 3 == 0 ) return("FizzBuzz")
  if(x %% 5 == 0 ) return("Buzz")
  if( x %% 3 == 0 ) return("Fizz")
  
  return(as.character(x))
  
}

# fizz_buzz(3) ➞ "Fizz"
# 
# fizz_buzz(5) ➞ "Buzz"
# 
# fizz_buzz(15) ➞ "FizzBuzz"
# 
# fizz_buzz(4) ➞ "4"


face_interval <- function(x){
  if(is.list(x) == FALSE) return(":/")
  nm<-max(unlist(x)) - min(unlist(x)) 
if(  (nm %in% x) == TRUE) return(":)") 
  return(":(")
  }

# face_interval(list(1, 2, 5, 8, 3, 9)) ➞ ":)"
# # List interval is equal to one of the other elements.
# 
# face_interval(list(5, 2, 8, 3, 11)) ➞ ":("
# # List interval is not among the other elements.
# 
# face_interval("bruh") ➞ ":/"
# # "bruh" is not a list. It's string.



is_alpha <- function(x){
  x<- tolower(x)
  
  k<-regmatches(x,gregexpr("[a-z]",x,perl = TRUE))[[1]]
  result<- character(0)
  for (i in seq_along(k)) {
    result[[length(result)+1]] <- which(k[i]==letters)
    
  }
  
  final <-sum(as.numeric(result))
  
  if(final %% 2 == 0) return(TRUE) else return(FALSE)
}


# is_alpha("i'am king")  ➞ True
# # 9 + 1 + 13 + 11 + 9 + 14 + 7 = 64 (even)
# 
# is_alpha("True") ➞ True
# # 20 + 18 + 21 + 5= 64 (even)
# 
# is_alpha("alexa") ➞ False
# # 1 + 12 + 5 + 24 + 1= 43 (odd)





# cons([5, 1, 4, 3, 2]) ➞ True
# // Can be re-arranged to form [1, 2, 3, 4, 5]
# 
# cons([5, 1, 4, 3, 2, 8]) ➞ False
# 
# cons([5, 6, 7, 8, 9, 9]) ➞ False
# // 9 appears twice