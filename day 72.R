#speed run very easy REGEX
is_valid <- function(x){
  if(nchar(x)>5) return(FALSE)
  if(isTRUE(grepl("\\s+",x,perl = TRUE))) return(FALSE)
  
  if(isTRUE(grepl("[^0-9]",x,perl = TRUE))) return(FALSE)
  TRUE
}

# is_valid("59001") ➞ True
# 
# is_valid("853a7") ➞ False
# 
# is_valid("732 32") ➞ False
is_valid("59238aa")


first_vowel <- function(x){
  x <- tolower(x)
  vowels <- c("a","e","i","o","u")
  
  result <- strsplit(x,"")[[1]]
  final <-intersect(result, vowels)[[1]]
  return(which(result == final,arr.ind = TRUE) -1)
  
}


# 
# first_vowel("apple") ➞ 0
# 
# first_vowel("hello") ➞ 1
# 
# first_vowel("STRAWBERRY") ➞ 3
# 
# first_vowel("pInEaPPLe") ➞ 1
