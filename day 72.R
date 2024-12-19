#speed run very easy REGEX

##### Very EASY
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







clear_fog <- function(x){
  x <- tolower(x)
  result <- regmatches(x,gregexpr("[^fog]",x,perl = TRUE))[[1]]
  paste0(result,collapse = "")
}

# clear_fog("sky") ➞ "It's a clear day!"
# 
# clear_fog("fogfogFFfoooofftogffreogffesGgfOogfog") ➞ "trees"
# 
# clear_fog("fogFogFogffoObirdsanffodthebffoeffoesGGGfOgFog") ➞ "birdsandthebees"


count_all <- function(x){
  
  re.x<-regmatches(x,gregexpr("[A-z]",x,perl = TRUE))[[1]]
  re.y<-regmatches(x,gregexpr("[0-9]",x,perl = TRUE))[[1]]
  
  c("LETTERS" = length(re.x), "DIGITS" = length(re.y))
}


# count_all("Hello World") ➞ { "LETTERS":  10, "DIGITS": 0 }
# 
# count_all("H3ll0 Wor1d") ➞ { "LETTERS":  7, "DIGITS": 3 }
# 
# count_all("149990") ➞ { "LETTERS": 0, "DIGITS": 6 }



####EASY

alphanumeric_restriction <- function(x){
  funny <- regmatches(x,gregexpr("[[:punct:]]",x,perl = TRUE))[[1]]
  if(length(funny) >0) return(FALSE)
  result <-regmatches(x,gregexpr("[A-z0-9]",x,perl = TRUE))[[1]]
  
  if(any(letters %in% result) == TRUE && any(0:9 %in% result) == TRUE) return(FALSE)
  if( any(LETTERS %in% result) == TRUE && any(0:9 %in% result) == TRUE) return(FALSE)
  TRUE
}
# alphanumeric_restriction("Bold") ➞ True
# 
# alphanumeric_restriction("123454321") ➞ True
# 
# alphanumeric_restriction("H3LL0") ➞ False
# 
# alphanumeric_restriction("ed@bit") ➞ False
alphanumeric_restriction("1a2b3c")



replace_vowel <- function(x){
  chartr("aeiou","12345",x) 
}
# replace_vowel("karachi") ➞ "k1r1ch3"
# 
# replace_vowel("chembur") ➞ "ch2mb5r"
# 
# replace_vowel("khandbari") ➞ "kh1ndb1ri"


double_letters <- function(x){
  result <- strsplit(x,"")[[1]]
  for (i  in 1:(length(result) -1)) {
    if(result[[i]] == result[[i + 1]]) {
      return(TRUE)
    }
  }
  FALSE
}

# double_letters("loop") ➞ True
# 
# double_letters("yummy") ➞ True
# 
# double_letters("orange") ➞ False
# 
# double_letters("munchkin") ➞ False

