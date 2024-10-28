count_smileys <- function(x){
  sum(grepl("^[:;][-~]?[)D]$",x))
  
}


# count_smileys(c(":)", ";(", ";}", ":-D")) ➞ 2
# 
# count_smileys([";D", ":-(", ":-)", ";~)"]) ➞ 3
# 
# count_smileys(c(";]", ":[", ";*", ":$", ";-D")) ➞ 1


assignment <- function(x){
  grepl("^[0-9]{4}/[0-9]{2}/[0-9]{2}$",x)
}

# assignment("12/1/1") ➞ False
# 
# assignment("1234/12/01") ➞ True
# 
# assignment("2012/1/1") ➞ False
# 
# assignment("2012/01/07") ➞ True
assignment("2018/09/12")
assignment("ammo2011/01/07")

assignment("dates2012/01/07appo")

to_camel_case <- function(x){
  
  result<-strsplit(x,"(-|_)")[[1]]
  
  
  final<-paste0(
    result[1],
    paste0(toupper(substr(result[-1],1,1)),tolower(substr(result[-1],2,nchar(result[-1]))),collapse = ""))
  final
  
}


# to_camel_case("A-B-C") ➞ "ABC"
# 
# to_camel_case("the-stealth-warrior") ➞ "theStealthWarrior"
# 
# to_camel_case("The_Stealth_Warrior") ➞ "TheStealthWarrior"
x <- "1234/12/01"

x<-"A-B-C"

result<-strsplit(x,"(-|_)")[[1]]


paste0(
  result[1],
       paste0(toupper(substr(result[-1],1,1)),tolower(substr(result[-1],2,nchar(result[-1]))),collapse = "")
  
  )


num_in_str <- function(x){
  grep("[0-9]+",x,value = TRUE)
  }

# num_in_str(c("1a", "a", "2b", "b")) ➞ ["1a", "2b"]
# 
# num_in_str(c("abc", "abc10")) ➞ ["abc10"]
# 
# num_in_str(c("abc", "ab10c", "a10bc", "bcd")) ➞ ["ab10c", "a10bc"]
# 
# num_in_str(c("this is a test", "test1")) ➞ ["test1"]



tweet <- function(x){
  regmatches(x,gregexpr("[@#][A-z]+",x,perl = TRUE))[[1]] 
}

# tweet("Visit us at @edabit") ➞ "@edabit"
# 
# tweet("Follow @JavaScript") ➞ "@JavaScript"
# 
# tweet("#Honesty is the best @policy!!") ➞ "#Honesty @policy"


split_and_delimit <- function(x,y,z){
  
  
  split_string_equal_parts <- function(text, part_length) {
    # Initialize an empty vector to store the parts
    parts <- character(0)
    
    # Calculate the total number of parts
    n <- ceiling(nchar(text) / part_length)
    
    # Loop through each part and extract the substring
    for (i in 1:n) {
      start <- (i - 1) * part_length + 1
      end <- min(i * part_length, nchar(text))
      parts <- c(parts, substring(text, start, end))
    }
    
    return(parts)
  }
  
  result <- split_string_equal_parts(x, y)
  
  paste0(result,collapse = z)
  
  
}

# split_and_delimit("bellow", 2, "&") ➞ "be&ll&ow"
# 
# split_and_delimit("magnify", 3, ":") ➞ "mag:nif:y"
# 
# split_and_delimit("poisonous", 2, "~") ➞ "po~is~on~ou~s"





no_yelling <- function(x){
  regmatches(x, gregexpr("[[:punct:]]+$",x,perl = TRUE))[[1]] <-regmatches(x, gregexpr("[[:punct:]]$",x,perl = TRUE))[[1]]
  x  
}

# no_yelling("What went wrong?????????") ➞ "What went wrong?"
# 
# no_yelling("Oh my goodness!!!") ➞ "Oh my goodness!"
# 
# no_yelling("I just!!! can!!! not!!! believe!!! it!!!") ➞ "I just!!! can!!! not!!! believe!!! it!"
# # Only change repeating punctuation at the end of the sentence.
# 
# no_yelling("Oh my goodness!") ➞ "Oh my goodness!"
# # Do not change sentences where there exists only one or zero exclamation marks/question marks.
# 
# no_yelling("I just cannot believe it.") ➞ "I just cannot believe it."

