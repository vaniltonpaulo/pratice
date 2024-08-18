sentence_searcher <- function(txt,word){
  
  word <- tolower(word)
  sentences<-strsplit(txt,"\\.")[[1]]
  clone.sentences <- sentences
  clone.sentences <- tolower(clone.sentences)
  ok<-grepl(word,clone.sentences,perl = TRUE)
  if(sum(ok) == 0){ 
    return("")
  } else{
    finalsentence<-sentences[ok][[1]]
    paste0(trimws(finalsentence),".",collapse = "")

    }
  
}
# txt = "I have a cat. I have a mat. Things are going swell."
# 
# sentence_searcher(txt, "have") ➞ "I have a cat."
# 
# sentence_searcher(txt, "MAT") ➞ "I have a mat."
# 
# sentence_searcher(txt, "things") ➞ "Things are going swell."
# 
# sentence_searcher(txt, "flat") ➞ ""
sentence_searcher(txt, "Have")
sentence_searcher(txt, "swell")





count_ones <- function(x) {
  ones <- strsplit(paste(lst, collapse = ""), "0")[[1]]
  
  # Count how many of the resulting segments have a length greater than 1
  sum(nchar(ones) > 1)
}


# count_ones(c(1, 0, 0, 1, 1, 0, 1, 1, 1)) ➞ 2
# # Two instances: [1, 1] (middle) and [1, 1, 1] (end)
# 
# count_ones(c(1, 0, 1, 0, 1, 0, 1, 0)) ➞ 0
# 
# count_ones(c(1, 1, 1, 1, 0, 0, 0, 0)) ➞ 1
# 
# count_ones(c(0, 0, 0)) ➞ 0


simple_symbols <- function(x){
  
  k<-regmatches(x,gregexpr("\\+[A-z]\\+",x,perl = TRUE))[[1]]
  y<-regmatches(x,gregexpr("[A-z]",x,perl = TRUE))[[1]]
  length(y)
  
  if(length(k) != length(y)) return(FALSE) else return(TRUE)
  
}



# simple_symbols("f++d+") ➞ False
# 
# simple_symbols("+d+=3=+s+") ➞ True
# 
# simple_symbols("==+p+++++++++====8+z++++") ➞ True

simple_symbols("+u+====3+mmmmm===m++")
simple_symbols("======2+++4+u===+i+")




is_parsel_tongue <- function(x){
  k<-strsplit(x,"\\s+",perl = TRUE)[[1]]
  k<-tolower(k)
  if(sum(grepl("(\\s|[bcdfghjklmnpqrtvwxyzaeiou])s[bcdfghjklmnpqrtvwxyzaeiou])",clone.k,perl = TRUE)) > 0) return(FALSE)
  if(sum(grepl("ss",clone.k,perl = TRUE))>0 ) return(TRUE)
}


# is_parsel_tongue("Sshe ssselects to eat that apple. ") ➞ True
# 
# is_parsel_tongue("She ssselects to eat that apple. ") ➞ False
# # "She" only contains one "s".
# 
# is_parsel_tongue("Beatrice samples lemonade") ➞ False
# # While "samples" has 2 instances of "s", they are not together.
# 
# is_parsel_tongue("You ssseldom sssspeak sso boldly, ssso messmerizingly.") ➞ True


x<- "She ssselects to eat that apple. "
k<-strsplit(x,"\\s+",perl = TRUE)[[1]]
k<-tolower(k)
grepl("ss",clone.k,perl = TRUE)


words <- unlist(strsplit(x, "\\s+"))
is_parsel_word <- function(word) {
  # If the word has two or more consecutive 's', it is a parseltongue word
  if (grepl("ss+", word, ignore.case = TRUE)) {
    return(TRUE)
  }
  # If the word has no 's', it is also a parseltongue word
  if (!grepl("s", word, ignore.case = TRUE)) {
    return(TRUE)
  }
  # Otherwise, it is not a parseltongue word
  return(FALSE)
}
all(sapply(words, is_parsel_word))



is_parsel_tongue <- function(sentence) {
  # Split the sentence into words
  words <- unlist(strsplit(sentence, "\\s+"))
  
  # Function to check if a word is a parseltongue word
  is_parsel_word <- function(word) {
    # If the word has two or more consecutive 's', it is a parseltongue word
    if (grepl("ss+", word, ignore.case = TRUE)) {
      return(TRUE)
    }
    # If the word has no 's', it is also a parseltongue word
    if (!grepl("s", word, ignore.case = TRUE)) {
      return(TRUE)
    }
    # Otherwise, it is not a parseltongue word
    return(FALSE)
  }
  
  # Apply the check to all words in the sentence
  all(sapply(words, is_parsel_word))
}


!grepl("s",k, ignore.case = TRUE)