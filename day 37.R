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


x<- c(1, 0, 0, 1, 1, 0, 1, 1, 1)
ones <- strsplit(paste(x, collapse = ""), "0")[[1]]

# Count how many of the resulting segments have a length greater than 1
sum(nchar(ones) > 1)





