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
