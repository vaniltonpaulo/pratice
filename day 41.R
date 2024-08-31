sort_by_character <- function(x,y){
  
  k<-lapply(x,function(k){
    strsplit(k,"")[[1]]
  })
  
  
  result<- character(0)
  
  for (i in seq_len(length(k))) {
    result<-c(result,k[[i]][[y]])
    
  }
  return(x[order(result)])
}



# sort_by_character(c("az16", "by35", "cx24"), 2) ➞ ["cx24", "by35", "az16"]
# // By 2nd character: ["x", "y", "z"] is in alphabetical order.
# 
# sort_by_character(c("mayor", "apple", "petal"), 5) ➞ ["apple", "petal", "mayor"]
# # By 5th character: ["e", "l", "r"] is in alphabetical order
# 
# sort_by_character(c("mate", "team", "bade"), 3) ➞ ["team", "bade", "mate"]




sort_by_last <- function(x){
  
  k<-strsplit(x,"\\s+")[[1]]
    result<- character(0)
  
  substr(k[[1]],nchar(k[[1]]),nchar(k[[1]]))
  result<- character(0)
  
  for (i in seq_len(length(k))) {
    result<-c(result, substr(k[[i]],nchar(k[[i]]),nchar(k[[i]])))
    
    
  }
  final <-k[order(result)]
  paste0(final,collapse = " ")
}


# sort_by_last("herb camera dynamic") ➞ "camera herb dynamic"
# 
# sort_by_last("stab traction artist approach") ➞ "stab approach traction artist"
# 
# sort_by_last("sample partner autonomy swallow trend") ➞ "trend sample partner swallow autonomy"




make_grlex <- function(x){
  
  k<-vapply(x,function(k){
    nchar(k)
  },numeric(1))
  
  y<-names(sort(k))
  p<-sort_by_character(y,1)
  return(p)
}


# make_grlex(c("small", "big")) ➞ ["big", "small"]
# 
# make_grlex(c("cat", "ran", "for", "the", "rat")) ➞ ["cat", "for", "ran", "rat", "the"]
# 
# make_grlex(c("this", "is", "a", "small", "test")) ➞ ["a", "is", "small","this", "test"]



