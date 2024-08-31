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



