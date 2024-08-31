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







str_to_dict <- function(x){
  
  k<-lapply(x, function(k){
    strsplit(k,"=")[[1]]
  })
  
  
  result<- character(0)
  result.1<- character(0)
  
  
  for (i in seq_len(length(k))) {
    result<-c(result,k[[i]][[1]])
    result.1<-c(result.1,k[[i]][2])
  }
  
  result
  result.1
  
  names(result.1)<-result
  
  result.1
  
  
}


# str_to_dict(c("1=one", "2=two", "3=three", "4=four")) ➞ {"1": "one", "2": "two", "3": "three", "4": "four"}
# 
# str_to_dict(c("dog=bark", "cat=meow", "cow=moo")) ➞ {"dog": "bark", "cat": "meow", "cow": "moo"}
# 
# str_to_dict(c("bob=human", "lola=dog", "mittens=cat", "todd=frog")) ➞ {"bob": "human", "lola": "dog", "mittens": "cat", "todd": "frog"}



move_zeros <- function(x){
  
  k<-sub("0",NA,x)
  new.k<-k[!is.na(k)]
  c(new.k,rep(0,sum(is.na(k))))
}




# move_zeros(c(1, 0, 1, 2, 0, 1, 3)) ➞ [1, 1, 2, 1, 3, 0, 0]
# 
# move_zeros(c(0, 1, NULL, 2, FALSE, 1, 0)) ➞ [1, None, 2, false, 1, 0, 0]
# 
# move_zeros(c('a', 0, 0, 'b', 'c', 'd', 0, 1, 0, 1, 0, 3, 0, 1, 9, 0, 0, 0, 0, 9)) ➞ ['a', 'b', 'c', 'd', 1, 1, 3, 1, 9, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]




#Now Hard


