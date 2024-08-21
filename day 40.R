sum_missing_numbers <- function(x){
  x<-sort(x)
  k<- x[[1]] : x[[length(x)]]
  
  sum(setdiff(k,x))
}

# sum_missing_numbers([1, 3, 5, 7, 10]) ➞ 29
# # 2 + 4 + 6 + 8 + 9
# 
# sum_missing_numbers(c(10, 7, 5, 3, 1)) ➞ 29
# 
# sum_missing_numbers(c(10, 20, 30, 40, 50, 60)) ➞ 1575

sum_missing_numbers(c(7, 8, 9, 10))
sum_missing_numbers(c(99, 2, 45, 4, 17))



largest_gap <- function(x){
  
  x<-sort(x)
  result <- numeric(0)
  seq_along(x) -1
  for (i in 1:(length(x)-1)) {
    
    result[[length(result) + 1]] <- x[[i +1]]- x[[i]]
    
  }
  
  max(result)
}


# largest_gap([9, 4, 26, 26, 0, 0, 5, 20, 6, 25, 5]) ➞ 11
# # After sorting get [0, 0, 4, 5, 5, 6, 9, 20, 25, 26, 26]
# # Largest gap of 11 between 9 and 20
# 
# largest_gap(c(14, 13, 7, 1, 4, 12, 3, 7, 7, 12, 11, 5, 7)) ➞ 4
# # After sorting get [1, 3, 4, 5, 7, 7, 7, 7, 11, 12, 12, 13, 14]
# # Largest gap of 4 between 7 and 11
# 
# largest_gap(c(13, 3, 8, 5, 5, 2, 13, 6, 14, 2, 11, 4, 10, 8, 1, 9)) ➞ 2
# # After sorting get [1, 2, 2, 3, 4, 5, 5, 6, 8, 8, 9, 10, 11, 13, 13, 14]
# # Largest gap of 2 between 6 and 8


merge_sort <- function(x,y){
  if(all(sort(unlist(x)) == unlist(x))){
    y<-sort(unlist(y))
    k<-c(unlist(x),unlist(y))
    return(lapply(k,function(l){
      return(l)
    }))
  } else{
    y<-sort(unlist(y),decreasing = TRUE)
    k<-c(unlist(x),unlist(y))
    return(lapply(k,function(l){
      return(l)
    }))
  }
}


# merge_sort([1, 2, 3], [5, 4, 6]) ➞ [1, 2, 3, 4, 5, 6]
# 
# merge_sort(list(8, 6, 4, 2), list(-2, -6, 0, -4)) ➞ [8, 6, 4, 2, 0, -2, -4, -6]
# 
# merge_sort([120, 180, 200], [190, 175, 130]) ➞ [120, 130, 175, 180, 190, 200]




fruit_salad <- function(x){
  
  split.word <- function(word){
    first.half<-substr(word,1,floor((nchar(word)/2)))
    second.half<- substr(word,floor((nchar(word)/2)+1),nchar(word))
    result <- c(first.half,second.half)
    result
  }
  
  result <- character(0)
  
  for (i in seq_along(x)) {
    result<-c(result,split.word(x[[i]]))
  }
  return(paste0(sort(result),collapse = ""))
  
}


# fruit_salad(c("apple", "pear", "grapes")) ➞ "apargrapepesple"
# 
# fruit_salad(c("raspberries", "mango")) ➞ "erriesmangoraspb"
# 
# fruit_salad(c("banana")) ➞ "anaban"

