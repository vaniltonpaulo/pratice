#Is Johnny Making Progress?
progress_days <- function(x){
  count <- 0
  for (i in seq_len(length(x)-1)) {
    if(x[[i]]<x[[i+1]]){
      count <- count +1
    } else{
      count <- count +0
    }
  }
  count
}


# 
# progress_days(c(3, 4, 1, 2)) ➞ 2
# # There are two progress days, (3->4) and (1->2)
# 
# progress_days(c(10, 11, 12, 9, 10)) ➞ 3
# 
# progress_days(c(6, 5, 4, 3, 2, 9)) ➞ 1
# 
# progress_days(c(9, 9))  ➞ 0



#Perfect Square Patch

square_patch <- function(n){
  matrix(n,nrow = n,ncol = n)
}



# 
# square_patch(3) ➞ [
#   [3, 3, 3],
#   [3, 3, 3],
#   [3, 3, 3]
# ]
# 
# square_patch(5) ➞ [
#   [5, 5, 5, 5, 5],
#   [5, 5, 5, 5, 5],
#   [5, 5, 5, 5, 5],
#   [5, 5, 5, 5, 5],
#   [5, 5, 5, 5, 5]
# ]
# 
# square_patch(1) ➞ [
#   [1]
# ]
# 
# square_patch(0) ➞ []



#Neutralisation
neutralise <- function(x,y){
  
  x.splited<-strsplit(x,"")[[1]]
  y.splited<-strsplit(y,"")[[1]]
  result <-character(0)
  for (i in seq_len(length(x.splited))) {
    if(x.splited[[i]] ==y.splited[[i]]){
      result[[length(result) + 1]] <- x.splited[[i]]
    }else{
      result[[length(result) + 1]] <-0
    }
    
    
  }
  paste0(result,collapse = "")
  
}

# neutralise("--++--", "++--++") ➞ "000000"
# 
# neutralise("-+-+-+", "-+-+-+") ➞ "-+-+-+"
# 
# neutralise("-++-", "-+-+") ➞ "-+00"


#Syncopated Rhythm
has_syncopation <- function(x){
  x.splited <-strsplit(x,"")[[1]]
  length(x.splited)
  
  result <- character(0)
  for (i in seq_len(length(x.splited))) {
    if( i %% 2 == 0){
      result[[length(result) + 1]]<-x.splited[[i]]
    } 
    
  }
  any(result == "#")
}
# has_syncopation(".#.#.#.#") ➞ True
# # There are Hash signs in the second, fourth, sixth and
# # eighth positions of the string.
# 
# has_syncopation("#.#...#.") ➞ False
# # There are no Hash signs in the second, fourth, sixth or
# # eighth positions of the string.
# 
# has_syncopation("#.#.###.") ➞ True
# # There is a Hash sign in the sixth position of the string.


Reverse Coding Challenge #6
