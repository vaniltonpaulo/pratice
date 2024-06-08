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


#Reverse Coding Challenge #6
mystery_func <- function(n){
  n <- as.character(n)
  n.splited <-strsplit(n,"")[[1]]
  n.splited <- as.numeric(n.splited)
  result <-1
  for (i in seq_len(length(n.splited))) {
    result <- result * n.splited[[i]]
    
  }
  result
}

# mystery_func(152) ➞ 10
# 
# mystery_func(832) ➞ 48
# 
# mystery_func(19) ➞ 9
# 
# mystery_func(133) ➞ 9




#Iterated Square Root
i_sqrt <- function(n){
  if(n == 1) return(0)
  if(n < 0) return("invalid")
  
  count <- 1
  repeat{
    n <- sqrt(n)
    if(n< 2) break
    count <- count +1
  }
  count 
  
}

# i_sqrt(1) ➞ 0
# 
# i_sqrt(2) ➞ 1
# 
# i_sqrt(7) ➞ 2
# 
# i_sqrt(27) ➞ 3
# 
# i_sqrt(256) ➞ 4
# 
# i_sqrt(-1) ➞ "invalid"


#Seven Boom!

seven_boom <- function(x){
  result<-vapply(x,function(k){
    k.splited<-strsplit(as.character(k),"")[[1]]
    if(any(k.splited == 7)) return(TRUE) else FALSE
  },logical(1))
  if(any(result == TRUE)) return("Boom!")
  return("there is no 7 in the list")
}

# 
# seven_boom(c(1, 2, 3, 4, 5, 6, 7)) ➞ "Boom!"
# # 7 contains the number seven.
# 
# seven_boom(c(8, 6, 33, 100)) ➞ "there is no 7 in the list"
# # None of the items contain 7 within them.
# 
# seven_boom(c(2, 55, 60, 97, 86)) ➞ "Boom!"
# # 97 contains the number seven.



#The Nearest Element

nearest_element <- function(n,x){
  result <- numeric(0)
  for (i in seq_len(length(x))) {
    result[[length(result) +1]] <-abs(n - x[[i]]) 
  }
  
  fi.res <-which(result == min(result)) 
  which(x == max(x[fi.res]))
}


# nearest_element(10, c(1, 100, 1000)) ➞ 0
# # 1 is the number nearest to 10.
# 
# nearest_element(50, c(100, 49, 51)) ➞ 2
# # 49 and 51 are equally distant from 50, with 51 being the greatest.
# 
# nearest_element(-20, c(-50, -10, -30)) ➞ 1
# # -10 and -30 are equally distant from -20, with -10 being the greatest.

