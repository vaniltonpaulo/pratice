#day 50
track_robot <- function(x){
  x.axis <-0
  y.axis <- 0
  
  
  
  for (i in seq_along(x)) {
    main.x<-x[[i]]
    result<-strsplit(main.x," ")[[1]]
    
    if(result[[1]] == "right"){
      x.axis <- x.axis  + as.numeric(result[[2]])
    }
    if(result[[1]] == "up"){
      y.axis <- y.axis  + as.numeric(result[[2]])
    } 
    if(result[[1]] == "left"){
      x.axis <- x.axis  - as.numeric(result[[2]])
    } 
    
    if(result[[1]] == "down"){
      y.axis <- y.axis  - as.numeric(result[[2]])
    } 
    
  }
  return(c(x.axis,y.axis)) 
}

#track_robot(c("right 10", "up 50", "left 30", "down 10")) ➞ [-20, 40]

#track_robot(c()) ➞ [0, 0]
#// If there are no instructions, the robot doesn't move.

#track_robot(c("right 100", "right 100", "up 500", "up 10000")) ➞ [200, 10500]

track_robot(c("left 10", "left 100", "left 1000", "left 10000"))




gen_tiles <- function(x){
  numbers <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  pinyin <- c("yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu")
  
  # Combine them into a matrix
  mt <- cbind(Number = numbers, Pinyin = pinyin)
  
  
  
  result<-strsplit(x," ")[[1]]
  word<-result[[1]]
  
  codebook <-c("Five" = 5,"Seven" = 7,"One" = 1, "Three" = 3)
  num<-codebook[[which(word == names(codebook))]]
  toreplaceword<-mt[match(num,mt),2]
  final <-sub(word,toreplaceword,x)
  final <- gsub("of","",final)
  
  final<- gsub("\\s+", " ",final)
  final
  
}







# gen_tiles("Five of tong") ➞ "wu tong"
# 
# gen_tiles("Seven of wan") ➞ "qi wan"
# 
# gen_tiles("One of tiao") ➞ "ji"
# 
# gen_tiles("Three of tiao") ➞ "san tiao"



pile_of_cubes<- function(m){
  
  counter <- 0
  x <- 0

  while(x < m){
    x <- x + counter^3
    counter <- counter + 1
    if(x == m){
      return(counter -1)
    }
    if(x > m) return("NONE")
  }
  
}



# pile_of_cubes(1071225) ➞ 45
# 
# pile_of_cubes(4183059834009) ➞ 2022
# 
# pile_of_cubes(16) ➞ None



get_frame <- function(x,y,z){
  if(x <= 2 || y <= 2) stop("Invalid")
  
  
  mt <- matrix(numeric(0),nrow = y,ncol = 1)
  
  
  for (i in seq_len(nrow(mt))) {
    if(i == 1 || i == length(seq_len(y))){
      mt[i,1] <- paste0(rep(z,x),collapse = "")
      
    }else{
      mt[i,1] <- paste0("#",paste(rep(" ",x-2),collapse = ""),"#")
      
    }
    
  }
  mt
  
}

# get_frame(4, 5, "#") ➞ [
#   ["####"],
#   ["#  #"],
#   ["#  #"],
#   ["#  #"],
#   ["####"]
# ]
# # Frame is 4 characters wide and 5 characters tall.
# 
# 
# get_frame(10, 3, "*") ➞ [
#   ["**********"],
#   ["*        *"],
#   ["**********"]
# ]
# # Frame is 10 characters and wide and 3 characters tall.
# 
# 
# get_frame(2, 5, "0") ➞ "invalid"
# # Frame's width is not more than 2.





# fibo(1) ➞ 1
# 
# fibo(2) ➞ 1
# 
# fibo(3) ➞ 2
# 
# fibo(6) ➞ 8
# 
# fibo(30) ➞ 832040

fibo <- function(x){
  if( x== 2) return(1)
  if( x== 1) return(1)
  
  fibo(x-1) + fibo(x-2)
  
}




