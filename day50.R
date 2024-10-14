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
