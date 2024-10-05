next_edge <- function(x,y){
  result <- (x+y) -1
  result
}

# next_edge(8, 10) ➞ 17
# 
# next_edge(5, 7) ➞ 11
# 
# next_edge(9, 2) ➞ 10



animals <- function(x,y,z){
  
  chickens <- x *2
  cows <- y * 4
  pigs <- z * 4
  
  sum(chickens,cows,pigs)
}


# animals(2, 3, 5) ➞ 36
# 
# animals(1, 2, 3) ➞ 22
# 
# animals(5, 2, 8) ➞ 50



makes10 <- function(a,b){
  if( a == 10 || b == 10 ) return(TRUE)
  if(sum(a,b) == 10) return(TRUE)
  
  FALSE
}

# makes10(9, 10) ➞ True
# 
# makes10(9, 9) ➞ False
# 
# makes10(1, 9) ➞ True



frames <- function(x,y){
  frame_amount <- x * 60
  result <- frame_amount * y
  result
}


# frames(1, 1) ➞ 60
# 
# frames(10, 1) ➞ 600
# 
# frames(10, 25) ➞ 15000
frames(52, 33)


even_or_odd <- function(...){
  if(missing(...)) return("even")
ifelse(  sum(unlist(list(...))) %% 2 == 0,"even","odd")
  }

# even_or_odd(0) ➞ "even"
# 
# even_or_odd(1) ➞ "odd"
# 
# even_or_odd() ➞ "even"
# 
# even_or_odd(0, 1, 5) ➞ "even"



area_shape <- function(x,y,z){
  ifelse(z == "triangle",0.5 *x*y, x *y)
}


# area_shape(2, 3, "triangle") ➞ 3
# 
# area_shape(8, 6, "parallelogram") ➞ 48
# 
# area_shape(2.9, 1.3, "parallelogram") ➞ 3.77


future_people <- function(currentPopulation, birthsPerMonth){
  threeDecades <-3*10
  months.in.decade <-decades * 12
  
  birthsInThreeDecades <-months.in.decade *birthsPerMonth
  futurePop <-birthsInThreeDecades + currentPopulation
  futurePop
}


# future_people(256, 2) ➞ 976
# 
# future_people(3248, 6) ➞ 5408
# 
# future_people(5240, 3) ➞ 6320

count_d <- function(x){
  result<-strsplit(x,"")[[1]]
  
  final<-regmatches(x,gregexpr("d|D",x,perl = TRUE))[[1]]
  sum(nchar(final))
}

# count_d("My friend Dylan got distracted in school.") ➞ 4
# 
# count_d("Debris was scattered all over the yard.") ➞ 3
# 
# count_d("The rodents hibernated in their den.") ➞ 3



accept_into_movie <- function(x,y){
  if(x >= 15) return(TRUE)
  ifelse(x < 15 & y == TRUE,TRUE,FALSE)
  
}

# accept_into_movie(14, TRUE) ➞ True
# 
# accept_into_movie(14, FALSE) ➞ False
# 
# accept_into_movie(16, FALSE) ➞ True


stack_boxes <- function(x){
  if( x == 1) return(1)
  if( x == 0) return(0)
  
  result <- x*x
  result
}


# stack_boxes(1) ➞ 1
# 
# stack_boxes(2) ➞ 4
# 
# stack_boxes(0) ➞ 0
stack_boxes(512)

stack_boxes(27)



programmers <- function(...){
  x <- unlist(list(...))
  max(x) -min(x)
}

# programmers(147, 33, 526) ➞ 493
# 
# programmers(33, 72, 74) ➞ 41
# 
# programmers(1, 5, 9) ➞ 8

leap_year <- function(x)
  if(substr(x,nchar(x) -1 ,nchar(x)) == "00"){
     return( x%% 400 ==0)
  }else{
    return(x %% 4 == 0)
  }



# leap_year(2020) ➞ True
# 
# leap_year(2021) ➞ False
# 
# leap_year(1968) ➞ True
leap_year(2024)
leap_year(2100)


sum_cubes <- function(x){
  sum((seq_len(x))^3)
}


# sum_cubes(7) ➞ 784
# 
# sum_cubes(8) ➞ 1296
# 
# sum_cubes(9) ➞ 2025


total_cups <- function(x){
  x + x %/% 6
}

# total_cups(6) ➞ 7
# 
# total_cups(12) ➞ 14
# 
# total_cups(213) ➞ 248


largest_numbers <- function(y,x){
  if(y == 0) return(numeric(0))
  x<-sort(x,decreasing = TRUE)
  
  result<- numeric(0)
  for (i in seq_len(y)) {
    result[[length(result) + 1]] <- x[i]
    
  }
  sort(result,decreasing = FALSE)
}

# largest_numbers(2, c(4, 3, 2, 1)) ➞ [3, 4]
# 
# largest_numbers(1, c(7, 19, 4, 2)) ➞ [19]
# 
# largest_numbers(3, c(14, 12, 57, 11, 18, 16)) ➞ [16, 18, 57]
# 
# largest_numbers(0, c(1, 3, 4, 2) ➞ []




n_tables_plus_one <- function(x){
  n <- 1
  
  result <- numeric(0)
  
  repeat{
    
    if((n %% x == 0) == TRUE){
      result[length(result) + 1] <- n
    }
    
    if(length(result) == 10){
      break
    } 
    
    
    n <- n +1
  }
  result +1
}



# n_tables_plus_one(7) ➞ "8,15,22,29,36,43,50,57,64,71"
# 
# n_tables_plus_one(1) ➞ "2,3,4,5,6,7,8,9,10,11"
# 
# n_tables_plus_one(3) ➞ "4,7,10,13,16,19,22,25,28,31"


