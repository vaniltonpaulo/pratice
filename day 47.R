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

middle_earth <- function(x){
  if( which(x == "Frodo")+ 1 ==  which(x == "Sam")) return(TRUE)
  if( which(x == "Sam")+ 1 ==  which(x == "Frodo")) return(TRUE)
  FALSE
  
}


# middle_earth(c("Frodo", "Sam", "Gandalf")) ➞ True
# 
# middle_earth(c("Frodo", "Saruman", "Sam")) ➞ False
# 
# middle_earth(c("Orc", "Sam", "Frodo", "Legolas")) ➞ True




is_omnipresent <- function(x,y){
  result<-lapply(x, function(k){
    y %in% k
  })
  
  all(unlist(result))
}




# is_omnipresent(list(c(1, 1), c(1, 3), c(5, 1), c(6, 1)), 1) ➞ True
# 
# is_omnipresent([[1, 1], [1, 3], [5, 1], [6, 1]], 6) ➞ False
# 
# is_omnipresent([[5], [5], [5], [6, 5]], 5) ➞ True
# 
# is_omnipresent(list(c(5), c(5), c(5), c(6, 5)), 6) ➞ False



####Easy

stutter <- function(x){
  first.two<-substr(x,1,2)
  paste0(first.two,"...",first.two,"...",x,"?")
}
# stutter("incredible") ➞ "in... in... incredible?"
# 
# stutter("enthusiastic") ➞ "en... en... enthusiastic?"
# 
# stutter("outstanding") ➞ "ou... ou... outstanding?"


solve_for_exp <- function(x,y){
  result<- numeric(0)
  
  for (i in seq_len(y)) {
    if((x^i) == y){
      result[[length(result) +1 ]] <- i
    }
    
  }
  result
}

# solve_for_exp(4, 1024) ➞ 5
# 
# solve_for_exp(2, 1024) ➞ 10
# 
# solve_for_exp(9, 3486784401) ➞ 10



missing_num <- function(x){
  x<-sort(x)
  y<-seq_len(10)
  result<- numeric(0)
  
  for (i in seq_len(10)) {
    if(  all(sort(append(x,i)) == y)){
      result[[length(result) +1 ]] <- i
      
    }
  }
  
  result
}



# missing_num(c(1, 2, 3, 4, 6, 7, 8, 9, 10)) ➞ 5
# 
# missing_num(c(7, 2, 3, 6, 5, 9, 1, 4, 8)) ➞ 10
# 
# missing_num(c(10, 5, 1, 2, 4, 6, 8, 3, 9)) ➞ 7



square_digits <- function(x){
  x <- as.character(x)
  
  result<-strsplit(x,"")[[1]]
  result<-as.numeric(result)
  
  final<-result ^2
  
  as.numeric(paste0(final,collapse = ""))
  
}



# square_digits(9119) ➞ 811181
# 
# square_digits(2483) ➞ 416649
# 
# square_digits(3212) ➞ 9414


paths <- function(n){
  factorial(n)
}

# paths(4) ➞ 24
# 
# paths(1) ➞ 1
# 
# paths(9) ➞ 362880


century_from_year <- function(x){
  if(between(x,1801 , 1900)) return(19)
  if(between(x,1901, 2000)) return(20)
  return(21)
  
}

# century_from_year(2005) ➞ 21
# 
# century_from_year(1950) ➞ 20
# 
# century_from_year(1900) ➞ 19


get_discounts <- function(x,y){
  num<-regmatches(y,gregexpr("[0-9]+",y,perl = TRUE))[[1]]
  num <- as.numeric(num)
  
  percentange<-num/100
  x * percentange
}


# get_discounts(c(2, 4, 6, 11), "50%") ➞ [1, 2, 3, 5.5]
# 
# get_discounts(c(10, 20, 40, 80), "75%") ➞ [7.5, 15, 30, 60]
# 
# get_discounts(c(100), "45%") ➞ [45]



even_or_odd <- function(x){
  result <- strsplit(x,"")[[1]]
  result <- as.numeric(result)
  even.num <- sum(result[result %% 2 == 0])
  odd.num <- sum(result[result %% 2 == 1])
  
  if(even.num > odd.num) return("Even is greater than Odd")
  if(even.num < odd.num) return("Odd is greater than Even")
  if(even.num == odd.num) return("Even and Odd are the same")
}

# even_or_odd("22471") ➞ "Even and Odd are the same"
# 
# even_or_odd("213613") ➞ "Even and Odd are the same"
# 
# even_or_odd("23456") ➞ "Even is greater than Odd"


x <-"22471"

result <- strsplit(x,"")[[1]]
result <- as.numeric(result)
even.num <- result[result %% 2 == 0]
odd.num <- result[result %% 2 == 1]



############# Medium start






