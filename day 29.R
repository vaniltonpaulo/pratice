#Majority Vote

majority_vote <- function(l){
  more.than <-length(l)/2
  frequency <- table(l)
  result <- names(frequency)[c(frequency >more.than)]
  if(length(result) == 0) return("None") else return(result)
}

# majority_vote(c("A", "A", "B")) ➞ "A"
# 
# majority_vote(c("A", "A", "A", "B", "C", "A")) ➞ "A"
# 
# majority_vote(c("A", "B", "B", "A", "C", "C")) ➞ None

majority_vote(c("B", "A", "B", "B", "C", "A", "B", "B"))



#Loves Me, Loves Me Not...
loves_me <- function(n){
  
  result <-character(0)
  
  for (i in seq_len(n -1)) {
    if(i %% 2 != 0){
      result[[length(result) + 1]] <- "Loves me"
      
    } else{
      result[[length(result) + 1]] <- "Loves me not"
      
    }
  }
  if(length(result) %% 2 != 0){
    result[[length(result) + 1]] <-"LOVES ME NOT"
  }else{
    result[[length(result) + 1]] <-"LOVES ME"
    
  }
  paste0(result,collapse = ", ")
}
# 
# loves_me(3) ➞ "Loves me, Loves me not, LOVES ME"
# 
# loves_me(6) ➞ "Loves me, Loves me not, Loves me, Loves me not, Loves me, LOVES ME NOT"
# 
# loves_me(1) ➞ "LOVES ME"
loves_me(38)


#Factorial of Factorials
fact_of_fact <- function(n){
  result <- 1
  for (i in seq_len(n)) {
    result <- result * factorial(i)
  }
  result
  
}


# fact_of_fact(4) ➞ 288
# # 4! * 3! * 2! * 1! = 288
# 
# fact_of_fact(5) ➞ 34560
# 
# fact_of_fact(6) ➞ 24883200



#Pluralize!
pluralize <- function(x){
  if(any(duplicated(x))) {
    m <- gregexpr(unique(x[table(x) >1]),x,perl = TRUE)
    regmatches(x,m) <- paste(unique(x[table(x) >1]),"s",sep  = "")
    
   return(unique(x))
  } else return(x)
  
 
}

# pluralize(c("cow", "pig", "cow", "cow")) ➞ { "cows", "pig" }
# 
# pluralize(c("table", "table", "table")) ➞ { "tables" }
# 
# pluralize(c("chair", "pencil", "arm")) ➞ { "chair", "pencil", "arm" }


