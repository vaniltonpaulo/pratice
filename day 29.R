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



#3n + 1 Problem (Collatz Conjecture)
collatz <- function(x,y){
  
  ge.it <- function(k){
    result <- k
    count <- 1
    repeat{
      if(result %% 2 == 0){
        result <- result /2
      } else{
        result <- (result * 3) + 1
      }
      if(result == 1) break
      
      count <- count + 1
    }
    count
  }
  if(ge.it(x)<ge.it(y)) return("a") else("b")
}

# 
# collatz(10, 15) ➞ "a"
# # Because 10.0 - 5.0 - 16.0 - 8.0 - 4.0 - 2.0 - 1.0: 6 steps
# # 15.0 - 46.0 - 23.0 - 70.0 - 35.0 - 106.0 - 53.0 - 160.0 - 80.0 - 40.0 - 20.0 - 10.0 - 5.0 - 16.0 - 8.0 - 4.0 - 2.0 - 1.0: 17 steps
# 
# collatz(13, 16) ➞ "b"
#collatz(53782, 72534) ➞ "b"

collatz(1723817263, 837249873748)
collatz(72221, 11198)


##############
#UB6 CART
install.packages("mlr3verse")
install.packages("rattle")
library(mlr3verse)
library(rattle)
task <- tsk("bike_sharing")
task$select(task$feature_names[task$feature_names != "date"])
for (i in c(2, 4, 8)){
learner <- lrn("regr.rpart", minsplit = 2, maxdepth = i, minbucket = 1)
learner$train(task)
fancyRpartPlot(learner$model, caption = sprintf("maxdepth: %i", i))
}



for (i in c(5, 1000, 10000)) {
learner <- lrn("regr.rpart", minsplit = i, maxdepth = 20, minbucket = 1)
learner$train(task)
fancyRpartPlot(learner$model, caption = sprintf("minsplit: %i", i))
}