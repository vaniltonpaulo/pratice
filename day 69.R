nsum <- function(n) {
  result <- numeric(0)
  for (i in seq_len(n)) result[[length(result) + 1]] <-  i
  sum(result)
}
nsum(10)


df1 <- read.csv(text = "  a,  b,  c
 10, NA, 10
 NA,  9, NA
  1,  2,  3
  4,  5,  6
", colClasses = "numeric")

# Expected:
#> [1] 1 2 0 0

countMissings <- function(df) {
  # your code
  if(nrow(df) > 0 && ncol(df) == 0) return(0)
  if(ncol(df) == 0 | nrow(df) == 0) return(numeric(0))
  x<-apply(df, 2, is.na)
  apply(x,1,sum)
}

dropMissingCols <- function(df) {
  drop <- character(0)
  for (col in colnames(df)) {
    for (row in seq_len(nrow(df))) {
      if (is.na(df[row, col])) drop <- union(drop, col)
    }
  }
  df[, setdiff(colnames(df), drop), drop = FALSE]
}

dropMissingCols.1 <- function(df) {
  result<-vapply(colnames(df),function(k){
    if(any(is.na(df[,k])) == FALSE){
      return(k)
    }else{
      "pdf"
    }
  },character(1))
  
  final <-result[result !="pdf"]
  df[,final,drop = FALSE]
}

library(microbenchmark)

microbenchmark(
  dropMissingCols(testdf),
  dropMissingCols.1(testdf),
  times = 100  # Number of iterations
)


testdf <- data.frame(x = c(1, 2, 3), y = c("A", "B", NA), z = c(1, NA, 2))
dropMissingCols(testdf)  # data.frame with all coluns that do not contain missings: 'x', in this case




hoFib.1 <- function(n, x) {
  if (n < 1) return(0)
  if (n == 1) return(1)
  #result <- 0
  result <- numeric(x)   
  result[[1]] <- 0
  for (i in seq_len(x)) {
    result[[length(result) + 1]] <- result[[i]] + hoFib(n - i, x)
  }
  return(  sum(result))
}

vapply(seq_len(10), function(n) hoFib(n, 3), numeric(1))  # c(1, 1, 2, 4, 7, 13, 24, 44, 81, 149)



microbenchmark(
  hoFib(10,3),
  hoFib.1(10,3),
  times = 100  # Number of iterations
)
system.time({
  hoFib.1(10,3)
})