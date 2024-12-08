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