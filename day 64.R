idMtrx <- function(n){
  if( n >0){
    mat <- matrix(0,ncol = n,nrow = n)
    
    for (i in seq_len(nrow(mat))) {
      mat[i,i] <- 1
    }
    return(mat)
  }else{
    n <- n * -1
    mat <- matrix(0,ncol = n,nrow = n)
    for (i in seq_len(nrow(mat))) {
      mat[i,i] <- 1
    }
    return(  apply(mat, 2, rev))
  }
 
}


# idMtrx(2) ➞ [
#   [1, 0],
#   [0, 1]
# ]
# 
# idMtrx(-2) ➞ [
#   [0, 1],
#   [1, 0]
# ]
# 
# idMtrx(0) ➞ []
idMtrx(10)
idMtrx(-10)

io <- "hjd"

