pop <- function(x){
  if( length(x) == 1) return(x)
  
  pos<-which(x !=0)
  ok <-x[which(x !=0)]
  lo<-seq(ok-1,0)
  lo<-c(rep(0,pos),lo)
  
  for (i in seq_len(length(x))) {
    if(i <= pos){
      x[[i]]  <- i -1
    }
    if( i > pos){
      x[[i]] <- lo[[i]]
    }
    
    
  }
  x
  
}

pop(c(0,1,0))
# pop(c(0, 0, 0, 0, 4, 0, 0, 0, 0)) ➞ [0, 1, 2, 3, 4, 3, 2, 1, 0]
# 
# pop(c(0, 0, 0, 3, 0, 0, 0)) ➞ [0, 1, 2, 3, 2, 1, 0]
# 
# pop(c(0, 0, 2, 0, 0)) ➞ [0, 1, 2, 1, 0]
# 
# pop(c(0)) ➞ [0]

x <- c(0, 0, 0, 3, 0, 0, 0)

pos<-which(x !=0)
ok <-x[which(x !=0)]
lo<-seq(ok-1,0)
lo<-c(rep(0,pos),lo)

for (i in seq_len(length(x))) {
  if(i <= pos){
      x[[i]]  <- i -1
    }
    if( i > pos){
      x[[i]] <- lo[[i]]
    }
     
 
}
x
