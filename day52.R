split_n_cases <- function(x, y) {
  
  chunk_size<-nchar(x) / y
  
  startposition <- seq(1,nchar(x), by = chunk_size)
  substring(x,startposition, startposition + chunk_size -1)
}


# split_n_cases("Strengthened", 6) ➞ ["St", "re", "ng", "th", "en", "ed"]
# 
# split_n_cases("Unscrupulous", 2) ➞ ["Unscru", "pulous" ]
# 
# split_n_cases("Flavorless", 1) ➞ ["Flavorless" ]


x<-"Strengthened"
y<-6


chunk_size<-nchar(x) / y

startposition <- seq(1,nchar(x), by = chunk_size)
substring(x,startposition, startposition + chunk_size -1)