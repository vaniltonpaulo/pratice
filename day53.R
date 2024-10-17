make_happy <- function(x){
  result <-gsub("([:8x;])\\(", "\\1)", x)
  result
}


# make_happy("My current mood: :(") ➞ "My current mood: :)"
# 
# make_happy("I was hungry 8(") ➞ "I was hungry 8)"
# 
# make_happy("print('x(')") ➞ "print('x)')"

evenregex <- function(x){
  last.x <- substr(x,nchar(x),nchar(x))
  
  so<-regmatches(last.x,gregexpr("[24680]",last.x,perl = TRUE))[[1]]
  if(length(so) == 0) return(FALSE) else return(TRUE)
}

# evenregex("2341") ➞ false
# 
# evenregex("132") ➞ true
# 
# evenregex("29") ➞ false
# 
# evenregex("5578") ➞ true


split_code <- function(x){
  res.1 <- strsplit(x,"[0-9]+")[[1]]
  res.2<-strsplit(x,"[A-z]+")[[1]]
  res.2<- res.2[[2]]
  
  final <-c(res.1,as.numeric(res.2))
  final
}

# split_code("TEWA8392") ➞ ["TEWA", 8392]
# 
# split_code("MMU778") ➞ ["MMU", 778]
# 
# split_code("SRPE5532") ➞ ["SRPE", 5532]
split_code("WIEB3921")


