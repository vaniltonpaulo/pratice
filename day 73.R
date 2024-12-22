letter_check <- function(x){
  x <- tolower(x)
  
  result <-strsplit(x[[2]], "")[[1]]
  final <- strsplit(x[[1]],"")[[1]]
  
  isTRUE(all(result %in% final))
}
# letter_check(c("trances", "nectar")) ➞ True
# 
# letter_check(c("compadres", "DRAPES")) ➞ True
# 
# letter_check(c("parses", "parsecs")) ➞ False

reverse.123 <- function(x){
  result <-strsplit(x,"\\s+")[[1]]
  soo<-lapply(result, function(k){
    if(nchar(k) >= 5){
      final<-strsplit(k,"")[[1]]
      almost<- rev(final)
      paste0(almost,collapse = "")
    }else{
      k
    }
  })
  paste(soo, collapse = " ")
}
# reverse.123("Reverse") ➞ "esreveR"
# 
# reverse.123("This is a typical sentence.") ➞ "This is a lacipyt .ecnetnes"
# 
# reverse.123("The dog is big.") ➞ "The dog is big."


x <- "This is a typical sentence."
result <-strsplit(x,"\\s+")[[1]]
soo<-lapply(result, function(k){
  if(nchar(k) >= 5){
    final<-strsplit(k,"")[[1]]
    almost<- rev(final)
    paste0(almost,collapse = "")
  }else{
    k
  }
})
paste(soo, collapse = " ")