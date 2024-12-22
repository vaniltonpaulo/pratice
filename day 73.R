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


get_middle <- function(x) {
  y <- strsplit(x,"")[[1]]
  result <- nchar(x)
  imp <- result/2
  ifelse(result %% 2 == 0,paste0(y[[imp]],y[[imp + 1]],collapse = "") ,y[[imp + 1]])
}

# get_middle("test") ➞ "es"
# 
# get_middle("testing") ➞ "t"
# 
# get_middle("middle") ➞ "dd"
# 
# get_middle("A") ➞ "A"
get_middle("cabinet")



retrieve <- function(x){
  x <- tolower(x)
  regmatches(x,gregexpr("(?<![bcdfghjklmnpqrstvwxyzaeiou])[aeiou][a-z]*",x,perl = TRUE))[[1]]
}


# retrieve("A simple life is a happy life for me.") ➞ ["a", "is", "a"]
# 
# retrieve("Exercising is a healthy way to burn off energy.")
# ➞ ["exercising", "is", "a", "off", "energy"]
# 
# retrieve("The poor ostrich was ostracized.")
# ➞ ["ostrich", "ostracized"]
# 
# retrieve("")
# ➞ []

x <- "A simple life is a happy life for me."
x <- tolower(x)
regmatches(x,gregexpr("(?<![bcdfghjklmnpqrstvwxyz])[aeiou][a-z]*",x,perl = TRUE))[[1]]