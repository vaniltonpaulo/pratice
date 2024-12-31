best_friend <- function(x,t,u) {
  y <- paste0(t,u,collapse = "")
  result.1<- regmatches(x,gregexpr(paste0(t),x,perl =TRUE))[[1]]
  result.2 <- regmatches(x,gregexpr(paste0(y),x,perl =TRUE))[[1]]
  ifelse(isTRUE(all.equal(length(result.1),length(result.2))),TRUE,FALSE)
}

# best_friend("he headed to the store", "h", "e") ➞ True
# 
# best_friend("i found an ounce with my hound", "o", "u") ➞ True
# 
# best_friend("we found your dynamite", "d", "y") ➞ False

best_friend('go to edabit and meditate', 'e', 'd')

absolute <- function(x) {
  x<- gsub("^A(?=\\s)","An absolute",x,perl = TRUE)
  gsub("(?<=\\s)(a)(?=\\s)","an absolute",x,perl = TRUE)
}

# absolute("I am a champion!!!") ➞ "I am an absolute champion!!!"
# 
# absolute("Such an amazing bowler.") ➞ "Such an amazing bowler."
# 
# absolute("A man with no haters.") ➞ "An absolute man with no haters."

absolute("That place is such a beauty")




absolute <- function(x) {
  #here if you dont assign it back to x, it converts back to the original x
  x<- gsub("^A(?=\\s)","An absolute",x,perl = TRUE)
  gsub("(?<=\\s)(a)(?=\\s)","an absolute",x,perl = TRUE)
}

# absolute("I am a champion!!!") ➞ "I am an absolute champion!!!"
# 
# absolute("Such an amazing bowler.") ➞ "Such an amazing bowler."
# 
# absolute("A man with no haters.") ➞ "An absolute man with no haters."


x_pronounce <- function(x) {
  x<-gsub("(?<=\\s)x(?=[a-z])","z",x,perl = TRUE)
  x<-gsub("(?<=\\s)(x)(?=\\s)","ecks",x,perl = TRUE)
  gsub("x","cks",x,perl = TRUE)
}

# x_pronounce("Inside the box was a xylophone") ➞ "Inside the bocks was a zylophone"
# 
# x_pronounce("The x ray is excellent") ➞ "The ecks ray is eckscellent"
# 
# x_pronounce("OMG x box unboxing video x D") ➞ "OMG ecks bocks unbocksing video ecks D"


trouble <- function(x,y) {
  
  x <- as.character(x)
  x <- strsplit(x,"")[[1]]
  result.1<-names(table(x))[table(x) == 3]
  
  
  y <- as.character(y)
  y <- strsplit(y,"")[[1]]
  result.2<-names(table(y))[table(y) == 2]
  isTRUE(all.equal(result.1,result.2))
}




trouble <- function(num1, num2) {
  # Convert numbers to strings
  num1 <- as.character(num1)
  num2 <- as.character(num2)
  
  # Iterate over each character in num2
  for (i in strsplit(num2, "")[[1]]) {
    # Check if the character appears twice in a row in num2 and three times in a row in num1
    if (grepl(paste0(i, i), num2) && grepl(paste0(i, i, i), num1)) {
      return(TRUE)
    }
  }
  
  # If no match is found, return FALSE
  return(FALSE)
}

# trouble(451999277, 41177722899) ➞ True
# 
# trouble(1222345, 12345) ➞ False
# 
# trouble(666789, 12345667) ➞ True
# 
# trouble(33789, 12345337) ➞ False
