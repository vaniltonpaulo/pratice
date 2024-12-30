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