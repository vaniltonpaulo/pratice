calculate_fuel <- function(x) {
  ifelse(x* 10 > 100, x*10, 100)
}

# calculate_fuel(15) ➞ 150
# 
# calculate_fuel(23.5) ➞ 235
# 
# calculate_fuel(3) ➞ 100

flip <-  function(x) {
  ifelse(x== 1, 0,1)
}

# flip(1) ➞ 0
# 
# flip(0) ➞ 1



say_hello_bye <-  function(x,y){
  ifelse(y == 1,paste("Hello",x), paste("Bye",x))
}

# say_hello_bye("alon", 1) ➞ "Hello Alon"
# 
# say_hello_bye("Tomi", 0) ➞ "Bye Tomi"
# 
# say_hello_bye("jose", 0) ➞ "Bye Jose"


test_jackpot <-  function(x) {
  ifelse(length(unique(x)) == 1, TRUE,FALSE)
}
# test_jackpot(c("@", "@", "@", "@")) ➞ True
# 
# test_jackpot(["abc", "abc", "abc", "abc"]) ➞ True
# 
# test_jackpot(["SS", "SS", "SS", "SS"]) ➞ True
# 
# test_jackpot(c("&&", "&", "&&&", "&&&&")) ➞ False
# 
# test_jackpot(["SS", "SS", "SS", "Ss"]) ➞ False


hurdle_jump <-  function(x,y) {
  all(y>=x)
}
# hurdle_jump(c(1, 2, 3, 4, 5), 5) ➞ True
# 
# hurdle_jump(c(5, 5, 3, 4, 5), 3) ➞ False
# 
# hurdle_jump([5, 4, 5, 6], 10) ➞ True
# 
# hurdle_jump([1, 2, 1], 1) ➞ False


transform <-  function(x) {
  for (i in seq_len(length(x))) {
    if(x[[i]] %% 2 == 0){
      x[[i]] <-  x[[i]] - 1
    }else{
      x[[i]] <-  x[[i]] + 1
    }
    
  }
  x
}
# transform(c(1, 2, 3, 4, 5)) ➞ [2, 1, 4, 3, 6]
# 
# transform([3, 3, 4, 3]) ➞ [4, 4, 3, 4]
# 
# transform(c(2, 2, 0, 8, 10)) ➞ [1, 1, -1, 7, 9]


chatroom_status <-  function(x) {
  if(length(x)< 1) return("no one online")
  if(length(x)== 1) return(paste(x,"online"))
  if(length(x) == 2) return(paste(paste0(x,collapse =  " and "),"online"))
  if(length(x) > 2) return(paste(paste(x[[1]],x[[2]],sep =  ", "),"and",length(x) - 2,"more online"))
  
  
}
# chatroom_status(c()) ➞ "no one online"
# 
# chatroom_status(c("paRIE_to")) ➞ "paRIE_to ç
# 
# chatroom_status(c("s234f", "mailbox2")) ➞ "s234f and mailbox2 online"
# 
# chatroom_status(c("pap_ier44", "townieBOY", "panda321", "motor_bike5", "sandwichmaker833", "violinist91"))
# ➞ "pap_ier44, townieBOY and 4 more online"


count_palindromes <-  function(x,y) {
  result <- seq(x,y)
  final <-  logical(0)
  for (i in seq_len(length(result))) {
    if(isTRUE(paste0(rev(strsplit(as.character(result[[i]]),"")[[1]]),collapse = "") == result[[i]])) {
      final[[length(final) + 1]] <- TRUE
    }
    
  }
  
  length(final)
}
# count_palindromes(1, 10) ➞ 9
# 
# count_palindromes(555, 556) ➞ 1
# 
# count_palindromes(878, 898) ➞ 3


hacker_speak <-  function(x) {
  chartr("aeios","43105",x)  
}
# hacker_speak("javascript is cool") ➞ "j4v45cr1pt 15 c00l"
# 
# hacker_speak("programming is fun") ➞ "pr0gr4mm1ng 15 fun"
# 
# hacker_speak("become a coder") ➞ "b3c0m3 4 c0d3r"


century <-  function(x) {
  result <- as.numeric(substr(x,2,2)) + 1
  paste(paste0("1",result,"th"),"century")
}
# century(1756) ➞ "18th century"
# 
# century(1555) ➞ "16th century"
# 
# century(1000) ➞ "10th century"
# 
# century(1001) ➞ "11th century"
# 
# century(2005) ➞ "21st century"
