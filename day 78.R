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

