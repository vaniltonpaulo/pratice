
check_score <-  function(x) {
  point_values <- c("#" = 5, "!" = -1, "!!" = -3, "!!!" = -5, "O" = 3, "X" = 1)
  result <-  numeric(0)
  
  
  for (i in seq_len(length(x))) {
    result[[length(result) + 1]] <- sum(point_values[names(point_values) %in% x[[i]]])
  }
  ifelse(sum(result) < 0,0,sum(result))
  
}


# Test cases
check_score(list(
  c("#", "!"),
  c("!!", "X")
))# Output: 2

check_score(list(
  c("!!!", "O", "!"),
  c("X", "#", "!!!"),
  c("!!", "X", "O")
)) # Output: 0

check_score(list(
  c("#", "O", "#", "!!", "X", "!!", "#", "O", "O", "!!", "#", "X", "#", "O"),
  c("!!!", "!!!", "!!", "!!", "!", "!", "X", "!", "!!!", "O", "!", "!!!", "X", "#"),
  c("#", "X", "#", "!!!", "!", "!!", "#", "#", "!!", "X", "!!", "!!!", "X", "O"),
  c("!!", "X", "!!", "!!", "!!!", "#", "O", "O", "!!!", "#", "O", "O", "#", "!!"),
  c("O", "X", "#", "!", "!", "X", "!!!", "O", "!!!", "!!", "O", "!", "O", "X"),
  c("!!", "!!!", "X", "!!!", "!!", "!!", "!!!", "X", "O", "!", "#", "!!", "!!", "!!!"),
  c("!!", "!!", "#", "O", "!", "!!", "!", "!!!", "#", "O", "#", "!", "#", "!!"),
  c("X", "X", "O", "X", "!!!", "#", "!!!", "!!!", "X", "X", "X", "!", "#", "!!"),
  c("O", "!!!", "!", "O", "#", "!", "!", "#", "X", "X", "#", "O", "!!", "!"),
  c("X", "!", "!!", "#", "#", "X", "!!", "O", "!!", "X", "X", "!!", "#", "X"),
  c("!", "!!", "!!", "O", "!!", "!!", "#", "#", "!", "!!!", "O", "!", "#", "#"),
  c("!", "!!!", "!!", "X", "!!", "!!", "#", "!!!", "O", "!!", "!!!", "!", "!", "!"),
  c("!!!", "!!!", "!!", "O", "!", "!", "!!!", "!!!", "!!", "!!", "X", "!", "#", "#"),
  c("O", "O", "#", "O", "#", "!", "!!!", "X", "X", "O", "!", "!!!", "X", "O")
)) # Output: 12


first_before_second <-  function(string, first, second) {
  # Get the positions of the letters in the string
  first_positions <- gregexpr(first, string)[[1]]
  second_positions <- gregexpr(second, string)[[1]]
  
  # If either letter is not found, return TRUE since the condition is trivially satisfied
  if (first_positions[1] == -1 || second_positions[1] == -1) {
    return(TRUE)
  }
  
  # Check if the maximum position of 'first' is less than the minimum position of 'second'
  max(first_positions) < min(second_positions)
}
# first_before_second("a rabbit jumps joyfully", "a", "j") ➞ True
# # Every instance of "a" occurs before every instance of "j".
# 
# first_before_second("knaves knew about waterfalls", "k", "w") ➞  True
# 
# first_before_second("happy birthday", "a", "y") ➞ False
# # The "a" in "birthday" occurs after the "y" in "happy".
# 
# first_before_second("precarious kangaroos", "k", "a") ➞ False



can_build <-  function(x,y) {
  x <- gsub(" ","",x)
  y <- gsub(" ","",y)
  
  x.result <- strsplit(x,"")[[1]]
  so <- table(x.result)
  
  y.result <- strsplit(y,"")[[1]]
  ko <- table(y.result)
  
  for (i in names(so)) {
    if(so[i] > ko[i]) {
      return(FALSE)
    }
    
  }
  return(TRUE)
}


# can_build("got 2 go", "gogogo 2 today") ➞ True
# 
# can_build("sit on top", "its a moo point") ➞ True
# 
# can_build("long high add or", "highway road go long") ➞ False
# 
# can_build("fill tuck mid", "truck falls dim") ➞ False