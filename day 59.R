#very easy strings

bool_to_string <- function(x){
  as.character(x)
}
# bool_to_string(True) ➞ "True"
# 
# bool_to_string(False) ➞ "False"

bool_to_string(TRUE)



long_burp <- function(x){
  paste0("Bur",paste0(rep("r",x),collapse = ""),"p")
}

# long_burp(3) ➞ "Burrrp"
# 
# long_burp(5) ➞ "Burrrrrp"
# 
# long_burp(9) ➞ "Burrrrrrrrrp"

give_me_something <- function(x){
  paste("something",x,collapse = " ")
}


# give_me_something("is better than nothing") ➞ "something is better than nothing"
# 
# give_me_something("Bob Jane") ➞ "something Bob Jane"
# 
# give_me_something("something") ➞ "something something"


greeting <- function(x){
  if(x == "Mubashir") return("Hello, my Love!")
  paste("Hello,",paste0(x,"!"))
}

# greeting("Matt") ➞ "Hello, Matt!"
# 
# greeting("Helen") ➞ "Hello, Helen!"
# 
# greeting("Mubashir") ➞ "Hello, my Love!"


comp <- function(x,y){
  ifelse(nchar(x) == nchar(y),TRUE,FALSE)
}
# comp("AB", "CD") ➞ True
# 
# comp("ABC", "DE") ➞ False
# 
# comp("hello", "edabit") ➞ False


is_empty <- function(x){
  if(nchar(x) ==0 )return(TRUE)
  FALSE
}

# is_empty("") ➞ True
# 
# is_empty(" ") ➞ False
# 
# is_empty("a") ➞ False