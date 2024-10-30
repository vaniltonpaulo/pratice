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

repetition <- function(x,y){
  paste0(rep(x,y),collapse = "")
}


# repetition("ab", 3) ➞ "ababab"
# 
# repetition("kiwi", 1) ➞ "kiwi"
# 
# repetition("cherry", 2) ➞ "cherrycherry"


calculator <- function(x){
  eval(parse(text = x))
}

# calculator("23+4") ➞ 27
# 
# calculator("45-15") ➞ 30
# 
# calculator("13+2-5*2") ➞ 5
# 
# calculator("49/7*2-3") ➞ 11


first_last <- function(x){
  result <-strsplit(x,"")[[1]]
  paste0(result[[1]],result[[length(result)]],collapse = "")
}
# first_last("ganesh") ➞ "gh"
# 
# first_last("kali") ➞ "ki"
# 
# first_last("shiva") ➞ "sa"
# 
# first_last("vishnu") ➞ "vu"
# 
# first_last("durga") ➞ "da"

