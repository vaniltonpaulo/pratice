check_title <- function(x){
  result <- strsplit(x," ")[[1]]
  
  final <- vapply(result,function(k){
    substr(k,1,1)
  },character(1))
  
  isTRUE(all(final %in% LETTERS))
}

# check_title("A Mind Boggling Achievement") ➞ True
# 
# check_title("A Simple Python Program!") ➞ True
# 
# check_title("Water is transparent") ➞ False
check_title("I want To eat bread")

check_title("A Simple Java Script Program!")

googlify <- function(x){
  
  paste0("G",paste0(rep("o",x),collapse = ""),"gle",collapse = "")
}

# googlify(10) ➞ "Goooooooooogle"
# 
# googlify(23) ➞ "Gooooooooooooooooooooooogle"
# 
# googlify(2) ➞ "Google"
# 
# googlify(-2) ➞ "invalid"
