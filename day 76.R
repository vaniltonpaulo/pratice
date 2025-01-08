# int_to_str(4) ➞ "4"
# 
# str_to_int("4") ➞ 4
# 
# int_to_str(29348) ➞ "29348"



valid <- function(x) {
  
  if(grepl("[A-z]",x,perl = TRUE) == TRUE) return(FALSE)
  if(grepl("\\s+",x,perl = TRUE) == TRUE) return(FALSE)
  if(nchar(x) == 4 || nchar(x) == 6) return(TRUE)
  
  FALSE   
}
# valid("1234") ➞ True
# 
# valid("45135") ➞ False
# 
# valid("89abc1") ➞ False
# 
# valid("900876") ➞ True
# 
# valid(" 4983") ➞ False


normalize <- function(x) {
  so<- gsub(" ","",x)
  result <- regmatches(x,gregexpr("[A-Z]",x,perl = TRUE))[[1]]
  xo<-tolower(x)
  if(nchar(so) ==length(result)) return(paste0(toupper(substr(xo,1,1)),substr(xo,2,nchar(xo)),collapse = ""))
  x
}
# normalize("CAPS LOCK DAY IS OVER") ➞ "Caps lock day is over!"
# 
# normalize("Today is not caps lock day.") ➞ "Today is not caps lock day."
# 
# normalize("Let us stay calm, no need to panic.") ➞ "Let us stay calm, no need to panic."



stupid_addition <- function(x) {
  if(all(unlist(lapply(x,is.numeric)))) return(paste0(x,collapse = ""))
  if(all(unlist(lapply(x,is.character)))) return(sum(as.numeric(x)))
  return("NONE")
}

# stupid_addition(c(1, 2)) ➞ "12"
# 
# stupid_addition(c("1", "2")) ➞ 3
# 
# stupid_addition(c("1", 2)) ➞ None


reverse_words <- function(x) {
  result <- strsplit(x,"\\s+")[[1]]
  paste(rev(result),collapse = " ")
}
# reverse_words("the sky is blue") ➞ "blue is sky the"
# 
# reverse_words("  hello world!  ") ➞ "world! hello"
# 
# reverse_words("a good   example") ➞ "example good a"

shared_letters <- function(x) {
  y <- strsplit(x[[1]],"")[[1]]
  z <- strsplit(x[[2]],"")[[1]]
  length(unique(intersect(y,z)))
}
# shared_letters(c("apple", "meaty")) ➞ 2
# # Since "ea" is shared between "apple" and "meaty".
# 
# shared_letters(c("fan", "forsook")) ➞ 1
# 
# shared_letters(c("spout", "shout")) ➞ 4




histogram <- function(x,y) {
  result <- character(0)
  for (i in seq_len(length(x))) {
    result[[length(result) + 1]] <- paste0(rep(y,x[[i]]),collapse = "")
  }
  result
  
  cat(result, sep = "\n")
}
# histogram(c(1, 3, 4), "#") ➞ "#\n###\n####"
# 
# #
# ###
# ####
# 
# histogram(c(6, 2, 15, 3), "=") ➞ "======\n==\n===============\n==="
# 
# ======
#   ==
#   ===============
#   ===
#   
#   histogram(c(1, 10), "+") ➞ "+\n++++++++++"
# 
# +
#   ++++++++++

