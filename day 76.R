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

