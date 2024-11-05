change_string <- function(x){
  result.up<-regmatches(x,gregexpr("[A-Z]",x,perl = TRUE))[[1]]
  
  result <-character(0)
  for (i in seq_along(result.up)) {
    if(result.up[[1]] %in% LETTERS) {
      result[[length(result) + 1]] <- LETTERS[which(result.up[[i]] == LETTERS) +1]
    }
  }
  result
  
  regmatches(x,gregexpr("[A-Z]",x,perl = TRUE))[[1]] <- result
  regmatches(x,gregexpr("[a-z]",x,perl = TRUE))[[1]] <- toupper(regmatches(x,gregexpr("[a-z]",x,perl = TRUE))[[1]])
  
  
  paste0(rev(strsplit(x,"")[[1]]), collapse = "")
}



# change_string("ApPle") ➞ "ELQPB"
# 
# change_string("draGON") ➞ "OPHARD"
# 
# change_string("ZebrA") ➞ "BRBEA"
change_string("MElon")
change_string("sNaKe")

