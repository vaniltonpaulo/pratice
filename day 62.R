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



# add_str_nums("4", "5") ➞ "9"
# 
# add_str_nums("abcdefg", "3") ➞ "-1"
# 
# add_str_nums("1", "") ➞ "1"
# 
# add_str_nums("1874682736267235927359283579235789257", "32652983572985729") ➞ "1874682736267235927391936562808774986"

#TOO EASY EVEN THOUGH ITS LABELED AS HARD