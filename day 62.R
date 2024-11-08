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



inverter <- function(x,y) {
  if (y == "P") {
    result<-strsplit(x," ")[[1]]
    result<-rev(result)
    
    regmatches(result[[1]],gregexpr("^[A-z]",result[[1]],perl = TRUE))[[1]] <- toupper(substr(result[[1]],1,1))
    
    return(paste(result[[1]],paste(vapply(result[2:length(result)],tolower,character(1)),collapse = " "),collapse = " "))
  } else if ( y == "W") {
    result <- strsplit(x," ")[[1]]
    
    first<-strsplit(result[[1]],"")[[1]]
    first<-rev(first)
    first[[1]] <- toupper(first[[1]])
    first.sample<-vapply(first[2:length(first)],tolower,character(1))
    result[[1]] <-paste0(first[[1]],paste(first.sample,collapse = ""),collapse = "")
    
    
    done<-vapply(result[2:length(result)],function(k){
      so <- strsplit(k,"")[[1]]
      so <- rev(so)
      paste0(so,collapse = "")
    },character(1))
    
    return(paste(result[[1]],paste(done,collapse = " "),collapse = ""))
    
  }
  
}




# inverter("This is Valhalla", "P") ➞ "Valhalla is this"
# 
# inverter("One fine day to start", "W") ➞ "Eno enif yad ot trats"
# 
# inverter("Division by powers of two", "P") ➞ "Two of powers by division"

inverter("Excellence is achievable", "W")

x<-"nice"