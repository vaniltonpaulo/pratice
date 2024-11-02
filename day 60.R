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


shhh <- function(x){
  if(x == "") return(paste0('"','"',", whispered Edabit.",collapse = ""))
  result <- strsplit(x," ")[[1]]
  paste0('"',paste(paste0(toupper(substr(result[[1]],1,1)),tolower(substr(result[[1]],2,nchar(result[[1]]))),collapse = ""),paste0(tolower(result[-1]),collapse = " "),collapse = " "),'"',", whispered Edabit.",collapse = "")
}
# shhh("HI THERE!") ➞ '"Hi there!", whispered Edabit.'
# 
# shhh("tHaT'S Pretty awesOme") ➞ '"That's pretty awesome", whispered Edabit.'
# 
# shhh("") ➞ '"", whispered Edabit.'



rever <- function(x){
  result <- strsplit(x,"")[[1]]
  final<-rev(result)
  
  block<-character(0)
  for (i in seq_along(final)) {
    if(final[[i]] %in% letters){
      block[[length(block) + 1]]  <-toupper(final[[i]])
    }else{
      block[[length(block) + 1]]  <-tolower(final[[i]])
    }
  }
  
  paste0(block,collapse = "")
}


# rever("Hello World") ➞ "DLROw OLLEh"
# 
# rever("ReVeRsE") ➞ "eSrEvEr"
# 
# rever("Radar") ➞ "RADAr"




# correct_signs("3 < 7 < 11") ➞ True
# 
# correct_signs("13 > 44 > 33 > 1") ➞ False
# 
# correct_signs("1 < 2 < 6 < 9 > 3") ➞ True


x<- "3 < 7 < 11"
eval(parse(text = x))


result<-regmatches(x,gregexpr("[0-9]+",x,perl = TRUE))[[1]]
result.1<-regmatches(x,gregexpr("[[:punct:]]+",x,perl = TRUE))[[1]]