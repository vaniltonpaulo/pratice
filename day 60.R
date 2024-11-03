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



correct_signs <- function(expr) {
  # Split the expression by spaces to separate numbers and operators
  parts <- strsplit(expr, " ")[[1]]
  
  # Loop through each operator and check the condition
  for (i in seq(2, length(parts) - 1, by = 2)) {
    left <- as.numeric(parts[i - 1])
    op <- parts[i]
    right <- as.numeric(parts[i + 1])
    
    # Check each comparison based on the operator
    if (op == "<" && !(left < right)) return(FALSE)
    if (op == ">" && !(left > right)) return(FALSE)
    if (op == "<=" && !(left <= right)) return(FALSE)
    if (op == ">=" && !(left >= right)) return(FALSE)
    if (op == "==" && !(left == right)) return(FALSE)
    if (op == "!=" && !(left != right)) return(FALSE)
  }
  
  # If all comparisons are correct, return TRUE
  return(TRUE)
}

# Examples
print(correct_signs("3 < 7 < 11"))        # ➞ TRUE
print(correct_signs("13 > 44 > 33 > 1"))  # ➞ FALSE
print(correct_signs("1 < 2 < 6 < 9 > 3")) # ➞ TRUE




count_characters <- function(x){
  sum(nchar(x))
}
# 
# count_characters(c(
#   "###",
#   "###",
#   "###"
# )) ➞ 9
# 
# count_characters([
#   "22222222",
#   "22222222",
# ]) ➞ 16
# 
# count_characters([
#   "------------------"
# ]) ➞ 18
# 
# count_characters([]) ➞ 0
# 
# count_characters(c("", "")) ➞ 0


area_of_country <- function(x,y){
  paste(x,"is",paste0(round(((y/148940000)*100),2),"%"),"of the total world's landmass",collapse = " ")
}


# area_of_country("Russia", 17098242) ➞ "Russia is 11.48% of the total world's landmass"
# 
# area_of_country("USA", 9372610), "USA is 6.29% of the total world's landmass"
# 
# area_of_country("Iran", 1648195) ➞ "Iran is 1.11% of the total world's landmass"



plus_sign <- function(x){
  result<-regmatches(x,gregexpr("(?<=\\+)[A-z](?=\\+)",x,perl = TRUE))[[1]]
  result.1<-sum(strsplit(x,"")[[1]] %in% letters)
  return(ifelse(length(result) == result.1,TRUE,FALSE))
}

# plus_sign("+f+d+c+#+f+") ➞ True
# 
# plus_sign("+d+=3=+s+") ➞ True
# 
# plus_sign("f++d+g+8+") ➞ False
# 
# plus_sign("+s+7+fg+r+8+") ➞ False


mark_maths <- function(x){
  result<-strsplit(x,"=")
  
  final<-vapply(result,function(k){
    eval(parse(text = k[1])) == k[2]
  },logical(1))
  
  paste0(round((sum(final) / length(x)) * 100,0),"%",collapse = "")
  
}

# mark_maths(c("2+2=4", "3+2=5", "10-3=3", "5+5=10")) ➞ "75%"
# 
# mark_maths(c("1-2=-2"))  "0%"
# 
# mark_maths(c("2+3=5", "4+4=9", "3-1=2")) ➞ "67%"




# is_icecream_sandwich("CDC") ➞ True
# 
# is_icecream_sandwich("AAABB") ➞ False
# 
# is_icecream_sandwich("AA") ➞ False


