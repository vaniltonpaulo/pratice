encrypt <- function(x){
  x<-paste0(rev(strsplit(x,"")[[1]]),collapse = "")
  x<-gsub("a","0",x)
  x<-gsub("e","1",x)
  x<-gsub("i","2",x)
  x<-gsub("o","2",x)
  x<-gsub("u","3",x)
  paste0(x,"aca",collapse = "")
}


# encrypt("banana") ➞ "0n0n0baca"
# 
# encrypt("karaca") ➞ "0c0r0kaca"
# 
# encrypt("burak") ➞ "k0r3baca"
# 
# encrypt("alpaca") ➞ "0c0pl0aca"

encrypt("hello")



 lst = c("bad cookie", "good cookie", "bad cookie", "good cookie", "good cookie")
# pattern = "yourregularexpressionhere"
# 
# len(re.findall(pattern, ", ".join(lst))) ➞ 2
 
sum(grepl("bad cookie",lst,perl = TRUE))




parse_code <- function(x){
  final<-c(regmatches(x,gregexec("(?<first_name>^[A-z]+)0+(?<last_name>[A-z]+)0+(?<id>[0-9]+)",x,perl = TRUE))[[1]])
  
  fin <- c("first_name"= final[2],"last_name"= final[3],"id" = final[4])
  fin
}



# parse_code("John000Doe000123") ➞ {
#   "first_name": "John",
#   "last_name": "Doe",
#   "id": "123"
# }
# 
# parse_code("michael0smith004331") ➞ {
#   "first_name": "michael",
#   "last_name": "smith",
#   "id": "4331"
# }
# 
# parse_code("Thomas00LEE0000043") ➞ {
#   "first_name": "Thomas",
#   "last_name": "LEE",
#   "id": "43"
# }
parse_code('a0b01')



does_rhyme <- function(x,y){
  
  
  x.vect<-strsplit(x,"\\s+")[[1]]
  m.1 <-regmatches(x.vect[length(x.vect)],gregexpr("[aeiou]",x.vect[length(x.vect)],perl = TRUE))[[1]]
  
  
  y.vect<-strsplit(y,"\\s+")[[1]]
  m.2 <- regmatches(y.vect[length(y.vect)],gregexpr("[aeiou]",y.vect[length(y.vect)],perl = TRUE))[[1]]
  
  all(m.1==m.2)
}




# 
# does_rhyme("Sam I am!", "Green eggs and ham.") ➞ True
# 
# does_rhyme("Sam I am!", "Green eggs and HAM.") ➞ True
# # Capitalization and punctuation should not matter.
# 
# does_rhyme("You are off to the races", "a splendid day.") ➞ False
# 
# does_rhyme("and frequently do?", "you gotta move.") ➞ False
does_rhyme('You are off to the races', 'a splendid day.')
does_rhyme('your elbow and chin!', 'how much can you win?')




txt = "123 Redding Dr. 1560 Knoxville Ave. 3030 Norwalk Dr. 5 South St."
# pattern = "yourregularexpressionhere"
# 
# (re.findall(pattern, txt)) ➞ ["123 Redding Dr.", "1560 Knoxville Ave.", "3030 Norwalk Dr.", "5 South St."]

m <-regmatches(txt,gregexpr("[0-9]+ *[A-z]+ *[A-z]+.",txt,perl = TRUE))[[1]]
m<-trimws(m)
m



is_valid_hex_code <- function(x){
  if(grepl("^#",x,perl = TRUE) == TRUE){
    k<-substr(x,2,nchar(x))
    if(nchar(k) == 6){
      m<-strsplit(k,"")[[1]]
      return(all(grepl("([0-9]|[a-f]|[A-F])", m, perl = TRUE)))
    }else return(FALSE)
  }else{
    return(FALSE)
  }
}

# is_valid_hex_code("#CD5C5C") ➞ True
# 
# is_valid_hex_code("#EAECEE") ➞ True
# 
# is_valid_hex_code("#eaecee") ➞ True
# 
# is_valid_hex_code("#CD5C58C") ➞ False
# # Length exceeds 6
# 
# is_valid_hex_code("#CD5C5Z") ➞ False
# # Not all alphabetic characters in A-F
# 
# is_valid_hex_code("#CD5C&C") ➞ False
# # Contains unacceptable character
# 
# is_valid_hex_code("CD5C5C") ➞ False
# # Missing #


x<- "#CD5C5C"

grepl("^#",x,perl = TRUE)

k<-substr(x,2,nchar(x))
nchar(k)

m<-strsplit(k,"")[[1]]
all(m %in% c(letters,LETTERS,0,1,2,3,4,5,6,7,8,9))

all(grepl("([0-9]|[a-f]|[A-F])", m, perl = TRUE))

setdiff('[A-F]',LETTERS)