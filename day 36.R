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
  final<-c(regmatches(x,gregexec("(?<first_name>^[A-z][a-z]+)0+(?<last_name>[A-z][A-z]+)0+(?<id>[0-9]+)",x,perl = TRUE))[[1]])
  
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


