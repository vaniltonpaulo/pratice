validate_spelling <- function(x){
  result <-regmatches(x,gregexpr("[A-Z]+\\.",x,perl = TRUE))[[1]]
  result.1<-paste0(result,collapse = "")
  result.2<-gsub("\\.","",result.1)
  
  final<-regmatches(x,gregexpr("[A-Z][a-z]+",x,perl = TRUE))[[1]]
  
  isTRUE(tolower(result.2) == tolower(final))
}



# validate_spelling("C. Y. T. O. P. L. A. S. M. Cytoplasm?") ➞ True
# 
# validate_spelling("P. H. A. R. A. O. H. Pharaoh!") ➞ True
# 
# validate_spelling("H. A. N. K. E. R. C. H. E. I. F. Handkerchief.") ➞ False

validate_spelling("O. C. C. U. R. R. E. N. C. E. Occurrence?")



dashed<-function(x){
  gsub("([aeiouAEIOU])","-\\1-",x)
  
}

# dashed("Edabit") ➞ "-E-d-a-b-i-t"
# 
# dashed("Carpe Diem") ➞ "C-a-rp-e- D-i--e-m"
# 
# dashed("Fight for your right to party!") ➞ "F-i-ght f-o-r y-o--u-r r-i-ght t-o- p-a-rty!"



