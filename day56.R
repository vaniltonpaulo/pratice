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

is_valid_phone_number <- function(x){
  grepl("^\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}$",x,perl = TRUE)
  
}

# is_valid_phone_number("(123) 456-7890") ➞ True
# 
# is_valid_phone_number("1111)555 2345") ➞ False
# 
# is_valid_phone_number("098) 123 4567") ➞ False

club_entry <- function(x){
  codebook <- 1:26
  names(codebook) <- letters
  result<-strsplit(x,"")[[1]]
  item <-result[duplicated(result)]
  final<-codebook[which(names(codebook) == item)]*4
  unname(final)
}

# club_entry("hill") ➞ 48
# # 'l' is 12th in the alphabet
# # 12*4 = 48
# 
# club_entry("apple") ➞ 64
# 
# club_entry("bee") ➞ 20

