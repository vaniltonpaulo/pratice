#sorting


findLargestNum <- function(x){
  sort(x)
  
}

# findLargestNum([4, 5, 1, 3]) ➞ 5
# 
# findLargestNum([300, 200, 600, 150]) ➞ 600
# 
# findLargestNum([1000, 1001, 857, 1]) ➞ 1001

x<- c(1000, 1001, 857, 1)


sort_nums_ascending <- function(x){
  sort(x,decreasing = FALSE)
}

# sort_nums_ascending([1, 2, 10, 50, 5]) ➞ [1, 2, 5, 10, 50]
# 
# sort_nums_ascending(c(80, 29, 4, -95, -24, 85)) ➞ [-95, -24, 4, 29, 80, 85]
# 
# sort_nums_ascending([]) ➞ []





longest_string <- function(str1,str2){
  y<-sort(strsplit(str1,"")[[1]])
  x<-sort(strsplit(str2,"")[[1]])
  final<-unique(sort(c(x,y)))
  paste0(final,collapse = "")
  
}

# 
# str1 = "mubashir"
# str2 = "edabit"
# 
# longest_string(str1, str2) ➞ "abdehimrstu"
# # Contains sorted and distinct letters of the given strings.
# 
# str1 = "abcdefghijklmnopqrstuvwxyz"
# str2 = "abcdefghijklmnopqrstuvwxyz"
# 
# longest_string(str1, str2) ➞ "abcdefghijklmnopqrstuvwxyz"
# # Contains sorted and distinct letters of the given strings.



age_difference <- function(x){
  k<-sort(x,decreasing = TRUE)[1:2]
  if((k[[1]] - k[[2]]) == 1) return(paste(k[[1]] - k[[2]],"year",sep = " "))
  if((k[[1]] - k[[2]]) > 1) return(paste(k[[1]] - k[[2]],"years",sep = " "))
  return("No age difference between spouses.")
}


# age_difference(c(29, 1, 6, 8, 28)) ➞ "1 year"
# 
# age_difference(c(43, 86, 49, 86)) ➞ "No age difference between spouses."
# 
# age_difference(c(2, 4, 6, 32, 27)) ➞ "5 years"



reverse <- function(x){
  paste0(rev(strsplit(x,"")[[1]]),collapse = "")
}

# reverse("Hello World") ➞ "dlroW olleH"
# 
# reverse("The quick brown fox.") ➞ ".xof nworb kciuq ehT"
# 
# reverse("Edabit is really helpful!") ➞ "!lufpleh yllaer si tibadE"

