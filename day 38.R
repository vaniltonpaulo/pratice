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



findLargestNums <- function(x){
  lapply(x,function(k){
    sort(unlist(k),decreasing = TRUE)[[1]]
  })
}


# findLargestNums(list(list(4, 2, 7, 1), list(20, 70, 40, 90), list(1, 2, 0))) ➞ [7, 90, 2]
# 
# findLargestNums(list(list(-34, -54, -74), list(-32, -2, -65), list(-54, 7, -43))) ➞ [-34, -2, 7]
# 
# findLargestNums([[0.4321, 0.7634, 0.652], [1.324, 9.32, 2.5423, 6.4314], [9, 3, 6, 3]]) ➞ [0.7634, 9.32, 9]




asc_des_none <- function(x,y){
  if(y =="Asc") return(sort(x))
  if(y =="Des") return(sort(x,decreasing = TRUE))
  return(x)
}


# asc_des_none(c(4, 3, 2, 1), "Asc" ) ➞ [1, 2, 3, 4]
# 
# asc_des_none(c(7, 8, 11, 66), "Des") ➞ [66, 11, 8, 7]
# 
# asc_des_none([1, 2, 3, 4], "None") ➞ [1, 2, 3, 4]


missing_num <- function(x){
  setdiff(1:10,x)
}



# missing_num(c(1, 2, 3, 4, 6, 7, 8, 9, 10)) ➞ 5
# 
# missing_num([7, 2, 3, 6, 5, 9, 1, 4, 8]) ➞ 10
# 
# missing_num([10, 5, 1, 2, 4, 6, 8, 3, 9]) ➞ 7





name_shuffle <- function(x){
  paste0(rev(strsplit(x,"\\s+")[[1]]),collapse = " ")
}
# name_shuffle("Donald Trump") ➞ "Trump Donald"
# 
# name_shuffle("Rosie O'Donnell") ➞ "O'Donnell Rosie"
# 
# name_shuffle("Seymour Butts") ➞ "Butts Seymour"


alphabet_soup <- function(x){
  x<-sort(strsplit(x,"")[[1]])
  paste0(x,collapse = "")
}


# 
# alphabet_soup("hello") ➞ "ehllo"
# 
# alphabet_soup("edabit") ➞ "abdeit"
# 
# alphabet_soup("hacker") ➞ "acehkr"
# 
# alphabet_soup("geek") ➞ "eegk"
# 
# alphabet_soup("javascript") ➞ "aacijprstv"



number_len_sort <- function(x){
  x[order(nchar(x))]
}


# number_len_sort(c(1, 54, 1, 2, 463, 2)) ➞ [1, 1, 2, 2, 54, 463]
# 
# number_len_sort(c(999, 421, 22, 990, 32)) ➞ [22, 32, 999, 421, 990]
# 
# number_len_sort(c(9, 8, 7, 6, 5, 4, 31, 2, 1, 3)) ➞ [9, 8, 7, 6, 5, 4, 2, 1, 3, 31]


