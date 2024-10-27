count_smileys <- function(x){
  sum(grepl("^[:;][-~]?[)D]$",x))
  
}


# count_smileys(c(":)", ";(", ";}", ":-D")) ➞ 2
# 
# count_smileys([";D", ":-(", ":-)", ";~)"]) ➞ 3
# 
# count_smileys(c(";]", ":[", ";*", ":$", ";-D")) ➞ 1

