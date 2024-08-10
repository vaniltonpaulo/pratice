#Medium
#REGEX



txt = " alice15@gmail.com "
pattern = "yourregularexpressionhere"

#re.findall(pattern, txt) ➞ ["@", "."]
regmatches(txt , gregexpr("[[:punct:]]",txt,perl = TRUE))[[1]]



txt = "242Edabit2345can3443be3254324addictive!"
pattern = "yourregularexpressionhere"

#" ".join(re.findall(pattern, txt)) ➞ "Edabit can be addictive!"

regmatches(txt , gregexpr("[0-9]",txt,perl = TRUE))[[1]]
trimws(gsub("[0-9]"," ",txt))



double_swap <- function(word,c1,c2){
  word <- gsub(c1,"\1",word)
  word <- gsub(c2,c1,word)
  word <- gsub("\1",c2,word)
 
  return(word)
}

# double_swap("aabbccc", "a", "b") ➞ "bbaaccc"
# 
# double_swap("random w#rds writt&n h&r&", "#", "&")
# ➞ "random w&rds writt#n h#r#"
# 
# double_swap("128 895 556 788 999", "8", "9")
# ➞ "129 985 556 799 888"





to_scottish_screaming <- function(x){
  toupper(gsub("[aeiou]","e",x))
  
}



# to_scottish_screaming("hello world") ➞ "HELLE WERLD"
# 
# to_scottish_screaming("Mr. Fox was very naughty") ➞ "MR. FEX WES VERY NEEGHTY"
# 
# to_scottish_screaming("Butterflies are beautiful!") ➞ "BETTERFLEES ERE BEEETEFEL!"




math_expr <- function(x){
  
  pattern <- "^[0-9] *[+\\-*/%] *[0-9]$"
  
  # Use grepl to check if the input matches the pattern
  return(grepl(pattern, x,perl = TRUE))
}


# math_expr("4 + 5") ➞ True
# 
# math_expr("4*6") ➞ True
# 
# math_expr("4*no") ➞ False
math_expr("nope")
math_expr("a - b")
math_expr("4 - 5")


x<-"4 + 5"

grepl("^[0-9] *[+\\-*/%] *[0-9]$",x,perl = TRUE)

