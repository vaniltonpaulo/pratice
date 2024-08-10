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

