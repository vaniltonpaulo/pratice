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



vowels <- function(word) {
  # Convert to lowercase and remove non-alphabet characters
  word <- tolower(gsub("[^a-z]", "", word))
  
  # Count the number of vowels
  sum(str_count(word, "[aeiou]"))
}


vowels <- function(x){
  y<-gsub("[^A-z]", "", x)
  y <- tolower(y)
  so<-regmatches(y,gregexpr("[aeiou]",y,perl = TRUE))[[1]]
  sum(nchar(so))
}

consonants <- function(x){
  y<-gsub("[^A-z]", "", x)
  y <- tolower(y)
  so<-regmatches(y,gregexpr("[bcdfghjklmnpqrstvwxyz]",y,perl = TRUE))[[1]]
  sum(nchar(so))
}


# vowels('Jameel SAEB') ➞ 5
# 
# 
# consonants("Smithsonian") ➞ 7
# vowels("Smithsonian") ➞ 4
consonants('G^oOd B&y')







txt = "best buy best car best friend best-boy bestguest best dressed best bet best man best deal best boyfriend"
pattern = "yourregularexpressionhere"

#re.findall(pattern, txt) ➞ ["best buy", "best bet", "best boyfriend"]

regmatches(txt,gregexpr("best *b[a-z]*",txt,perl = TRUE))[[1]]











findall <- function(x){
  regmatches(x,gregexpr(" *x([0-9]|([a-f]|[A-F]))+",x,perl = TRUE))[[1]][[2]]
  
}

txt1 = "Exception 0xAF"
txt2 = "Exception 0xD3"
txt3 = "Exception 0xd3"
txt4 = "Exception 0xZZ"
pattern = "yourregularexpressionhere"
# 
# re.findall(txt1) ➞ ["xAF"]
# re.findall( txt2) ➞ ["xD3"]
# re.findall( txt3) ➞ []
# re.findall( txt4) ➞ []


regmatches(txt1,gregexpr(" *x([0-9]|([a-f]|[A-F]))+",txt1,perl = TRUE))[[1]][[2]]

regmatches(txt4,gregexpr(" *x([0-9]|([a-f]|[A-F]))+",txt4,perl = TRUE))[[1]][[2]]

regmatches(txt4,gregexpr(" *x([0-9]|([a-f]|[A-F]))+",txt4,perl = TRUE))[[1]][[2]]

