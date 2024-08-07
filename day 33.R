#easy regex




txt1 = "red flag blue flag"
txt2 = "yellow flag red flag blue flag green flag"
txt3 = "pink flag red flag black flag blue flag green flag red flag"
pattern = "yourregularexpressionhere"
txt4 = 'blue flag red flag red flag blue flag green flag red flag'

# re.findall(pattern, txt1) ➞ ["red flag", "blue flag"]
# re.findall(pattern, txt2) ➞ ["red flag", "blue flag"]
# re.findall(pattern, txt3) ➞ ["red flag", "blue flag", "red flag"]

regmatches(txt1,gregexpr("(red flag|blue flag)",txt1,perl = TRUE))
regmatches(txt2,gregexpr("(red flag|blue flag)",txt2,perl = TRUE))
regmatches(txt3,gregexpr("(red flag|blue flag)",txt3,perl = TRUE))
regmatches(txt4,gregexpr("(red flag|blue flag)",txt4,perl = TRUE))[[1]]



count_vowels <- function(x){
  x<-strsplit(x,"")[[1]]
  sum(grepl("[aeiou]",x,perl = TRUE))
}
# 
# count_vowels("Celebration") ➞ 5
# 
# count_vowels("Palm") ➞ 1
# 
# count_vowels("Prediction") ➞ 4
count_vowels("Convention")


correct_signs <- function(expression) {
  # Use tryCatch to safely evaluate the expression and handle potential errors
  result <- tryCatch({
    # Parse and evaluate the expression
    eval(parse(text = expression))
  }, error = function(e) {
    # Return FALSE if there's an error in evaluating the expression
    FALSE
  })
  # Ensure the result is a logical value
  return(isTRUE(result))
}



# correct_signs("3 < 7 < 11") ➞ True
# 
# correct_signs("13 > 44 > 33 > 1") ➞ False
# 
# correct_signs("1 < 2 < 6 < 9 > 3") ➞ True


x<- "3 < 7 < 11"

nums<- regmatches(x,gregexpr("[[:digit:]]+",x,perl = TRUE))[[1]]
signs <-regmatches(x,gregexpr("[[:punct:]]+",x,perl = TRUE))[[1]]
nums<-as.numeric(nums)
nums[[1]] < nums[[2]]

eval(parse(text = "3 < 7"))




replace_vowels <- function(x,y){
  gsub("[aeiou]",y,x,perl = TRUE)
}


# 
# replace_vowels("the aardvark", "#") ➞ "th# ##rdv#rk"
# 
# replace_vowels("minnie mouse", "?") ➞ "m?nn?? m??s?"
# 
# replace_vowels("shakespeare", "*") ➞ "sh*k*sp**r*"


XO <- function(x){
  if(!grepl("(x|o)",x,perl = TRUE) == TRUE) return(TRUE)
  
  x <- strsplit(x,"")[[1]]
  x.amount <- sum(grepl("(x|X)",x,perl = TRUE))
  o.amount <- sum(grepl("(o|O)",x,perl = TRUE))
  if(x.amount == o.amount){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

# 
# XO("ooxx") ➞ True
# 
# XO("xooxx") ➞ False
# 
# XO("ooxXm") ➞ True
# # Case insensitive.
# 
# XO("zpzpzpp") ➞ True
# # Returns True if no x and o.
# 
# XO("zzoo") ➞ False
XO("o")
XO("")




is_valid_PIN <- function(x){
  y<-strsplit(x,"")[[1]]
  values <-grepl("[0-9]",y,perl = TRUE)
  if((sum(values) == length(values)) == TRUE) {
    if(length(y) == 4 || length(y) == 6 ) return(TRUE)
    
  }
  return(FALSE) 
    
  
  
}

# is_valid_PIN("1234") ➞ True
# 
# is_valid_PIN("12345") ➞ False
# 
# is_valid_PIN("a234") ➞ False
# 
# is_valid_PIN("") ➞ False
is_valid_PIN("@234")
is_valid_PIN("123456")





remove_vowels <- function(x){
  gsub("[aeiou]","",x,perl = TRUE)
}

# remove_vowels("I have never seen a thin person drinking Diet Coke.")
# ➞ " hv nvr sn  thn prsn drnkng Dt Ck."
# 
# remove_vowels("We're gonna build a wall!")
# ➞ "W'r gnn bld  wll!"
# 
# remove_vowels("Happy Thanksgiving to all--even the haters and losers!")
# ➞ "Hppy Thnksgvng t ll--vn th htrs nd lsrs!"


cap_to_front <- function(x){
  ups<-regmatches(x,gregexpr("[[:upper:]]",x,perl = TRUE))[[1]]
  lows<-regmatches(x,gregexpr("[a-z]",x,perl = TRUE))[[1]]
  
  return(paste0(paste0(ups,collapse = ""),paste0(lows,collapse = ""),collapse = ""))
}



# cap_to_front("hApPy") ➞ "APhpy"
# 
# cap_to_front("moveMENT") ➞ "MENTmove"
# 
# cap_to_front("shOrtCAKE") ➞ "OCAKEshrt"


letters_only <- function(x){
  gsub("([[:punct:]]|\\d)","",x, perl = TRUE)
}


# letters_only("R!=:~0o0./c&}9k`60=y") ➞ "Rocky"
# 
# letters_only("^,]%4B|@56a![0{2m>b1&4i4") ➞ "Bambi"
# 
# letters_only("^U)6$22>8p).") ➞ "Up"

letters_only('97H^)~a8567ll*o?"6%)w63e37e<n?@=')