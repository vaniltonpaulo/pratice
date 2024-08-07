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


is_vowel_sandwich <- function(x){
  if(nchar(x) != 3)  return(FALSE)
  vowels <- c("a","e","i","o","u")
  consonants <- setdiff(letters,vowels)
  word.1<-substr(x,1,1)
  word.2<-substr(x,2,2)
  word.3<-substr(x,3,3)
  
  if(word.1 %in% consonants && word.2 %in% vowels &&  word.3 %in% consonants ) {
    return(TRUE)
    }
  else{
      return(FALSE)
    } 
}



# is_vowel_sandwich("cat") ➞ True
# 
# is_vowel_sandwich("ear") ➞ False
# 
# is_vowel_sandwich("bake") ➞ False
# 
# is_vowel_sandwich("try") ➞ False
is_vowel_sandwich("xap")



splittar <- function(x){
  y<-regmatches(x,gregexpr("[aeiou]",x,perl = TRUE))[[1]]
  y<- paste0(y,collapse = "")
  w<-gsub("[aeiou]","",x)
  paste0(y,w,collapse = "")
}


# splittar("abcde") ➞ "aebcd"
# 
# splittar("Hello!") ➞ "eoHll!"
# 
# splittar("What's the time?") ➞ "aeieWht's th tm?"




convert_binary <- function(x){
  y <-gsub("([a-m]|[A-M])","0",x)
  gsub("([n-z]|[N-Z])","1",y)
}


# convert_binary("house") ➞ "01110"
# 
# convert_binary("excLAIM") ➞ "0100000"
# 
# convert_binary("moon") ➞ "0111"
convert_binary("topsyTurvy")


count_adverbs <- function(x){
  x<-gsub("[[:punct:]]","",x)
  y <- strsplit(x,"\\s+")[[1]]
  sum(grepl("ly$",y,perl = TRUE))
  
}


# count_adverbs("She ran hurriedly towards the stadium.") ➞ 1
# 
# count_adverbs("She ate the lasagna heartily and noisily.") ➞ 2
# 
# count_adverbs("He hates potatoes.") ➞ 0
# 
# count_adverbs("He was happily, crazily, foolishly over the moon.") ➞ 3









zipcode <- function(x){
  if(nchar(x) !=5) return(FALSE)
  
  if(grepl("([^0-9]|\\s)",x,perl = TRUE) == FALSE) return( TRUE)
  
  return(FALSE)
}
zipcode("923444")
# "32554" ➞ True
# 
# "92 342" ➞ False
# # Invalid: contains a whitespace
# 
# "9@342" ➞ False
# # Invalid: contains a non-numeric character
# 
# "923444" ➞ False
# # Invalid: length is not 5



superheroes <- function(x){
  unlist(regmatches(x,gregexpr("[A-z]+[^(wo|Wo)]man$",x,perl = TRUE)))
}


# superheroes(c("Batman", "Superman", "Spider-man", "Hulk", "Wolverine", "Wonder-Woman"))
# ➞ ["Batman", "Spider-man", "Superman"]
# 
# superheroes(c("Catwoman", "Deadpool", "Dr.Strange", "Captain-America", "Aquaman", "Hawkeye"))
# ➞ ["Aquaman"]
# 
# superheroes(c("Wonder-Woman", "Catwoman", "Invisible-Woman"))
# ➞ []
superheroes(c("Batman", "Superman", "Spider-man", "Hulk", "Wolverine", "Deadpool", "Dr.Strange", "Captain-America", "Aquaman", "Hawkeye", "Iron-man", "Thor", "Black-Panther", "Iceman"))

x <- c("Batman", "Superman", "Spider-man", "Hulk", "Wolverine", "Wonder-Woman")


unlist(regmatches(x,gregexpr("[A-z]+[^(wo|Wo)]man$",x,perl = TRUE)))