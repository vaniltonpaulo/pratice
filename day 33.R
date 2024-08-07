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

