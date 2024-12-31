# correct_signs("3 < 7 < 11") ➞ True
# 
# correct_signs("13 > 44 > 33 > 1") ➞ False
# 
# correct_signs("1 < 2 < 6 < 9 > 3") ➞ True


correct_signs <- function(expression) {
  # Split the expression into individual components
  components <- unlist(strsplit(expression, " "))
  
  # Initialize a variable to track the result
  result <- TRUE
  
  # Evaluate each pair of components
  for (i in seq(2, length(components) - 1, by = 2)) {
    lhs <- as.numeric(components[i - 1])
    operator <- components[i]
    rhs <- as.numeric(components[i + 1])
    
    # Check the comparison based on the operator
    if (operator == "<") {
      result <- result && (lhs < rhs)
    } else if (operator == ">") {
      result <- result && (lhs > rhs)
    } else if (operator == "<=") {
      result <- result && (lhs <= rhs)
    } else if (operator == ">=") {
      result <- result && (lhs >= rhs)
    } else {
      stop("Unsupported operator!")
    }
    
    # Break early if any comparison fails
    if (!result) {
      break
    }
  }
  
  return(result)
}




remove_special_characters <- function(x) {
  result <- regmatches(x,gregexpr("[^\\.\\!@#\\$%\\^\\&\\*\\(\\)]",x,perl = TRUE))[[1]]
  paste0(result,collapse = "")
}
# remove_special_characters("The quick brown fox!") ➞ "The quick brown fox"
# 
# remove_special_characters("%fd76$fd(-)6GvKlO.") ➞ "fd76fd-6GvKlO"
# 
# remove_special_characters("D0n$c sed 0di0 du1") ➞ "D0nc sed 0di0 du1"





num_in_str <- function(x) {
  result <- grep("[0-9]+",x,value = TRUE,perl = TRUE)
  grep("[A-z]+",result,value = TRUE,perl = TRUE)  
}

# num_in_str(c("1a", "a", "2b", "b")) ➞ ["1a", "2b"]
# 
# num_in_str(c("abc", "abc10")) ➞ ["abc10"]
# 
# num_in_str(c("abc", "ab10c", "a10bc", "bcd")) ➞ ["ab10c", "a10bc"]
# 
# num_in_str(c("this is a test", "test1")) ➞ ["test1"]



absolute <- function(x) {
  #here if you dont assign it back to x, it converts back to the original x
  x<- gsub("^A(?=\\s)","An absolute",x,perl = TRUE)
  gsub("(?<=\\s)(a)(?=\\s)","an absolute",x,perl = TRUE)
}

# absolute("I am a champion!!!") ➞ "I am an absolute champion!!!"
# 
# absolute("Such an amazing bowler.") ➞ "Such an amazing bowler."
# 
# absolute("A man with no haters.") ➞ "An absolute man with no haters."



trouble <- function(x,y) {
  
  x <- as.character(x)
  x <- strsplit(x,"")[[1]]
  result.1<-names(table(x))[table(x) == 3]
  

  y <- as.character(y)
  y <- strsplit(y,"")[[1]]
  result.2<-names(table(y))[table(y) == 2]
  isTRUE(all.equal(result.1,result.2))
}



# Create a function that takes two integers and returns true if a digit repeats three times in a row at any place in num1 
# AND that same digit repeats two times in a row in num2.


trouble <- function(num1, num2) {
  # Convert numbers to strings
  num1 <- as.character(num1)
  num2 <- as.character(num2)
  
  # Iterate over each character in num2
  for (i in strsplit(num2, "")[[1]]) {
    # Check if the character appears twice in a row in num2 and three times in a row in num1
    if (grepl(paste0(i, i), num2) && grepl(paste0(i, i, i), num1)) {
      return(TRUE)
    }
  }
  
  # If no match is found, return FALSE
  return(FALSE)
}

# trouble(451999277, 41177722899) ➞ True
# 
# trouble(1222345, 12345) ➞ False
# 
# trouble(666789, 12345667) ➞ True
# 
# trouble(33789, 12345337) ➞ False









