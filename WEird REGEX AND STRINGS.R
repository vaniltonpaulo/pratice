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