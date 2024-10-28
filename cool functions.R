#coool stuff

split_string_equal_parts <- function(text, part_length) {
  # Initialize an empty vector to store the parts
  parts <- character(0)
  
  # Calculate the total number of parts
  n <- ceiling(nchar(text) / part_length)
  
  # Loop through each part and extract the substring
  for (i in 1:n) {
    start <- (i - 1) * part_length + 1
    end <- min(i * part_length, nchar(text))
    parts <- c(parts, substring(text, start, end))
  }
  
  return(parts)
}

# Example usage
print(split_string_equal_parts("abcdefghij", 3))
# Output: "abc" "def" "ghi" "j"

no_yelling <- function(x){
  regmatches(x, gregexpr("[[:punct:]]+$",x,perl = TRUE))[[1]] <-regmatches(x, gregexpr("[[:punct:]]$",x,perl = TRUE))[[1]]
x  
}

# no_yelling("What went wrong?????????") ➞ "What went wrong?"
# 
# no_yelling("Oh my goodness!!!") ➞ "Oh my goodness!"
# 
# no_yelling("I just!!! can!!! not!!! believe!!! it!!!") ➞ "I just!!! can!!! not!!! believe!!! it!"
# # Only change repeating punctuation at the end of the sentence.
# 
# no_yelling("Oh my goodness!") ➞ "Oh my goodness!"
# # Do not change sentences where there exists only one or zero exclamation marks/question marks.
# 
# no_yelling("I just cannot believe it.") ➞ "I just cannot believe it."

