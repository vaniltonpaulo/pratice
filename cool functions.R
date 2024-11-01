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




#To evalueate expresiions
eval(parse(text = "49/7*2-3"))




######################### How to use Filter
# Define a numeric vector
numbers <- 1:10

# Use Filter to keep only even numbers
even_numbers <- Filter(function(x) x %% 2 == 0, numbers)

print(even_numbers)  # Output: 2 4 6 8 10



my_list <- list(1, NULL, 3, NULL, 5)

# Use Filter to remove NULL elements
non_null_elements <- Filter(Negate(is.null), my_list)

print(non_null_elements)  # Output: 1 3 5



# Define a character vector
words <- c("apple", "kiwi", "banana", "fig", "grape")

# Filter words with more than 4 characters
long_words <- Filter(function(word) nchar(word) > 4, words)

print(long_words)  # Output: "apple" "banana" "grape"



# 
# Tips
# Filter is most commonly used with logical tests (TRUE/FALSE functions).
# If you need the opposite condition (filter out certain values), use Negate() to reverse the logical result.
# The Filter function is efficient for lists and vectors and keeps the code clean and concise when you need to filter items by condition.