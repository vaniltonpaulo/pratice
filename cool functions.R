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



######## TRYCATCH

# In R, tryCatch is used for error handling, allowing you to handle specific errors or conditions gracefully without stopping the execution of the entire script. The tryCatch function lets you define custom handling for different types of conditions: errors, warnings, messages, and finally blocks.
# 
# Here’s an example to demonstrate the basic structure of tryCatch:
  

result <- tryCatch({
  # Code that might produce an error
  x <- 10 / 0  # This will produce an error: division by zero
  x
}, 
error = function(e) {
  # Code to handle the error
  message("An error occurred: ", e$message)
  NA  # Return a default value in case of error
},
warning = function(w) {
  # Code to handle a warning
  message("A warning was raised: ", w$message)
  invokeRestart("muffleWarning")  # Prevent the warning from being printed
},
finally = {
  # Code that will always run, regardless of success or error
  message("Finished trying to compute result")
})

print(result)
# Explanation of each component:
#   Code Block: The code inside the first {} block is the primary code to execute. If this block runs without any errors, tryCatch simply returns the result.
# 
# Error Handling (error argument): If an error occurs (e.g., division by zero), the error block will execute, allowing you to handle it (e.g., print a message and return a default value).
# 
# Warning Handling (warning argument): Similarly, you can handle warnings. invokeRestart("muffleWarning") suppresses the warning if needed.
# 
# Finally Block (finally argument): This block executes regardless of whether an error occurred, which is useful for cleanup tasks.
# 
# Notes:
#   tryCatch provides flexibility in handling different conditions.
# For more complex workflows, consider using nested tryCatch blocks or custom error handlers, depending on the error conditions.




# Write a function that measures how long another function was running. The
# function should return the runtime of the given function, in seconds, rounded
# to the nearest integer second.
#
# The function being timed may sometimes throw an error, in which case the
# return value should depend on the `impute.inf`-argument: If it is `TRUE`, a
# value of `Inf` should be returned when an error was thrown, instead of the
# actual runtime. This would indicate that the function never actually finished.
#
# This function gets three arguments:
# - `fn` (function) the function to measure. It takes no arguments and should
#   be called as `fn()`.
# - `impute.inf` (`logical(1)`) whether to return `Inf`, instead of the runtime,
#   when `fn` throws an error.
#
# Functions that may be useful here are the `system.time()` function, or the
# `proc.time()` function. Make sure to use the `elapsed` part of the times that
# they report.
#
# Example functions to try out:
sleep1 <- function() Sys.sleep(1)
sleep2 <- function() Sys.sleep(2)
sleepErr <- function() {
  Sys.sleep(1)
  stop("error :-(")
}
# Example calls:
# > ex01TimeFun(sleep1, impute.inf = FALSE)  # 1
# > ex01TimeFun(sleep1, impute.inf = TRUE)  # 1
# > ex01TimeFun(sleep2, impute.inf = FALSE)  # 2
# > ex01TimeFun(sleepErr, impute.inf = FALSE)  # 1
# > ex01TimeFun(sleepErr, impute.inf = TRUE)  # Inf
ex01TimeFun <- function(fn,impute.inf) {
  # your code
  assertFunction(fn)
  assertFlag(impute.inf)
  
  runtime <- system.time({
    result <- tryCatch({
      fn()
    },error = function(e){
      FALSE
    })
  })
  
  if(isTRUE(impute.inf)  && isFALSE(result)){
    return(Inf)
  } 
  return(round(runtime[["elapsed"]],0))
}






correct_signs <- function(expr) {
  # Split the expression by spaces to separate numbers and operators
  parts <- strsplit(expr, " ")[[1]]
  
  # Loop through each operator and check the condition
  for (i in seq(2, length(parts) - 1, by = 2)) {
    left <- as.numeric(parts[i - 1])
    op <- parts[i]
    right <- as.numeric(parts[i + 1])
    
    # Check each comparison based on the operator
    if (op == "<" && !(left < right)) return(FALSE)
    if (op == ">" && !(left > right)) return(FALSE)
    if (op == "<=" && !(left <= right)) return(FALSE)
    if (op == ">=" && !(left >= right)) return(FALSE)
    if (op == "==" && !(left == right)) return(FALSE)
    if (op == "!=" && !(left != right)) return(FALSE)
  }
  
  # If all comparisons are correct, return TRUE
  return(TRUE)
}

# Examples
print(correct_signs("3 < 7 < 11"))        # ➞ TRUE
print(correct_signs("13 > 44 > 33 > 1"))  # ➞ FALSE
print(correct_signs("1 < 2 < 6 < 9 > 3")) # ➞ TRUE


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