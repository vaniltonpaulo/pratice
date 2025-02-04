#coool stuff

#The readline() function in R is used for interactive input from the user via the command line. Here's how it works:
# How It Works
# Displays a Prompt: If you specify a prompt, it is shown to the user.
# Waits for Input: The function pauses execution and waits for the user to enter a response.
# Captures Input: When the user presses Enter/Return, the input is returned as a character string.

name <- readline("Enter your name: ")
cat("Hello,", name, "!\n")

# check the speed pf both functions
dropMissingCols <- function(df) {
  drop <- character(0)
  for (col in colnames(df)) {
    for (row in seq_len(nrow(df))) {
      if (is.na(df[row, col])) drop <- union(drop, col)
    }
  }
  df[, setdiff(colnames(df), drop), drop = FALSE]
}

dropMissingCols.1 <- function(df) {
  result<-vapply(colnames(df),function(k){
    if(any(is.na(df[,k])) == FALSE){
      return(k)
    }else{
      "pdf"
    }
  },character(1))
  
  final <-result[result !="pdf"]
  df[,final,drop = FALSE]
}

library(microbenchmark)

microbenchmark(
  dropMissingCols(testdf),
  dropMissingCols.1(testdf),
  times = 100  # Number of iterations
)




# Function to check if a number is prime
is_prime <- function(n) {
  if(n == 2) return(TRUE)
  if (n <= 1) {
    return(FALSE)  # Numbers <= 1 are not prime
  }
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) {
      return(FALSE)  # Divisible by a number other than 1 and itself
    }
  }
  return(TRUE)  # Number is prime
}

# Example usage
is_prime(7)  # Returns: TRUE
is_prime(10) # Returns: FALSE

# HOW TO USE THE OPTIONAL '?' IN REGEX AND AND HOW TO GET DECIMAL NUMBERS
regmatches(result, gregexpr("-?\\d+(\\.\\d+)?", result, perl = TRUE))

#THis IS ALL YOU VERIFY IF TWO THINGS OR MORE ARE EQUAL
isTRUE(all.equal(firsthalf(result), secondHalf(result)))

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








histogram <- function(x,y) {
  result <- character(0)
  for (i in seq_len(length(x))) {
    result[[length(result) + 1]] <- paste0(rep(y,x[[i]]),collapse = "")
  }
  result
  
  cat(result, sep = "\n")
}
# histogram(c(1, 3, 4), "#") ➞ "#\n###\n####"
# 
# #
# ###
# ####
# 
# histogram(c(6, 2, 15, 3), "=") ➞ "======\n==\n===============\n==="
# 
# ======
#   ==
#   ===============
#   ===
#   
#   histogram(c(1, 10), "+") ➞ "+\n++++++++++"
# 
# +
#   ++++++++++







wave <- function(x) {
  result <- strsplit(x,"")[[1]]
  n <- 1
  final <- character(0)
  
  repeat{
    
    if(n <= nchar(x)) {
      result[[n]] <- toupper(result[[n]])
      final[[length(final) + 1]] <- paste0(result,collapse = "")
      result <- tolower(result)
      n <- n + 1
      
    }else {
      break
    }
    
    
  }
  
  final
  
}

# wave("edabit") ➞ ["Edabit", "eDabit", "edAbit", "edaBit", "edabIt", "edabiT"]
# 
# wave("just do it") ➞ ["Just do it", "jUst do it", "juSt do it", "jusT do it", "just Do it", "just dO it", "just do It", "just do iT"]
# 
# wave(" ") ➞ []



num_of_leapyears <- function(x) {
  result <- strsplit(x,"\\-")[[1]]
  result <-seq(result[[1]],result[[2]])
  isleap <- lapply(result, function(k){
    if( k %% 4 ==0 & (k %% 100 !=0  || k %% 400 == 0)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  length(result[unlist(isleap)])
}


# num_of_leapyears("1980-1984") ➞ 2
# # 1980 and 1984 are leapyears.
# 
# num_of_leapyears("2000-2020") ➞ 6
# 
# num_of_leapyears("1600-2000") ➞ 98



first_before_second <-  function(string, first, second) {
  # Get the positions of the letters in the string
  first_positions <- gregexpr(first, string)[[1]]
  second_positions <- gregexpr(second, string)[[1]]
  
  # If either letter is not found, return TRUE since the condition is trivially satisfied
  if (first_positions[1] == -1 || second_positions[1] == -1) {
    return(TRUE)
  }
  
  # Check if the maximum position of 'first' is less than the minimum position of 'second'
  max(first_positions) < min(second_positions)
}

can_build <-  function(x,y) {
  x <- gsub(" ","",x)
  y <- gsub(" ","",y)
  
  x.result <- strsplit(x,"")[[1]]
  so <- table(x.result)
  
  y.result <- strsplit(y,"")[[1]]
  ko <- table(y.result)
  
  for (i in names(so)) {
    if(so[i] > ko[i]) {
      return(FALSE)
    }
    
  }
  return(TRUE)
}


# can_build("got 2 go", "gogogo 2 today") ➞ True
# 
# can_build("sit on top", "its a moo point") ➞ True
# 
# can_build("long high add or", "highway road go long") ➞ False
# 
# can_build("fill tuck mid", "truck falls dim") ➞ False



######################################################### Matrix

isAdjacent <-  function(df,node1, node2) {
  ifelse(df[node1, node2] == 1, TRUE, FALSE)
}




df <- matrix(c(
  0, 1, 0, 0,
  1, 0, 1, 1,
  0, 1, 0, 1,
  0, 1, 1, 0
), nrow = 4, byrow = TRUE)

isAdjacent(df, 1,2)
isAdjacent(df, 1,3)


transposeMatrix <-  function(x) {
  result <- t(x)
  final <- apply(result,1 ,paste0, collapse = " ")
  paste(final, collapse = " ")
}

transposeMatrix(matrix(c("Enter", "the", "Matrix!"), nrow = 3, byrow = FALSE))
# Output: "Enter the Matrix!"

transposeMatrix(matrix(c("The", "columns", "are", "rows."), nrow = 2, byrow = FALSE))
# Output: "The columns are rows."

transposeMatrix(matrix(c("You", "must", "transpose", "the", "table", "order."), nrow = 3, byrow = FALSE))
# Output: "You must transpose the table order


makeTranspose <-  function(x) {
  # always start with a empty matrix
  result <- matrix(0, ncol = nrow(x), nrow = ncol(x))
  
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      
      result[j,i] <-  x[i,j]
      
    }
    
  }
  result
  
}
# Test cases
print(makeTranspose(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, byrow = TRUE)))
# Output:
#      [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9

print(makeTranspose(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)))
# Output:
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# [3,]    5    6

print(makeTranspose(matrix(c("a", "b"), nrow = 1, byrow = TRUE)))
# Output:
#      [,1]
# [1,] "a" 
# [2,] "b"



orderedMatrix <-  function(x, y) {
  matrix(1:(x * y), nrow = x, ncol = y)
  
}
# orderedMatrix(5, 5) ➞  matrix(c(
#   1, 2, 3, 4, 5,
#   6, 7, 8, 9, 10,
#   11, 12, 13, 14, 15,
#   16, 17, 18, 19, 20,
#   21, 22, 23, 24, 25
# ), nrow = 5, byrow = TRUE)


orderedMatrix(1, 5) 



spiralMatrix <- function(n) {
  # Create an empty n x n matrix
  matrix_result <- matrix(0, nrow = n, ncol = n)
  
  # Initialize directions (right, down, left, up)
  directions <- list(c(0, 1), c(1, 0), c(0, -1), c(-1, 0))
  current_direction <- 1
  
  # Starting point
  row <- 1
  col <- 1
  
  for (value in 1:(n * n)) {
    # Fill the current cell
    matrix_result[row, col] <- value
    
    # Compute the next position
    next_row <- row + directions[[current_direction]][1]
    next_col <- col + directions[[current_direction]][2]
    
    # Check if the next position is valid
    if (next_row < 1 || next_row > n || next_col < 1 || next_col > n || matrix_result[next_row, next_col] != 0) {
      # Change direction clockwise
      current_direction <- (current_direction %% 4) + 1
      next_row <- row + directions[[current_direction]][1]
      next_col <- col + directions[[current_direction]][2]
    }
    
    # Move to the next position
    row <- next_row
    col <- next_col
  }
  
  return(matrix_result)
}

# Test cases
print(spiralMatrix(2))
# Output:
#      [,1] [,2]
# [1,]    1    2
# [2,]    4    3

print(spiralMatrix(3))
# Output:
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    8    9    4
# [3,]    7    6    5

print(spiralMatrix(4))
# Output:
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    3    4
# [2,]   12   13   14    5
# [3,]   11   16   15    6
# [4,]   10    9    8    7




