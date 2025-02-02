# expect_equal()	Checks if two objects are exactly equal.
# expect_identical()	Checks if two objects are identical.
# expect_true()	Checks if a condition is TRUE.
# expect_false()	Checks if a condition is FALSE.
# expect_error()	Checks if a function throws an error.
# expect_warning()	Checks for warnings.
# expect_message()	Checks for messages.
# expect_output()	Checks the output of a function.


# 2. Writing Tests
# A test is typically defined using test_that(). Here's the basic syntax:

test_that("description of test", {
  # Test code goes here
  expect_equal(1 + 1, 2)
})



# Example project structure:
# 
# 
# my_project/
# ├── R/
# │   └── my_functions.R
# ├── tests/
# │   └── testthat/
# │       ├── test-my_functions.R
# │       └── test-other_functions.R
# └── DESCRIPTION




# Example: Writing and Running Tests
# Let's say you have a function called add_numbers():


add_numbers <- function(a, b) {
  a + b
}

#Create a test file test-add_numbers.R in the tests/testthat/ directory:


test_that("add_numbers works correctly", {
  expect_equal(add_numbers(2, 3), 5)
  expect_equal(add_numbers(-1, 1), 0)
})

library(testthat)
test_dir("tests/testthat")


square <- function(x) {
  x * x
}

Test (test-square.R):

test_that("square works correctly", {
  expect_equal(square(2), 4)
  expect_equal(square(-3), 9)
  expect_equal(square(0), 0)
})


#Run the Tests:
  
library(testthat)
test_dir("tests/testthat")



# Key Functions
# test_that(): Defines a test case.
# expect_*(): Various functions to check expectations.
# test_dir(): Runs all tests in a specified directory.
# test_file(): Runs a specific test file.
# test_check(): Runs tests during package development (for CRAN checks).


#######################################################. COMPLETE EXAMPLE #####################################


#Example Function File (math_utils.R)



add_numbers <- function(a, b) {
  a + b
}

divide_numbers <- function(a, b) {
  if (b == 0) stop("Division by zero is not allowed")
  a / b
}

print_message <- function(msg) {
  message("Message: ", msg)
}

generate_warning <- function() {
  warning("This is a warning!")
}

square <- function(x) {
  x * x
}




# tests/testthat/test-math_utils.R
test_that("math_utils functions behave as expected", {
  
  # 1. expect_equal() - Check numerical equality
  expect_equal(add_numbers(2, 3), 5)             # 2 + 3 = 5
  expect_equal(square(-3), 9)                    # (-3)^2 = 9
  
  # 2. expect_identical() - Check strict identity (including data type)
  expect_identical(add_numbers(1, 1), 2L)        # Identical as integer
  expect_identical(TRUE, !FALSE)                 # Identical TRUE values
  
  # 3. expect_true() - Check if a condition is TRUE
  expect_true(add_numbers(10, 5) == 15)          # 10 + 5 = 15
  
  # 4. expect_false() - Check if a condition is FALSE
  expect_false(add_numbers(2, 2) == 5)           # 2 + 2 is NOT equal to 5
  
  # 5. expect_error() - Expect an error when dividing by zero
  expect_error(divide_numbers(10, 0), "Division by zero is not allowed")
  
  # 6. expect_warning() - Expect a warning to be thrown
  expect_warning(generate_warning(), "This is a warning!")
  
  # 7. expect_message() - Expect a message to be printed
  expect_message(print_message("Hello"), "Message: Hello")
  
  # 8. expect_output() - Check printed output
  expect_output(print(letters[1:5]), "[1] \"a\" \"b\" \"c\" \"d\" \"e\"")
})