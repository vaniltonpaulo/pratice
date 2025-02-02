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
