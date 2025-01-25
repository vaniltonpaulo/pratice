#########Example: A Complete S3 Implementation
#Define a Class and Its Methods

# Create an S3 object
dog <- list(name = "Buddy", breed = "Golden Retriever", age = 5)
class(dog) <- "Dog"

# Generic function
describe <- function(x) {
  UseMethod("describe")
}

# Method for Dog class
describe.Dog <- function(x) {
  paste(x$name, "is a", x$age, "year old", x$breed, ".")
}

# Default method
describe.default <- function(x) {
  "I don't know how to describe this object."
}

# Test the implementation
describe(dog)          # Calls describe.Dog
describe("unknown")    # Calls describe.default





############################################################ constructor
############In this example, we'll create a class called Creature. 
###A constructor function ensures the creation of valid objects of the Creature class.


# Load the 'checkmate' package for assertions (optional)
# install.packages("checkmate")
library(checkmate)

# Constructor for the 'Creature' class
Creature <- function(name, age) {
  # Validate inputs
  assertString(name)
  assertNumber(age, lower = 0)

  # Create and return the object
  structure(
    list(name = name, age = age),
    class = "Creature"
  )
}

# Example usage
creature <- Creature(name = "Griffin", age = 300)
print(creature)

# 2. Adding Generic Functions and Methods
# After creating the Creature constructor, you can define custom methods for generic functions like print or your own functions.


# Print method for 'Creature' objects
print.Creature <- function(x) {
  cat("Creature Information:\n")
  cat("  Name:", x$name, "\n")
  cat("  Age:", x$age, "\n")
}

# Test the print method
print(creature)


# Generic function
describe <- function(x) {
  UseMethod("describe")
}

# Method for 'Creature' class
describe.Creature <- function(x) {
  paste(x$name, "is", x$age, "years old.")
}

# Default method
describe.default <- function(x) {
  "I cannot describe this object."
}

# Test the describe function
describe(creature)            # Calls describe.Creature
describe("Some random input") # Calls describe.def



# Generic function
birthday <- function(x) {
  UseMethod("birthday")
}

# Method for 'Creature' class
birthday.Creature <- function(x) {
  x$age <- x$age + 1
  cat(x$name, "has turned", x$age, "years old!\n")
  return(x)
}

# Test the birthday function
creature <- birthday(creature)


# 4. Inheritance via Multiple Classes
# We can add multiple classes to an object to simulate inheritance.
# 
# Example: Add a Subclass Dragon



Dragon <- function(name, age, firepower) {
  assertString(name)
  assertNumber(age, lower = 0)
  assertNumber(firepower, lower = 0)
  
  structure(
    list(name = name, age = age, firepower = firepower),
    class = c("Dragon", "Creature") # Inherit from Creature
  )
}

# Define a method for the subclass 'Dragon'
describe.Dragon <- function(x) {
  paste(x$name, "is a", x$age, "year old dragon with firepower", x$firepower, ".")
}

# Create a Dragon object
dragon <- Dragon(name = "Smaug", age = 500, firepower = 100)

# Test the describe function
describe(dragon) # Calls describe.Dragon