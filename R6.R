# R6 classes consist of:
#   
#   Public Members: Attributes and methods accessible from outside the class.
# Private Members: Attributes and methods accessible only within the class.
# Active Bindings: Properties that look like fields but are computed on access.
# Inheritance: Mechanism to derive a new class from an existing one.
# Initialization: Constructor-like mechanism to set up an object when it is created.
#invisible(self)


########################## FULL EXAMPLE

# 3. Initializing and Defining a Basic R6 Class
# Here’s how to define and instantiate a basic R6 class:
  
# Load the R6 package
library(R6)

# Define a simple R6 class
Person <- R6Class("Person",
                  public = list(
                    name = NULL,
                    age = NULL,
                    
                    # Constructor
                    initialize = function(name = NULL, age = NULL) {
                      self$name <- name
                      self$age <- age
                    },
                    
                    # Method to display details
                    introduce = function() {
                      paste("Hi, my name is", self$name, "and I am", self$age, "years old.")
                    }
                  )
)

# Instantiate the class
john <- Person$new(name = "John", age = 30)

# Access public members
print(john$name)         # "John"
print(john$age)          # 30

# Call a method
print(john$introduce())  # "Hi, my name is John and I am 30 years old."`



# 4. Public and Private Members
# Public members are accessible from outside the object.
# Private members are hidden and can only be accessed internally.
# Example:
  
 
# Class with private members
BankAccount <- R6Class("BankAccount",
                       public = list(
                         account_holder = NULL,
                         initialize = function(name, balance) {
                           private$balance <- balance
                           self$account_holder <- name
                         },
                         get_balance = function() {
                           paste("Account holder:", self$account_holder, "Balance:", private$balance)
                         }
                       ),
                       private = list(
                         balance = NULL  # Private field
                       )
)

# Create an object
account <- BankAccount$new("Alice", 1000)

# Access public members
print(account$account_holder)  # "Alice"
print(account$get_balance())   # "Account holder: Alice Balance: 1000"

# Accessing private members directly will fail
# print(account$balance)  # Error




# 5. Using Active Bindings
# Active bindings look like fields but execute a function when accessed or modified.

#Example:
# Class with active bindings
Rectangle <- R6Class("Rectangle",
                     public = list(
                       width = NULL,
                       height = NULL,
                       initialize = function(width, height) {
                         self$width <- width
                         self$height <- height
                       }
                     ),
                     active = list(
                       area = function() {
                         self$width * self$height
                       }
                     )
)

# Instantiate and use the class
rect <- Rectangle$new(5, 3)
print(rect$area)  # 15 (computed as width * height)





# 
# 6. Inheritance
# R6 supports inheritance, allowing one class to extend another.
# 
# Example:
#   
#   R
# Copy code
# Define a base class
Animal <- R6Class("Animal",
                  public = list(
                    species = NULL,
                    initialize = function(species) {
                      self$species <- species
                    },
                    speak = function() {
                      paste("I am a", self$species)
                    }
                  )
)

# Define a subclass
Dog <- R6Class("Dog",
               inherit = Animal,
               public = list(
                 bark = function() {
                   "Woof! Woof!"
                 }
               )
)

# Create an object of the subclass
dog <- Dog$new("Dog")
print(dog$speak())  # "I am a Dog"
print(dog$bark())   # "Woof! Woof!"



# 
# 7. Polymorphism
# Polymorphism allows a subclass to override a method from the parent class.
# 
# Example:
#   
#   R
# Copy code
# Subclass overrides a parent method
Cat <- R6Class("Cat",
               inherit = Animal,
               public = list(
                 speak = function() {
                   "Meow!"
                 }
               )
)

# Create a Cat object
cat <- Cat$new("Cat")
print(cat$speak())  # "Meow!"



# 
# 9. Advanced Features
# Cloning
# By default, R6 objects are references, but you can create a deep copy using clone().
# 
# R
# Copy code
 clone <- john$clone(deep = TRUE)
 
 
 
 
 
#  10. invisible(self)
#  What is invisible(self)?
#    In R6, invisible(self) is often used at the end of methods that modify the objects state.
# It ensures that the method does not print or return the object unless explicitly requested. This is particularly useful for "setter" methods where the goal is to change the state of the object, not to return anything.
# Why use it?
# Without invisible(self), methods would return the entire object by default, cluttering your output.
# Using invisible(self) allows method chaining, as it makes the modified object available without explicitly returning it.
# Example: Using invisible(self)
# R
# Copy code
Person <- R6Class("Person",
  public = list(
    name = NULL,
    age = NULL,
    
    # Constructor
    initialize = function(name = NULL, age = NULL) {
      self$name <- name
      self$age <- age
      invisible(self)  # Allows chaining during object creation
    },
    
    # Method to set the name
    set_name = function(new_name) {
      self$name <- new_name
      invisible(self)  # Allows method chaining
    },
    
    # Method to set the age
    set_age = function(new_age) {
      self$age <- new_age
      invisible(self)  # Allows method chaining
    },
    
    # Method to display details
    introduce = function() {
      paste("Hi, my name is", self$name, "and I am", self$age, "years old.")
    }
  )
)

# Example usage
john <- Person$new("John", 30)
print(john$introduce())  # "Hi, my name is John and I am 30 years old."

# Method chaining
john$set_name("Jonathan")$set_age(35)
print(john$introduce())  # "Hi, my name is Jonathan and I am 35 years old."




########################## FULL EXAMPLE


# Example: A Banking System
# This example models a simple banking system with Account as the base class and SavingsAccount as a subclass.
# 
# R
# Copy code
library(R6)

# Base Class: Account
Account <- R6Class("Account",
                   public = list(
                     account_holder = NULL,  # Public field
                     account_number = NULL,  # Public field
                     
                     # Constructor
                     initialize = function(account_holder, account_number) {
                       self$account_holder <- account_holder
                       self$account_number <- account_number
                       private$balance <- 0  # Initialize balance to zero
                       invisible(self)
                     },
                     
                     # Public Method: Deposit money
                     deposit = function(amount) {
                       if (!is.numeric(amount) || amount <= 0) {
                         stop("Deposit amount must be a positive number.")
                       }
                       private$balance <- private$balance + amount
                       message("Deposited $", amount, ". New balance: $", private$balance)
                       invisible(self)
                     },
                     
                     # Public Method: Withdraw money
                     withdraw = function(amount) {
                       if (!is.numeric(amount) || amount <= 0) {
                         stop("Withdrawal amount must be a positive number.")
                       }
                       if (amount > private$balance) {
                         stop("Insufficient funds.")
                       }
                       private$balance <- private$balance - amount
                       message("Withdrew $", amount, ". New balance: $", private$balance)
                       invisible(self)
                     },
                     
                     # Public Method: Display account details
                     display_account = function() {
                       paste0("Account Holder: ", self$account_holder, 
                              "\nAccount Number: ", self$account_number,
                              "\nBalance: $", private$balance)
                     }
                   ),
                   
                   private = list(
                     balance = NULL  # Private field: Balance
                   ),
                   
                   active = list(
                     # Active Binding: Retrieve current balance
                     balance = function() {
                       private$balance
                     }
                   )
)

# Subclass: SavingsAccount
SavingsAccount <- R6Class("SavingsAccount",
                          inherit = Account,  # Inherits from Account class
                          
                          public = list(
                            interest_rate = NULL,  # Public field for interest rate
                            
                            # Constructor
                            initialize = function(account_holder, account_number, interest_rate) {
                              super$initialize(account_holder, account_number)  # Call parent constructor
                              self$interest_rate <- interest_rate
                              invisible(self)
                            },
                            
                            # Public Method: Add interest
                            add_interest = function() {
                              if (is.null(self$interest_rate) || self$interest_rate <= 0) {
                                stop("Invalid interest rate.")
                              }
                              interest = private$balance * self$interest_rate / 100
                              private$balance <- private$balance + interest
                              message("Interest added: $", interest, ". New balance: $", private$balance)
                              invisible(self)
                            }
                          )
)

# Usage Example
# Create a Savings Account
savings <- SavingsAccount$new(
  account_holder = "John Doe",
  account_number = "123456789",
  interest_rate = 5  # 5% interest rate
)

# Deposit money
savings$deposit(1000)

# Check balance
print(savings$balance)  # Active binding to get balance

# Add interest
savings$add_interest()

# Withdraw money
savings$withdraw(500)

# Display account details
cat(savings$display_account())