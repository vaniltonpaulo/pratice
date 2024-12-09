#AssertSubset vs AssertChoice

#AssertSubset:
  
#   This function is used to check if all elements of one vector (x) are contained in another vector (choices).
# It is appropriate for ensuring that a set of values is within a defined allowable set.
#Use this when validating multiple values or ensuring that subsets adhere to the permissible set.

# Example:


assertSubset(c("apple", "banana"), c("apple", "banana", "cherry"))
# Passes since both "apple" and "banana" are in the set of choices

#AssertChoice:
  
#   This function is used to check if a single value is part of a predefined set of allowed values (choices).
# It is more specific than assertSubset, designed for scalar values.
# Example:
assertChoice("apple", c("apple", "banana", "cherry"))
# Passes since "apple" is in the set of choices

#When to Use Each:

## - Use assertSubset when the input could have multiple elements, and you want to ensure all of them are valid options.

## - Use assertChoice when validating a single input value.



# Logical Assertions
# 
# assert_flag(x): Asserts that x is a logical scalar (TRUE or FALSE).
# assert_logical(x, len = 1): Asserts that x is a logical scalar of length 1 (TRUE, FALSE, or NA).


# Numeric Assertions
# 
# assert_number(x): Asserts that x is a numeric scalar (finite and non-NA).
# assert_int(x): Asserts that x is an integer scalar (whole number).
# assert_integerish(x, len = 1): Asserts that x can be coerced to an integer scalar.
# assert_numeric(x, len = 1): Asserts that x is a numeric scalar (supports NA and non-finite values if specified).
# assert_count(x): Asserts that x is a non-negative integer scalar (e.g., for counts).
# assert_posixct(x, len = 1): Asserts that x is a POSIXct date-time scalar.



# Character Assertions
# 
# assert_string(x): Asserts that x is a non-empty character scalar (string).
# assert_character(x, len = 1): Asserts that x is a character scalar (string) of length 1.
# assert_scalar(x): A generic assertion that x has length 1.



# Value-specific Assertions
# 
# assert_true(x): Asserts that x is TRUE.
# assert_false(x): Asserts that x is FALSE.

# ESSENTIALLY AS INPUT, WE ARE ONLY GOING TO ACCEPT(use this for for words, not in matrix rather)
# assert_choice(x, choices): Asserts that x is a scalar and one of the provided choices.

# assert_scalar_na(x): Asserts that x is a scalar NA.
# assert_scalar_non_na(x): Asserts that x is a non-NA scalar.



# Range and Bound Constraints
# 
# assert_number(x, lower = , upper = ): Asserts that x is a numeric scalar within specific bounds.
# assert_int(x, lower = , upper = ): Asserts that x is an integer scalar within specific bounds.





##############################################################################


# Assertions for Matrices
# Basic Matrix Assertions
# 
# assert_matrix(x): Asserts that x is a matrix.
# assert_matrix(x, mode = "numeric"): Asserts that x is a matrix with numeric values.
# assert_matrix(x, nrows = , ncols = ): Ensures x has a specific number of rows or columns.
# assert_matrix(x, min.rows = , max.rows = ): Ensures x has a minimum or maximum number of rows.
# assert_matrix(x, min.cols = , max.cols = ): Ensures x has a minimum or maximum number of columns.
# assert_matrix(x, any.missing = FALSE): Asserts that there are no missing values in the matrix.



# Useful Assertions Within a Matrix Context
# 
# assert_numeric(x): Check if elements in a numeric matrix are of the correct type.
# assert_int(x): Ensure that all values in an integer matrix are integers.
# assert_subset(colnames(x), choices = c("col1", "col2")): Asserts specific column names are present in the matrix if it’s labeled.



##############################################################################


# Assertions for Data Frames and Data Tables
# Data Frame Assertions
# 
# assert_data_frame(x): Asserts x is a data frame.
# assert_data_frame(x, nrows = , ncols = ): Ensures specific row or column count.
# assert_data_frame(x, min.rows = , max.rows = ): Ensures a minimum or maximum number of rows.
# assert_data_frame(x, col.names = "unique"): Ensures unique column names.
# assert_data_frame(x, any.missing = FALSE): Ensures no missing values in the data frame.


# Data Table Assertions
# 
# assert_data_table(x): Asserts x is a data.table.
# assert_data_table(x, nrows = , ncols = ): Ensures specific row or column count.
# assert_data_table(x, col.names = "strict"): Ensures that there are no duplicate or missing column names.



# Useful Assertions Within a Data Frame or Data Table Context
# 
# assert_names(names(x), type = "unique"): Checks column names are unique.
# assert_subset(names(x), choices = c("column1", "column2")): Ensures specific column names exist.
# assert_logical(x$flag_column): Ensures a specific column is logical (TRUE/FALSE).
# assert_numeric(x$numeric_column): Ensures a column is numeric.






##############################################################################


# Assertions for Lists
# Basic List Assertions
# 
# assert_list(x): Asserts x is a list.
# assert_list(x, len = ): Ensures the list has a specific length.
# assert_list(x, any.missing = FALSE): Ensures no missing elements in the list.
# assert_list(x, types = c("numeric", "character")): Asserts that elements in the list are of specific types.



# Useful Assertions Within a List Context
# 
# assert_names(names(x), type = "unique"): Ensures all elements in a named list have unique names.
# assert_subset(names(x), choices = c("item1", "item2")): Ensures specific named elements exist.
# assert_numeric(x$element_name): Ensures a specific list element is numeric.





##############################################################################


# Assertions for Names
# Assertions for Named Vectors, Lists, or Data Frames
# 
# assert_names(x, type = "unique"): Ensures all names are unique.
# assert_names(x, type = "strict"): Ensures no duplicate or missing names.
# assert_names(x, subset.of = c("name1", "name2")): Ensures names in x are a subset of specific names.
# assert_names(x, must.include = c("name1", "name2")): Ensures certain names must be included in x.