## tHIS IS THE REDO OF THE FIRST HW WITH THE SOLUTUIONS


# Write a function that takes a list of named lists as input and creates a `data.table` as
# output, containing the input lists as rows.
# Inputs:
# - lst: `list` of named `lists` containing numeric scalars.
# Output: `data.table`
# The resulting `data.table` should contain the input rows in order and have columns according
# to the names of the input list (ordering of columns does not matter).
#
# Example:
# ex01List2DT(list(list(a = 1, b = 2), list(b = 3, a = 4)))
# --> data.table(a = c(1, 4), b = c(2, 3))
# Some lists do not contain elements for all columns; in that case, a 0 should be used:
# ex01List2DT(list(list(a = 1, b = 2), list(a = 4, c = 5)))
# --> data.table(a = c(1, 4), b = c(2, 0), c = c(0, 5))
# if there are elements in the lists that are not non-NA scalar numerics, an error should be thrown.
#
# You should probably use `rbindlist` and possibly `nafill` / `setnafill` here.
ex01List2DT <- function(lst) {
  assertList(lst)
  
  #ensure that each element of the main list is a list
  lapply(lst,function(k) {
    assertList(k,
               #asserts that the `names` inside the list  are unique
      names = "unique")
    #assert that each element inside the sub-list are scalar numeric
    lapply(k,assertNumber)
  })
  
  result <- rbindlist(lst,
                      #check use.names great explanation
                      use.names = TRUE,
                      #fill empty rows with Na
                      #Like in this case where we have multiple columns and some values will be empty
                      ## ex01List2DT(list(list(a = 1, b = 2), list(a = 4, c = 5)))
                      fill = TRUE)
  setnafill(result,fill = 0)[]
}
