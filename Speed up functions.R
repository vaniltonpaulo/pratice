### THIS DOCUMENT SERVES TO SHOW YOU HOW TO BEST SPEED UP FUNCTIONS


########Aufgabe 1


# In this exercise, you will be trying to make functions faster.
#
# Although performance is almost always decidedly less important than
# correctness and legibility, it is still helpful to know how fast/slow
# operations are in R.
#
# In the following exercises, you are given a reference-solution for each
# exercise. Your task is to write a function that behaves the same but runs
# in less than 1/2 the time for the specified input (median speedup,
# as per `microbenchmark()`).
#
# Feel free to go for larger speed-ups: Many of the functions can easily be
# sped up much more than by a factor of 2, and experimenting with different
# approaches will give you valuable experience.
#
# Note: Because the reference functions were written to be deliberately slow,
# they should not serve as inspiration for your own code in the future!

# Find the largest and smallest values, i.e. "outliers", in a vector
#
# Suppose You have taken a few noisy measurements, but you are worried about
# outlier values. You therefore want to extract a few of the largest, and of
# the smallest measurement values you have taken, so you can analyse them
# further.
#
# Write a function that keeps the `n.keep` largest, and the `n.keep`
# smallest values from a vector `x`, returning a vector of length
# `2 * n.keep` (or the original vector `x` if `n.keep` is larger than half the
# length of `x`).
#
# Input arguments:
# - `x`: `numeric` vector of values from which to extract outliers. You can
#   assume that `x` does not have duplicate values.
# - `n.keep`: non-negative integer `numeric(1)` indicating the number of
#   smallest, as well as the number of largest, outliers to keep.
# Return value: `numeric` vector, where the top `n.keep` and the bottom `n.keep`
# values of `x` are kept, with their order preserved. This vector should have
# length `2 * n.keep` (or `length(x)` if `n.keep` is larger than half the length
# of `x`).
# Performance goal: Your function should have a median runtime of 1/2 as much
# as `ex01KeepOutliersReference` when called with a vector `x` of length
# 1000 and n.keep between 25 and 35.
# Remarks:
# - You can use `microbenchmark` to assess runtime of individual components of
#   the function.
# - You might want to handle edge cases (no outliers, everything is an outlier)
#   first to save time.

# Warm-up for components you might find useful:
# - Make yourself familiar with `microbenchmark` by, e.g., comparing the
#   runtimes of `log` vs `log10` on values 1, 2, ..., 1000000.
# - Use `microbenchmark` to compare the runtimes on `x = 1:1000000` of the
#   following functions:
#   - fun.1(x) <- function(x) {
#       result <- numeric(length(x))
#       for (i in x) result[i] <- x[i] %/% 7
#       result
#     }
#   - fun.2(x) <- function(x) x %/% 7

ex01KeepOutliersReference <- function(x, n.keep) {
  assertCount(n.keep, tol = 1e-100)
  assertNumeric(x, any.missing = FALSE)
  x.discard <- x
  # create a vector of everything that we want to discard
  for (i in seq_len(n.keep)) {
    x.discard <- x.discard[-which.max(x.discard)]
    x.discard <- x.discard[-which.min(x.discard)]
  }
  # get the complement of elements that are discarded. This works because
  # there are no duplicates in x
  setdiff(x, x.discard)
}

ex01KeepOutliers <- function(x, n.keep) {
  assertCount(n.keep, tol = 1e-100)
  assertNumeric(x, any.missing = FALSE)
  # your code

  
}


#profvis::profvis(replicate(1000, ex01KeepOutliersReference(seq_len(100),2) ))