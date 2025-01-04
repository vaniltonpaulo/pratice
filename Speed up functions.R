### THIS DOCUMENT SERVES TO SHOW YOU HOW TO BEST SPEED UP FUNCTIONS

#to analyze what makes it slow
#this is much better
profvis::profvis(for (i in 1:1000) ex01SimpleEcology(10, 2, 50, 1000))

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

  #option 1 (4.45 faster)
  x.rank <- rank(x)
  # this is looking at the position ZB: lowest 25 or highest 25
  x[x.rank <= n.keep| x.rank > length(x) - n.keep]
  
  
  #option 2(7.75 faster)
  if(n.keep == 0) return(numeric(0))
  if (2 * n.keep >= length(x)) return(x)
  
  x.sorted <- sort(x)
  cutoff.lower <- x[[n.keep]]
  cutoff.upper <- x[[length(x) - n.keep]]
  x[x <= cutoff.lower| x > cutoff.upper]
}


#profvis::profvis(replicate(1000, ex01KeepOutliersReference(seq_len(100),2) ))











########Aufgabe 2


# Simple Ecology
#
# The objective here is again to rewrite the following functions to be faster
# than the provided reference versions.
#
# Consider a very simple ecological model that predicts how many individuals
# of a given species are alive in time `t`. The system starts out with
# `x(1)` individuals at time 1. In every discrete time step, the population
# changes because of two effects:
# - Reproduction with reproduction rate `qr`: Add `(qr - 1) * x(t)` individuals
#   after every time step `t`.
# - Starvation with carrying capacity `G`: Remove a proportion of `x(t) / G` of
#   individuals after time step `t`, i.e., an absolute number of
#   `x(t) * x(t) / G` die in every step.

# With the two effects, the quantity of living individuals at time step `t + 1`
# is given by
#   x(t + 1) = (1 / G) * x(t) * (qr * G - x(t)).
# Notice that `x` is a quantity that changes over time `t`, whereas `G` and
# `qr` are model parameters.
#
#   Derivation, in case you are interested (if not, skip to the task):
#      x(t + 1) = x(t) + (qr - 1) * x(t)   - x(t) * x(t) / G
#               = qr * x(t)                - x(t) * x(t) / G
#               = (1 / G) * x(t) * qr * G  - (1 / G) * x(t) * x(t)
#               = (1 / G) * x(t) * (qr * G - x(t))
#      This is the "logistic map" and plays a role in chaos theory:
#      <https://en.wikipedia.org/wiki/Logistic_map>. To get from our formula
#      to the one in Wikipedia, set `qr` to `r` and `G` to `1 / r`.)

# Task: Calculate the sequence of living individuals.
#
# Write a function that calculates the sequence of quantities `x(t)` for `t`
# between 1 and `t.max` (inclusive).
#
# Input arguments:
# - `x1`: `numeric(1)` indicating `x(1)`, at least 0
# - `qr`: `numeric(1)` the reproduction rate, at least 1.
# - `g`: `numeric(1)` the carrying capacity, at least 0.
# - `t.max`: positive integer `numeric(1)` indicating the length of the result.
# Return value: `numeric` vector giving the `x(t)` for `t` in 1 ... `t.max`
# (inclusive).
# Performance goal: Your function should have a median runtime 1/2 as much as
# `ex01SimpleEcologyReference` for values of `t.max` of 1000, `qr` between 1
# and 4, `g` between 0 and 100, and x1 between 0 and `qr * g`.
# Remarks:
# - You can use `microbenchmark` to assess runtime of individual components of
#   the function.
# - You can use `profvis` to see which part of a function take the most time
#   and therefore profit most from optimization. For this, you will need to
#   call your function in a loop, since otherwise the function returns too
#   quickly for `profvis` to measure anything:
#   #> profvis::profvis({for (i in 1:1000) ex01SimpleEcologyReference(10, 2, 50, 1000)})
#   !! you need to "source" this .R-file so that `profvis` can see it: Use the
#   'Source' button in RStudio. Ctrl/Cmd-Enter will not work !!
# - Work with continuous quantities (no rounding).
# - You might want to ensure that recurring quantities are only computed once.
# - Your function can achieve the necessary speedup while still containing a
#   `for` loop. Not everything in life can be vectorized!

ex01SimpleEcologyReference <- function(x1, qr, g, t.max) {
  assertNumber(x1, lower = 0)
  assertNumber(qr, lower = 1)
  assertNumber(g, lower = 0)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- x1
  xt <- x1
  for (t in seq_len(t.max - 1)) {
    xt.next <- 1 / g * xt * (qr * g - xt)
    result <- append(result, xt.next)
    xt <- xt.next
  }
  result
}

ex01SimpleEcology <- function(x1, qr, g, t.max) {
  assertNumber(x1, lower = 0)
  assertNumber(qr, lower = 1)
  assertNumber(g, lower = 0)
  assertInt(t.max, lower = 1, tol = 1e-100)
  #Option 1(7.73 faster)
  result <- x1
  
  xt <- x1
  for (t in seq_len(t.max - 1)) {
    #you could also pre compute 1/g and create a variable outside of the loop for a +4 improvement
    
    xt.next <- 1 / g * xt * (qr * g - xt)
    #The append was the problem
    result[[length(result) + 1]] <-  xt.next
    xt <- xt.next
  }
  result
  
  
  
  #Option 2(faster 25.36)
  result <- numeric(t.max)
  result[[1]] <- x1
  
  xt <- x1
  for (t in seq_len(t.max - 1)) {
    #you could also pre compute 1/g and create a variable outside of the loop for a +4 improvement
    xt.next <- 1 / g * xt * (qr * g - xt)
    #The append was the problem
    result[[t+ 1]] <-  xt.next
    xt <- xt.next
  }
  result
  
}

#to analyze what makes it slow
#this is much better
profvis::profvis(for (i in 1:1000) ex01SimpleEcology(10, 2, 50, 1000))







########Aufgabe 3





# Complex Ecology
#
# The objective here is again to rewrite the following functions to be faster
# than the provided reference versions.
#
# A more complex ecological system, made up of two species, is described by the
# "Lotka-Volterra" equations. It is made up of the "predator" and the "prey"
# species, where the quantity of prey grows by a constant factor and gets
# reduced proportionally to how many predators are present, and the quantity of
# predators grows proportionally to how many prey are present and gets reduced
# by a constant factor.
# The actual Lotka-Volterra equations are differential equations, but here we
# will use a discretization of the model:
#   - prey(t + 1)     = prey(t) * qr.prey         - prey(t) * predator(t) * qi.prey
#   - predator(t + 1) = predator(t) * qr.predator + predator(t) * prey(t) * qi.predator
#   - When any of the equations above has a negative result, then the values
#     are set to 0. This ensures that `prey` or `predator` never become negative.
# We apply the constraints `qr.prey` >= 1 and 0 <= `qr.predator` <= 1 to avoid
# either of the following scenarios: With no predators present, the prey would
# grow exponentially, and with no prey present, the predators would die out.

# Task: Calculate the sequence of quantities `prey(t)` and `predator(t)`.
#
# Input arguments:
# - `x1`: `numeric(2)` indicating `prey(1)` in component 1 and `predator(1)`
#   in component 2, both >= 0.
# - `qr`: `numeric(2)` indicating the natural growth rates in the absence of
#   the other species: `qr[1]` is `qr.prey` (at least 1) and `qr[2]` is
#   `qr.predator` (between 0 and 1, inclusive).
# - `qi`: `numeric(2)` indicating the effect of growth rates of the species on
#   each other. `qi[1]` is `qi.prey` and `qi[2]` is `qi.predator`, both >= 0.
# - `t.max`: positive integer `numeric(1)` indicating the number of entries in
#   the result.
# Return value: a `matrix` with two columns and `max.t` rows, where columns are
# named `prey` and `predator`, containing the quantities of both at each time
# between 1 and `max.t` (inclusive).
# Performance goal: Your function should have a median runtime 1/2 as much as
# `ex01ComplexEcologyReference` for values of `t.max` of 100 (less than in
# 'Simple Ecology', so make sure to adjust the variable), `qr[1]` between 1 and
# 1.2, `qr[2]` between 0.8 and 1, and `qi` between 0 and 0.2.
#
# The remarks from "simple ecology" are also relevant here.

ex01ComplexEcologyReference <- function(x1, qr, qi, t.max) {
  assertNumeric(x1, len = 2, lower = 0, any.missing = FALSE)
  assertNumeric(qr, len = 2, any.missing = FALSE)
  if (qr[[1]] < 1 || qr[[2]] < 0 || qr[[2]] > 1) stop("qr[1] must be at least 1 and qr[2] must be between 0 and 1.")
  assertNumeric(qi, len = 2, lower = 0, upper = 1, any.missing = FALSE)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("prey", "predator")))
  xt <- x1
  for (t in seq_len(t.max)) {
    result <- rbind(result, xt, deparse.level = 0)
    xt <- xt * qr + c(-1, 1) * xt[[1]] * xt[[2]] * qi
    xt <- pmax(xt, 0)  # set values that are < 0 to 0
  }
  result
}

ex01ComplexEcology <- function(x1, qr, qi, t.max) {
  #option 1 (2.25 faster)
  assertNumeric(x1, len = 2, lower = 0, any.missing = FALSE)
  assertNumeric(qr, len = 2, any.missing = FALSE)
  if (qr[[1]] < 1 || qr[[2]] < 0 || qr[[2]] > 1) stop("qr[1] must be at least 1 and qr[2] must be between 0 and 1.")
  assertNumeric(qi, len = 2, lower = 0, upper = 1, any.missing = FALSE)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("prey", "predator")))
  xt <- x1
  for (t in seq_len(t.max)) {
    result <- rbind(result, xt, deparse.level = 0)
    xt <- xt * qr + c(-1, 1) * xt[[1]] * xt[[2]] * qi
    #pmax was too slow
    # set values that are < 0 to 0
     xt[xt < 0] <- 0  
  }
  result
  
  
  

  
  #option 2 (6.29 faster)
  assertNumeric(x1, len = 2, lower = 0, any.missing = FALSE)
  assertNumeric(qr, len = 2, any.missing = FALSE)
  if (qr[[1]] < 1 || qr[[2]] < 0 || qr[[2]] > 1) stop("qr[1] must be at least 1 and qr[2] must be between 0 and 1.")
  assertNumeric(qi, len = 2, lower = 0, upper = 1, any.missing = FALSE)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- matrix(0,nrow = t.max ,ncol = 2, dimnames = list(NULL, c("prey", "predator")))
  xt <- x1
  for (t in seq_len(t.max)) {
    result[t, ] <-  xt
    xt <- xt * qr + c(-1, 1) * xt[[1]] * xt[[2]] * qi
    #pmax was too slow
    # set values that are < 0 to 0
    xt[xt < 0] <- 0  
  }
  result
}



