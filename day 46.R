# Write a function that calculates the median and maximal arrival delay of the
# 3 most frequent carriers.
#
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# Output:
# `data.table` with columns `carrier`, `delay.median`, `delay.max`, indicating
# the minimum and maximum arrival delay experienced by flights of the three
# most represented carriers in the dataset. The median delay should be of the
# flights that were delayed at all, i.e. flights that were not delayed should
# not be counted. (If there were no delays in any flights of a carrier, this
# value should be zero). Only the lines for the three carriers with the most
# flights should be given (in any order). You can rely on there being at least
# three different carriers in the dataset.
# The result with the example dataset could be (up to row order):
flights.delays <- rbindlist(list(
  list(carrier = NULL, delay.median = NULL, delay.max = NULL),
  list("AA",           19,                  1524),
  list("DL",           15,                  1107),
  list("UA",           18,                  668)
))
# (There may be a datatype error when you use `median()` inside a `[ ]`
# aggregation; in that case, use `as.numeric(median())`.)
ex01DelayStats <- function(flights) {
  # your code
  assertDataTable(flights)
  
  
}
