# If you have looked at some of the data.table material then this function should
# be a breeze for you.


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
  lapply(lst, function(k) {
    assertList(k, names = "unique")
    lapply(k, assertNumber)
  })
  
  result <-  rbindlist(lst, use.names = TRUE, fill = TRUE)
  setnafill(result, fill = 0)[]
}



# This exercise, and the following exercises, concern themselves with data in a certain format.
#
# Setting: You are a data science consultant, hired to help the WidgetCorp (R) corporation
# analyse its production facilities for Widgets (TM). WidgetCorp (R) has a number of
# Widget (TM) production machines that are lovingly called Machine01, Machine02, Machine03, etc.
# Each of these machines has the same set of Sensors, measuring temperature, humidity,
# electricity consumption, water inflow rate, water purity, and various other metrics.
# Because the particulars of the quantities being measured are not relevant, these sensors
# are just enumerated as sensor01, sensor02, sensor03, etc.
# You are given a dataset that records, for each produced Widget (TM), (1) the Machine that
# produced it, (2) the quality of the Widget (TM), on a scale from 0 to 100, as assessed by
# certified Widget (TM) Quality Assessment Professionals, and (3) the values of the various
# sensor readings when the Widget (TM) was produced.
# However, the sensors have a limited measurement range that sometimes gets undercut. E.g.
# if the electricity consumption is below a certain threshold, the sensor is not able to
# record that value and just records NA.
# Example datasets could look like the following.
# (Note the `rbindlist`-layout is only for readability here. You can run the following code and
# will get a proper `data.table` from it.)
widget.corp.data <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
  list("Machine01",    78,             23,              28.6,            -23,             NA),
  list("Machine02",    28,             41,              77.8,            NA,              27),
  list("Machine03",    32,             57,              91.6,            -29,             10),
  list("Machine03",    80,             NA,              32.3,            NA,              NA),
  list("Machine03",    58,             10,              77.8,            3,               NA),
  list("Machine02",    74,             NA,              24.5,            -18,             3),
  list("Machine01",    46,             81,              NA,              NA,              NA),
  list("Machine01",    24,             43,              13.3,            -22,             NA),
  list("Machine02",    7,              96,              96.0,            0,               NA),
  list("Machine01",    22,             107,             23.5,            7,               8),
  list("Machine03",    98,             NA,              NA,              11,              NA)
))
# In the following tasks you should write functions that handle data just like this. However, you may get
# a dataset with more or fewer machines, and with more or fewer sensors.

# All your functions should `assertDataTable` the input value, but do not need to make any further assertions
# regarding the format of `data.table` arguments here.

# Hint:
# You will probably write some functions that modify the input data in-place. This means that calling
# one of your functions may actually cause the input value to be different after the call. Be aware of this
# if you try out your functions on the `widget.corp.data` example data. E.g. if you do
# > result <- ex04CleanTable(widget.corp.data)
# then `widget.corp.data` itself may have changed and could give different results in future experiments.
# You should therefore always run your functions on a `copy` of the input data, like so:
# > result <- ex04CleanTable(copy(widget.corp.data))
# Alternatively, you can just execute the above code-snippet (or source this .R-file) again after
# each experiment.

# Write a function that accepts one `data.table` argument `data` and sorts the given data according to
# the number of missing values in the `sensorXX` columns, in descending order. Ties should be broken
# by the `quality`, descending. Your function should return the `data.table` with rows sorted.
# The output for the example data would therefore be:
widget.corp.data.sorted <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
  list("Machine03",    98,             NA,              NA,              11,              NA),
  list("Machine03",    80,             NA,              32.3,            NA,              NA),
  list("Machine01",    46,             81,              NA,              NA,              NA),
  list("Machine01",    78,             23,              28.6,            -23,             NA),
  list("Machine02",    74,             NA,              24.5,            -18,             3),
  list("Machine03",    58,             10,              77.8,            3,               NA),
  list("Machine02",    28,             41,              77.8,            NA,              27),
  list("Machine01",    24,             43,              13.3,            -22,             NA),
  list("Machine02",    7,              96,              96.0,            0,               NA),
  list("Machine03",    32,             57,              91.6,            -29,             10),
  list("Machine01",    22,             107,             23.5,            7,               8)
))
ex01SortTable <- function(data) {
  assertDataTable(data)
  #rowSums(is.na(.SD)): Computes the number of missing values for each row across the selected columns.
 missings <-  data[,rowSums(is.na(.SD)), .SDcols = patterns("^sensor")]
 data[order(missings,quality,decreasing = TRUE)][]
}
ex01SortTable(copy(widget.corp.data))









widget.corp.data.sorted <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
  list("Machine03",    98,             NA,              NA,              11,              NA),
  list("Machine03",    80,             NA,              32.3,            NA,              NA),
  list("Machine01",    46,             81,              NA,              NA,              NA),
  list("Machine01",    78,             23,              28.6,            -23,             NA),
  list("Machine02",    74,             NA,              24.5,            -18,             3),
  list("Machine03",    58,             10,              77.8,            3,               NA),
  list("Machine02",    28,             41,              77.8,            NA,              27),
  list("Machine01",    24,             43,              13.3,            -22,             NA),
  list("Machine02",    7,              96,              96.0,            0,               NA),
  list("Machine03",    32,             57,              91.6,            -29,             10),
  list("Machine01",    22,             107,             23.5,            7,               8)
))
ex01SortTable <- function(data) {
assertDataTable(data)
  missings <- data[,rowSums(is.na(.SD)), .SDcols = patterns("^sensor")]
  data[order(missings,quality,decreasing = TRUE)]
}
ex01SortTable(copy(widget.corp.data))
data <-  copy(widget.corp.data[, new_col := 10])
missing <- data[,colSums(is.na(.SD)), .SDcols = patterns("^sensor")]
sensor_cols_sorted <- names(missing)[order(missing)]

data[, (sensor_cols_sorted) := .SD, .SDcols = sensor_cols_sorted[order(unname(missing))]]


data <- copy(widget.corp.data[, new_col := 10])

missing <- data[, colSums(is.na(.SD)), .SDcols = patterns("^sensor")]

sensor_cols_sorted <- names(missing)[order(missing)]

cols.i.want.to.keep <- grep("^sensor\\.", colnames(data), invert = TRUE, value = TRUE)
data[, c(cols.i.want.to.keep, sensor_cols_sorted), with = FALSE]






data <- copy(widget.corp.data)
data[, new_col := 10]

# Calculate the count of missing values for columns matching the pattern "^sensor"
missing <- data[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = patterns("^sensor")]

# Get sensor columns sorted by the count of missing values
sensor_cols_sorted <- names(missing)[order(unlist(missing))]

# Select columns you want to keep
cols_i_want_to_keep <- grep("^sensor\\.", colnames(data), invert = TRUE, value = TRUE)

# Subset the data with selected columns and sorted sensor columns
result <- data[, c(cols_i_want_to_keep, sensor_cols_sorted), with = FALSE]

# View the resulting dataset
result



# Create a copy of the dataset
data <- copy(widget.corp.data)

# Add a new column (example of additional non-sensor column manipulation)
data[, new_col := 10]

sensor_cols <- grep("^sensor", colnames(data), value = TRUE)

# Identify all non-sensor columns dynamically
non_sensor_cols <- setdiff(colnames(data), sensor_cols)

# Get the positions of the non-sensor columns in the original dataset
non_sensor_positions <- match(non_sensor_cols, colnames(data))
# Identify the first two columns and the last column by position
non_sensor_positions <- non_sensor_positions
non_sensor_cols <- colnames(data)[non_sensor_positions]

# Calculate the count of missing values for columns matching the pattern "^sensor"
missing <- data[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = patterns("^sensor")]

# Get sensor columns sorted by the count of missing values
sensor_cols_sorted <- names(missing)[order(unlist(missing))]

# Create the final ordered column list
final_columns <- colnames(data)  # Start with the original column order
final_columns[non_sensor_positions] <- non_sensor_cols  # Plug back the first 2 and last columns at their original positions
final_columns[-non_sensor_positions] <- sensor_cols_sorted  # Fill the remaining positions with sorted sensor columns

# Subset the data using the final column order
result <- data[, ..final_columns]

# View the resulting dataset
result