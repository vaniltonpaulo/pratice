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

# Write a function that imputes all missing values for sensor columns as the minimum of that column.
# The function takes one `data.table` argument `data` and should return the modified `data.table`.
# You can rely on at least one non-missing value being present in each sensor-column. The result
# for the `widget.corp.data` example data would be
widget.corp.data.imputed <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
  list("Machine01",    78,             23,              28.6,            -23,             3),
  list("Machine02",    28,             41,              77.8,            -29,             27),
  list("Machine03",    32,             57,              91.6,            -29,             10),
  list("Machine03",    80,             10,              32.3,            -29,             3),
  list("Machine03",    58,             10,              77.8,            3,               3),
  list("Machine02",    74,             10,              24.5,            -18,             3),
  list("Machine01",    46,             81,              13.3,            -29,             3),
  list("Machine01",    24,             43,              13.3,            -22,             3),
  list("Machine02",    7,              96,              96.0,            0,               3),
  list("Machine01",    22,             107,             23.5,            7,               8),
  list("Machine03",    98,             10,              13.3,            11,              3)
))
ex01ImputeTable <- function(data) {
  
}

data <-  copy(widget.corp.data)
assertDataTable(data)

sensorcols <- grep("sensor[0-9]+", colnames(data), value = TRUE)
data[, (sensorcols) := lapply(.SD, function(k) {
  nafill(k,fill = min(k, na.rm = TRUE))
}), .SDcols = sensorcols][]